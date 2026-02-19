`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

#' Create a prediction raster grid for a country
#'
#' Generates an empty raster covering the bounding box of the country panel
#' at the configured resolution.
#'
#' @param panel_sf sf object with country geometries.
#' @param resolution_m Grid cell size in metres.
#' @param canonical_crs CRS string.
#' @return A terra SpatRaster template (values are NA).
create_prediction_grid <- function(panel_sf, resolution_m = 1000,
                                   canonical_crs = "EPSG:3035") {
  panel_proj <- sf::st_transform(panel_sf, canonical_crs)

  # Filter out empty geometries
  non_empty <- !sf::st_is_empty(panel_proj)
  if (any(non_empty)) {
    bbox <- sf::st_bbox(panel_proj[non_empty, ])
  } else {
    bbox <- c(xmin = -10000, ymin = -10000, xmax = 10000, ymax = 10000)
    names(bbox) <- c("xmin", "ymin", "xmax", "ymax")
  }

  if (any(is.na(bbox))) {
    bbox <- c(xmin = -10000, ymin = -10000, xmax = 10000, ymax = 10000)
    names(bbox) <- c("xmin", "ymin", "xmax", "ymax")
  }

  # Small buffer so edge polygons are fully covered
  buf <- resolution_m * 2
  ext <- terra::ext(
    bbox["xmin"] - buf, bbox["xmax"] + buf,
    bbox["ymin"] - buf, bbox["ymax"] + buf
  )

  ncol <- max(5L, as.integer(ceiling((ext[2] - ext[1]) / resolution_m)))
  nrow <- max(5L, as.integer(ceiling((ext[4] - ext[3]) / resolution_m)))

  terra::rast(ext, nrows = nrow, ncols = ncol, crs = canonical_crs)
}

#' Build feature stack for prediction
#'
#' Loads each raster feature and stacks them into a single multi-layer
#' SpatRaster aligned to the prediction grid. Constant layers for
#' \code{log_area} (log1p of cell area in m²) and \code{year} are appended so
#' that the stack matches the full feature set used during model training.
#'
#' @param grid_template SpatRaster template from create_prediction_grid.
#' @param feature_registry List of enabled feature entries.
#' @param canonical_crs CRS string.
#' @param root_dir Project root.
#' @param resolution_m Cell size in metres; used to compute constant log_area.
#' @param prediction_year Integer prediction year; used to set the matching
#'   \code{year_XXXX} dummy layer to 1 (all others 0).
#' @param feature_names Character vector of feature names from the trained
#'   model; used to determine which \code{year_XXXX} dummy layers to create.
#' @return A multi-layer SpatRaster with one layer per feature.
build_feature_stack <- function(grid_template, feature_registry,
                                canonical_crs = "EPSG:3035",
                                root_dir = ".",
                                resolution_m = 1000, prediction_year = NULL,
                                feature_names = NULL) {
  layers <- list()

  for (feature_entry in feature_registry) {
    source_config <- feature_entry$source_config
    source_cfg <- load_source_config(source_config, root_dir = root_dir)

    r <- load_raster(
      source_cfg = source_cfg,
      feature_entry = feature_entry,
      canonical_crs = canonical_crs,
      root_dir = root_dir
    )

    # Resample to prediction grid
    r_aligned <- terra::resample(r, grid_template, method = "bilinear")
    names(r_aligned) <- feature_entry$id
    layers[[length(layers) + 1]] <- r_aligned
  }

  if (length(layers) == 0) {
    stop("No feature layers available for prediction", call. = FALSE)
  }

  # --- Constant derived-feature layers ---
  # log_area: log1p of cell area in m² (EPSG:3035 is equal-area, so constant)
  log_area_lyr <- grid_template
  terra::values(log_area_lyr) <- log1p(resolution_m^2)
  names(log_area_lyr) <- "log_area"
  layers[["log_area"]] <- log_area_lyr

  # year dummy layers: one binary layer per decade that the model knows about.
  # The layer for `prediction_year` is set to 1; all others are 0.
  # Layer names are taken directly from feature_names (e.g. "year_1910") so
  # the raster stack matches the training feature matrix exactly.
  if (!is.null(feature_names)) {
    year_dummy_feats <- grep("^year_\\d+$", feature_names, value = TRUE)
    for (yn in year_dummy_feats) {
      yr_val <- as.integer(sub("^year_", "", yn))
      lyr    <- grid_template
      terra::values(lyr) <- if (!is.null(prediction_year) &&
                                  yr_val == as.integer(prediction_year)) 1L else 0L
      names(lyr)       <- yn
      layers[[yn]]     <- lyr
    }
  }

  # lon / lat: geographic coordinates for every grid cell
  ncells    <- terra::ncell(grid_template)
  xy_proj   <- terra::xyFromCell(grid_template, seq_len(ncells))
  xy_wgs84  <- sf::sf_project(
    from = terra::crs(grid_template),
    to   = "EPSG:4326",
    pts  = xy_proj
  )
  lon_lyr <- grid_template
  terra::values(lon_lyr) <- xy_wgs84[, 1L]
  names(lon_lyr) <- "lon"
  layers[["lon"]] <- lon_lyr

  lat_lyr <- grid_template
  terra::values(lat_lyr) <- xy_wgs84[, 2L]
  names(lat_lyr) <- "lat"
  layers[["lat"]] <- lat_lyr

  terra::rast(layers)
}

#' Predict onto a raster grid
#'
#' Uses a trained model to predict values for each cell in the feature stack.
#'
#' @param model Fitted model object (ranger, xgb.Booster, or catboost.Model).
#' @param feature_stack Multi-layer SpatRaster from build_feature_stack.
#' @param feature_names Character vector of feature names (layer names).
#' @param clamp Logical; clamp predictions to training range.
#' @param train_y Numeric vector of training response (for clamping).
#' @return SpatRaster with predictions.
predict_to_raster <- function(model, feature_stack, feature_names,
                              clamp = TRUE, train_y = NULL) {
  # terra::predict needs a function wrapper.
  # Subsetting data to feature_names ensures correct column order for all engines.
  if (inherits(model, "ranger")) {
    pred_fun <- function(model, data, ...) {
      stats::predict(model, data = data[, feature_names, drop = FALSE])$predictions
    }
    pred_raster <- terra::predict(feature_stack, model, fun = pred_fun)
  } else if (inherits(model, "xgb.Booster")) {
    pred_fun <- function(model, data, ...) {
      dmat <- xgboost::xgb.DMatrix(
        data = as.matrix(data[, feature_names, drop = FALSE])
      )
      stats::predict(model, newdata = dmat)
    }
    pred_raster <- terra::predict(feature_stack, model, fun = pred_fun)
  } else if (inherits(model, "catboost.Model")) {
    pred_fun <- function(model, data, ...) {
      df      <- as.data.frame(data[, feature_names, drop = FALSE])
      cat_idx <- get_cat_feature_indices(df)
      pool    <- catboost::catboost.load_pool(
        data         = df,
        cat_features = if (length(cat_idx) > 0) cat_idx else NULL
      )
      catboost::catboost.predict(model, pool)
    }
    pred_raster <- terra::predict(feature_stack, model, fun = pred_fun)
  } else {
    stop(sprintf("Unsupported model class: %s", paste(class(model), collapse = ", ")),
         call. = FALSE)
  }

  # Clamp to training range
  if (isTRUE(clamp) && !is.null(train_y) && length(train_y) > 0) {
    y_min <- min(train_y, na.rm = TRUE)
    y_max <- max(train_y, na.rm = TRUE)
    pred_raster <- terra::clamp(pred_raster, lower = y_min, upper = y_max)
  }

  names(pred_raster) <- "prediction"
  pred_raster
}

#' Write prediction raster to GeoTIFF
#'
#' All predictions land in a single flat folder (no per-country subdirectory)
#' so they are easy to discover as a collection.
#'
#' @param pred_raster SpatRaster of predictions.
#' @param label File-name label (e.g. \code{"global_1990"}).
#' @param model_id Model identifier.
#' @param output_dir Output directory (relative to root); defaults to
#'   data/final/predictions.
#' @param root_dir Project root.
#' @return Character path of written file.
write_prediction_raster <- function(pred_raster, label, model_id,
                                    output_dir = "data/final/predictions",
                                    root_dir = ".") {
  out_dir <- file.path(root_dir, output_dir)
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  filename <- paste0(label, "_prediction_", model_id, ".tif")
  out_path <- file.path(out_dir, filename)

  terra::writeRaster(pred_raster, out_path, overwrite = TRUE)
  out_path
}

#' Run full raster prediction pipeline for one time slice
#'
#' Creates a global grid, builds the feature stack (including \code{lat},
#' \code{lon}, \code{log_area}, and a constant \code{year} layer at
#' \code{prediction_year}), runs the best model, and writes a GeoTIFF.
#'
#' @param best_cv_result Best model CV result from select_best_model.
#' @param panel_sf sf panel used for grid extent and clamping range.
#' @param feature_registry List of enabled feature entries.
#' @param ml_cfg Parsed ML config.
#' @param canonical_crs CRS string.
#' @param prediction_year Integer year to embed as the constant year feature.
#'   Defaults to \code{max(panel_sf$year)}.
#' @param label File-name prefix (e.g. \code{"global_1990"}).
#' @param output_dir Output directory.
#' @param root_dir Project root.
#' @return Character path of written GeoTIFF.
run_raster_prediction <- function(best_cv_result, panel_sf, feature_registry,
                                  ml_cfg, canonical_crs = "EPSG:3035",
                                  prediction_year = NULL,
                                  label = "global",
                                  output_dir = "data/final/predictions",
                                  root_dir = ".") {

  resolution_m <- ml_cfg$ml$raster_prediction$resolution_m %||% 1000
  clamp        <- ml_cfg$ml$raster_prediction$clamp_to_training_range %||% TRUE
  output_dir   <- output_dir %||% "data/final/predictions"

  # Fall back to most recent year in the panel when not supplied
  if (is.null(prediction_year)) {
    panel_df        <- sf::st_drop_geometry(panel_sf)
    prediction_year <- if ("year" %in% names(panel_df)) {
      max(as.integer(panel_df$year), na.rm = TRUE)
    } else NULL
  }

  grid <- create_prediction_grid(
    panel_sf      = panel_sf,
    resolution_m  = resolution_m,
    canonical_crs = canonical_crs
  )

  feature_stack <- build_feature_stack(
    grid_template    = grid,
    feature_registry = feature_registry,
    canonical_crs    = canonical_crs,
    root_dir         = root_dir,
    resolution_m     = resolution_m,
    prediction_year  = prediction_year,
    feature_names    = best_cv_result$feature_names
  )

  # Get training y for clamping
  target_var <- ml_cfg$ml$target_variable %||% "population"
  train_y    <- as.numeric(sf::st_drop_geometry(panel_sf)[[target_var]])

  pred_raster <- predict_to_raster(
    model         = best_cv_result$final_model,
    feature_stack = feature_stack,
    feature_names = best_cv_result$feature_names,
    clamp         = clamp,
    train_y       = train_y
  )

  write_prediction_raster(
    pred_raster = pred_raster,
    label       = label,
    model_id    = best_cv_result$model_id,
    output_dir  = output_dir,
    root_dir    = root_dir
  )
}
