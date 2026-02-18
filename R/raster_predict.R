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
#' Loads (or mocks) each raster feature and stacks them into a single
#' multi-layer SpatRaster aligned to the prediction grid.
#'
#' @param grid_template SpatRaster template from create_prediction_grid.
#' @param feature_registry List of enabled feature entries.
#' @param panel_sf sf panel for mock raster extent.
#' @param canonical_crs CRS string.
#' @param root_dir Project root.
#' @param mock_mode Logical.
#' @return A multi-layer SpatRaster with one layer per feature.
build_feature_stack <- function(grid_template, feature_registry, panel_sf,
                                canonical_crs = "EPSG:3035",
                                root_dir = ".", mock_mode = TRUE) {
  layers <- list()

  for (feature_entry in feature_registry) {
    source_config <- feature_entry$source_config
    source_cfg <- tryCatch(
      load_source_config(source_config, root_dir = root_dir),
      error = function(e) {
        if (isTRUE(mock_mode)) return(list())
        stop(e)
      }
    )

    r <- load_or_mock_raster(
      source_cfg = source_cfg,
      feature_entry = feature_entry,
      panel_sf = panel_sf,
      canonical_crs = canonical_crs,
      root_dir = root_dir,
      mock_mode = mock_mode
    )

    # Resample to prediction grid
    r_aligned <- terra::resample(r, grid_template, method = "bilinear")
    names(r_aligned) <- feature_entry$id
    layers[[length(layers) + 1]] <- r_aligned
  }

  if (length(layers) == 0) {
    stop("No feature layers available for prediction", call. = FALSE)
  }

  terra::rast(layers)
}

#' Predict onto a raster grid
#'
#' Uses a trained model to predict values for each cell in the feature stack.
#'
#' @param model Fitted model object (ranger or xgb.Booster).
#' @param feature_stack Multi-layer SpatRaster from build_feature_stack.
#' @param feature_names Character vector of feature names (layer names).
#' @param clamp Logical; clamp predictions to training range.
#' @param train_y Numeric vector of training response (for clamping).
#' @return SpatRaster with predictions.
predict_to_raster <- function(model, feature_stack, feature_names,
                              clamp = TRUE, train_y = NULL) {
  # terra::predict needs a function wrapper
  if (inherits(model, "ranger")) {
    pred_fun <- function(model, data, ...) {
      stats::predict(model, data = data)$predictions
    }
    pred_raster <- terra::predict(feature_stack, model, fun = pred_fun)
  } else if (inherits(model, "xgb.Booster")) {
    pred_fun <- function(model, data, ...) {
      dmat <- xgboost::xgb.DMatrix(data = as.matrix(data))
      stats::predict(model, newdata = dmat)
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
#' @param pred_raster SpatRaster of predictions.
#' @param country_code ISO3 country code.
#' @param model_id Model identifier.
#' @param output_dir Output directory (relative to root).
#' @param root_dir Project root.
#' @return Character path of written file.
write_prediction_raster <- function(pred_raster, country_code, model_id,
                                    output_dir = "data/final", root_dir = ".") {
  out_dir <- file.path(root_dir, output_dir, country_code)
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  filename <- paste0(country_code, "_prediction_", model_id, ".tif")
  out_path <- file.path(out_dir, filename)

  terra::writeRaster(pred_raster, out_path, overwrite = TRUE)
  out_path
}

#' Run full raster prediction pipeline for one country
#'
#' Creates grid, builds feature stack, predicts, writes output.
#'
#' @param best_cv_result Best model CV result from select_best_model.
#' @param panel_sf sf country panel (for extent and training data).
#' @param feature_registry List of enabled feature entries.
#' @param ml_cfg Parsed ML config.
#' @param canonical_crs CRS string.
#' @param country_code ISO3 code.
#' @param output_dir Output directory.
#' @param root_dir Project root.
#' @param mock_mode Logical.
#' @return Character path of written GeoTIFF.
run_raster_prediction <- function(best_cv_result, panel_sf, feature_registry,
                                  ml_cfg, canonical_crs = "EPSG:3035",
                                  country_code = "UNK",
                                  output_dir = "data/final",
                                  root_dir = ".", mock_mode = TRUE) {

  resolution_m <- ml_cfg$ml$raster_prediction$resolution_m %||% 1000
  clamp <- ml_cfg$ml$raster_prediction$clamp_to_training_range %||% TRUE

  grid <- create_prediction_grid(
    panel_sf = panel_sf,
    resolution_m = resolution_m,
    canonical_crs = canonical_crs
  )

  feature_stack <- build_feature_stack(
    grid_template = grid,
    feature_registry = feature_registry,
    panel_sf = panel_sf,
    canonical_crs = canonical_crs,
    root_dir = root_dir,
    mock_mode = mock_mode
  )

  # Get training y for clamping
  target_var <- ml_cfg$ml$target_variable %||% "population"
  train_y <- as.numeric(sf::st_drop_geometry(panel_sf)[[target_var]])

  pred_raster <- predict_to_raster(
    model = best_cv_result$final_model,
    feature_stack = feature_stack,
    feature_names = best_cv_result$feature_names,
    clamp = clamp,
    train_y = train_y
  )

  write_prediction_raster(
    pred_raster = pred_raster,
    country_code = country_code,
    model_id = best_cv_result$model_id,
    output_dir = output_dir,
    root_dir = root_dir
  )
}
