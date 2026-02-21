`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

# ── SoilGrids 250m v2.0 ─────────────────────────────────────────
#
# Physical properties: bdod, cfvo, clay, sand, silt
# Chemical properties: cec, nitrogen, phh2o, soc, ocd, ocs
#
# Standard depths: 0-5 cm, 5-15 cm, 15-30 cm, 30-60 cm,
#                  60-100 cm, 100-200 cm
# Exception: ocs is only available at 0-30 cm
#
# Data access: ISRIC WebDAV via GDAL /vsicurl/ + VRT files
# Reference: Poggio et al. (2021) SOIL 7:217-240
# ─────────────────────────────────────────────────────────────────

#' Build table of SoilGrids layer specifications
#'
#' Returns a data.frame describing every property × depth combination
#' with download metadata.
#'
#' @return data.frame with columns: property, depth, layer_id, col_name.
soilgrids_layer_spec <- function() {
  properties <- c(
    "bdod", "cfvo", "clay", "sand", "silt",
    "cec", "nitrogen", "phh2o", "soc", "ocd"
  )
  depths <- c("0-5cm", "5-15cm", "15-30cm", "30-60cm",
              "60-100cm", "100-200cm")

  grid <- expand.grid(
    property = properties, depth = depths,
    stringsAsFactors = FALSE
  )
  # ocs only at 0-30 cm

  grid <- rbind(grid, data.frame(property = "ocs", depth = "0-30cm"))

  grid$layer_id <- paste0(grid$property, "_", grid$depth, "_mean")
  depth_clean   <- gsub("-", "_", gsub("cm$", "", grid$depth))
  grid$col_name <- paste0("soil_", grid$property, "_", depth_clean)

  grid
}

#' Download a single SoilGrids layer
#'
#' Reads the remote VRT via \code{/vsicurl/}, crops to the study
#' bounding box, projects to the canonical CRS, and writes a GeoTIFF.
#' Skips if the output file already exists.
#'
#' @param property SoilGrids property ID.
#' @param depth    Depth label (e.g. \code{"0-5cm"}).
#' @param out_path Absolute output path for the GeoTIFF.
#' @param bbox_wgs84 Numeric vector \code{c(xmin, ymin, xmax, ymax)} in WGS84.
#' @param canonical_crs CRS string for the output raster.
#' @param timeout_s Download timeout in seconds.
#' @return Character path to the written GeoTIFF.
download_single_soilgrids_layer <- function(property, depth, out_path,
                                            bbox_wgs84,
                                            canonical_crs = "EPSG:3035",
                                            timeout_s = 600L,
                                            max_retries = 3L,
                                            retry_wait_s = 30L) {
  if (file.exists(out_path)) {
    message(sprintf("[soilgrids] Already cached: %s", basename(out_path)))
    return(out_path)
  }

  dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)

  layer_id <- paste0(property, "_", depth, "_mean")
  vrt_url  <- paste0(
    "/vsicurl/https://files.isric.org/soilgrids/latest/data/",
    property, "/", layer_id, ".vrt"
  )

  prev_timeout <- getOption("timeout")
  on.exit(options(timeout = prev_timeout), add = TRUE)
  options(timeout = timeout_s)

  # Prepare the crop extent once (project study bbox to native CRS)
  bbox_poly <- sf::st_as_sfc(sf::st_bbox(
    c(xmin = bbox_wgs84[1], ymin = bbox_wgs84[2],
      xmax = bbox_wgs84[3], ymax = bbox_wgs84[4]),
    crs = "EPSG:4326"
  ))

  last_err <- NULL
  for (attempt in seq_len(max_retries)) {
    if (attempt > 1L) {
      message(sprintf(
        "[soilgrids] Retry %d/%d for %s (waiting %ds)...",
        attempt, max_retries, layer_id, retry_wait_s
      ))
      Sys.sleep(retry_wait_s)
    } else {
      message(sprintf("[soilgrids] Downloading %s ...", layer_id))
    }

    last_err <- tryCatch({
      r <- terra::rast(vrt_url)

      data_crs  <- terra::crs(r)
      bbox_proj <- sf::st_transform(bbox_poly, data_crs)
      crop_ext  <- terra::ext(sf::st_bbox(bbox_proj))

      r_crop <- terra::crop(r, crop_ext)

      # Validate that the crop actually produced data
      if (terra::ncell(r_crop) == 0 || all(is.na(terra::values(r_crop)))) {
        stop("Crop returned empty raster — remote tile read likely failed",
             call. = FALSE)
      }

      r_proj <- terra::project(r_crop, canonical_crs, method = "bilinear")

      terra::writeRaster(r_proj, out_path, overwrite = TRUE)
      message(sprintf("[soilgrids] Written: %s", basename(out_path)))
      return(out_path)
    }, error = function(e) e)
  }

  # All retries exhausted
  stop(
    sprintf(
      "[soilgrids] Failed to download %s after %d attempts: %s",
      layer_id, max_retries, conditionMessage(last_err)
    ),
    call. = FALSE
  )
}

#' Download all SoilGrids rasters for the study area
#'
#' Iterates over every property × depth combination, downloads each
#' layer, crops to the configured bounding box, and reprojects to the
#' canonical CRS. Files are cached locally and skipped on re-run.
#'
#' @param soilgrids_cfg Parsed \code{config/sources/soilgrids.yml}.
#' @param canonical_crs CRS string.
#' @param root_dir Project root.
#' @return Character vector of written GeoTIFF paths.
download_soilgrids_rasters <- function(soilgrids_cfg,
                                       canonical_crs = "EPSG:3035",
                                       root_dir = ".") {
  processed_dir <- file.path(
    root_dir,
    soilgrids_cfg$storage$processed_dir %||%
      "data/intermediate/features/soilgrids"
  )
  bbox      <- as.numeric(unlist(
    soilgrids_cfg$processing$bbox_wgs84 %||% c(-10, 40, 20, 60)
  ))
  timeout_s <- as.integer(soilgrids_cfg$source$download_timeout_s %||% 600L)

  spec  <- soilgrids_layer_spec()
  paths <- character(nrow(spec))

  for (i in seq_len(nrow(spec))) {
    out_path <- file.path(processed_dir, paste0(spec$col_name[i], ".tif"))
    paths[i] <- download_single_soilgrids_layer(
      property      = spec$property[i],
      depth         = spec$depth[i],
      out_path      = out_path,
      bbox_wgs84    = bbox,
      canonical_crs = canonical_crs,
      timeout_s     = timeout_s
    )
  }

  paths
}

# ── Zonal extraction ────────────────────────────────────────────

#' Extract zonal soil features for a panel
#'
#' Loops over all SoilGrids layers and appends a zonal-mean column
#' for each property × depth combination.
#'
#' @param panel_sf sf object with admin geometries (canonical CRS).
#' @param soilgrids_cfg Parsed soilgrids source config.
#' @param canonical_crs CRS string.
#' @param root_dir Project root.
#' @return sf object with soil_* columns appended.
extract_soil_zonal_features <- function(panel_sf, soilgrids_cfg,
                                        canonical_crs = "EPSG:3035",
                                        root_dir = ".") {
  processed_dir <- file.path(
    root_dir,
    soilgrids_cfg$storage$processed_dir %||%
      "data/intermediate/features/soilgrids"
  )
  spec <- soilgrids_layer_spec()

  for (i in seq_len(nrow(spec))) {
    tif_path <- file.path(processed_dir, paste0(spec$col_name[i], ".tif"))
    if (!file.exists(tif_path)) {
      warning(sprintf("[soilgrids] Missing raster: %s, filling NA",
                      basename(tif_path)))
      panel_sf[[spec$col_name[i]]] <- NA_real_
      next
    }

    r <- terra::rast(tif_path)
    if (!identical(terra::crs(r, describe = TRUE)$code,
                   sf::st_crs(canonical_crs)$epsg)) {
      r <- terra::project(r, canonical_crs)
    }

    panel_sf[[spec$col_name[i]]] <- extract_zonal_stat(
      raster      = r,
      panel_sf    = panel_sf,
      stat        = "mean",
      feature_col = spec$col_name[i]
    )
  }

  panel_sf
}

# ── PCA ─────────────────────────────────────────────────────────

#' Identify soil feature column names
#'
#' @param df A data.frame (or sf).
#' @return Character vector of column names matching \code{^soil_}.
soil_feature_columns <- function(df) {
  grep("^soil_", names(df), value = TRUE)
}

#' Fit PCA on soil feature columns
#'
#' Centers and scales all \code{soil_*} columns, runs \code{prcomp},
#' and selects the number of components that explain at least
#' \code{variance_threshold} of the total variance.
#'
#' @param panel_sf sf panel with soil columns.
#' @param variance_threshold Cumulative variance fraction (default 0.95).
#' @return Named list: \code{pca} (prcomp), \code{n_components},
#'   \code{soil_cols} (columns actually used), \code{all_soil_cols},
#'   \code{pc_names}, \code{variance_explained}.
fit_soil_pca <- function(panel_sf, variance_threshold = 0.95) {
  df        <- sf::st_drop_geometry(panel_sf)
  soil_cols <- soil_feature_columns(df)

  if (length(soil_cols) == 0) {
    stop("[soilgrids] No soil_* columns found for PCA", call. = FALSE)
  }

  soil_mat <- as.matrix(df[, soil_cols, drop = FALSE])

  # Impute NAs with column medians
  for (j in seq_len(ncol(soil_mat))) {
    na_idx <- is.na(soil_mat[, j])
    if (any(na_idx)) {
      soil_mat[na_idx, j] <- stats::median(soil_mat[, j], na.rm = TRUE)
    }
  }

  # Drop zero-variance columns
  col_sd    <- apply(soil_mat, 2, stats::sd, na.rm = TRUE)
  keep      <- col_sd > 0
  soil_mat  <- soil_mat[, keep, drop = FALSE]
  used_cols <- soil_cols[keep]

  if (ncol(soil_mat) < 2) {
    stop("[soilgrids] Fewer than 2 non-constant soil columns for PCA",
         call. = FALSE)
  }

  pca <- stats::prcomp(soil_mat, center = TRUE, scale. = TRUE)

  cum_var      <- cumsum(pca$sdev^2) / sum(pca$sdev^2)
  n_components <- which(cum_var >= variance_threshold)[1L]
  if (is.na(n_components)) n_components <- length(cum_var)
  n_components <- max(n_components, 1L)

  pc_names <- paste0("soil_pc", seq_len(n_components))

  message(sprintf(
    "[soilgrids] PCA: %d components explain %.1f%% variance (threshold: %.0f%%)",
    n_components, cum_var[n_components] * 100, variance_threshold * 100
  ))

  list(
    pca               = pca,
    n_components      = n_components,
    soil_cols         = used_cols,
    all_soil_cols     = soil_cols,
    pc_names          = pc_names,
    variance_explained = cum_var[seq_len(n_components)]
  )
}

#' Apply a fitted soil PCA to a panel
#'
#' Projects raw \code{soil_*} columns through the saved PCA rotation,
#' adds \code{soil_pc*} columns, and drops the original soil columns.
#'
#' @param panel_sf sf object with raw soil columns.
#' @param soil_pca_model List from \code{fit_soil_pca}.
#' @return sf object with \code{soil_pc*} columns replacing \code{soil_*}.
apply_soil_pca_to_panel <- function(panel_sf, soil_pca_model) {
  df        <- sf::st_drop_geometry(panel_sf)
  soil_cols <- soil_pca_model$soil_cols

  soil_mat <- as.matrix(df[, soil_cols, drop = FALSE])

  # Impute NAs with PCA center values
  for (j in seq_len(ncol(soil_mat))) {
    na_idx <- is.na(soil_mat[, j])
    if (any(na_idx)) {
      soil_mat[na_idx, j] <- soil_pca_model$pca$center[j]
    }
  }

  scores <- stats::predict(soil_pca_model$pca, newdata = soil_mat)
  n      <- soil_pca_model$n_components

  for (i in seq_len(n)) {
    panel_sf[[soil_pca_model$pc_names[i]]] <- scores[, i]
  }

  # Remove raw soil columns
  drop <- soil_pca_model$all_soil_cols
  panel_sf <- panel_sf[, !(names(panel_sf) %in% drop), drop = FALSE]

  panel_sf
}

# ── Raster prediction helpers ──────────────────────────────────

#' Build soil PCA raster layers for a prediction grid
#'
#' Loads all raw soil rasters, resamples to the prediction grid,
#' applies the saved PCA rotation, and returns a multi-layer
#' SpatRaster of PC components.
#'
#' @param grid_template SpatRaster template.
#' @param soil_pca_model List from \code{fit_soil_pca}.
#' @param canonical_crs CRS string.
#' @param root_dir Project root.
#' @param processed_dir Relative path to cached soil rasters.
#' @return Multi-layer SpatRaster with \code{soil_pc*} layers.
build_soil_pca_raster_stack <- function(grid_template, soil_pca_model,
                                        canonical_crs = "EPSG:3035",
                                        root_dir = ".",
                                        processed_dir = "data/intermediate/features/soilgrids") {
  soil_cols <- soil_pca_model$soil_cols
  full_dir  <- file.path(root_dir, processed_dir)

  soil_layers <- list()
  for (col_name in soil_cols) {
    tif_path <- file.path(full_dir, paste0(col_name, ".tif"))
    if (!file.exists(tif_path)) {
      stop(sprintf("[soilgrids] Missing raster for prediction: %s", tif_path),
           call. = FALSE)
    }
    r <- terra::rast(tif_path)
    r_aligned <- terra::resample(r, grid_template, method = "bilinear")
    soil_layers[[col_name]] <- r_aligned
  }

  soil_stack <- terra::rast(soil_layers)

  # Cell-wise PCA projection

  soil_mat <- terra::values(soil_stack)

  for (j in seq_len(ncol(soil_mat))) {
    na_idx <- is.na(soil_mat[, j])
    if (any(na_idx)) {
      soil_mat[na_idx, j] <- soil_pca_model$pca$center[j]
    }
  }

  scores <- stats::predict(soil_pca_model$pca, newdata = soil_mat)
  n      <- soil_pca_model$n_components

  pc_layers <- list()
  for (i in seq_len(n)) {
    lyr <- grid_template
    terra::values(lyr) <- scores[, i]
    names(lyr) <- soil_pca_model$pc_names[i]
    pc_layers[[i]] <- lyr
  }

  terra::rast(pc_layers)
}
