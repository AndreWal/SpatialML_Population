`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

#' Load the feature registry from features.yml
#'
#' @param root_dir Project root.
#' @param features_file Relative path to features.yml.
#' @return List of enabled feature entries.
load_feature_registry <- function(root_dir = ".",
                                  features_file = file.path("config", "sources", "features.yml")) {
  path <- file.path(root_dir, features_file)
  if (!file.exists(path)) {
    stop(sprintf("Feature registry not found: %s", path), call. = FALSE)
  }
  cfg <- yaml::read_yaml(path, eval.expr = FALSE)
  registry <- cfg$features_registry %||% list()
  Filter(function(f) isTRUE(f$enabled), registry)
}

#' Load a feature source config
#'
#' @param source_config Relative path to source config yml.
#' @param root_dir Project root.
#' @return Parsed source config list.
load_source_config <- function(source_config, root_dir = ".") {
  path <- file.path(root_dir, source_config)
  if (!file.exists(path)) {
    stop(sprintf("Source config not found: %s", path), call. = FALSE)
  }
  yaml::read_yaml(path, eval.expr = FALSE)
}

#' Create a deterministic mock raster for testing
#'
#' Covers the bounding box of the provided sf object with spatially varying
#' synthetic values. Used when real raster data is unavailable (MOCK_MODE).
#'
#' @param panel_sf sf object to derive extent from.
#' @param feature_id Feature identifier (seeds the values).
#' @param resolution_m Approximate resolution in metres.
#' @param canonical_crs CRS string for output.
#' @return A terra SpatRaster.
make_mock_raster <- function(panel_sf, feature_id = "mock",
                             resolution_m = 1000, canonical_crs = "EPSG:3035") {
  panel_proj <- sf::st_transform(panel_sf, canonical_crs)

  # Filter out empty geometries for bbox calculation
  non_empty <- !sf::st_is_empty(panel_proj)
  if (any(non_empty)) {
    bbox <- sf::st_bbox(panel_proj[non_empty, ])
  } else {
    # Fallback: create a small raster around the origin of the CRS
    bbox <- c(xmin = -10000, ymin = -10000, xmax = 10000, ymax = 10000)
    names(bbox) <- c("xmin", "ymin", "xmax", "ymax")
  }

  # Guard against NA or degenerate bbox
  if (any(is.na(bbox))) {
    bbox <- c(xmin = -10000, ymin = -10000, xmax = 10000, ymax = 10000)
    names(bbox) <- c("xmin", "ymin", "xmax", "ymax")
  }

  # Buffer extent slightly so polygons are fully covered
  dx <- (bbox["xmax"] - bbox["xmin"]) * 0.1 + resolution_m
  dy <- (bbox["ymax"] - bbox["ymin"]) * 0.1 + resolution_m
  ext <- terra::ext(
    bbox["xmin"] - dx, bbox["xmax"] + dx,
    bbox["ymin"] - dy, bbox["ymax"] + dy
  )

  ncol <- max(5L, as.integer(ceiling((ext[2] - ext[1]) / resolution_m)))
  nrow <- max(5L, as.integer(ceiling((ext[4] - ext[3]) / resolution_m)))

  r <- terra::rast(ext, nrows = nrow, ncols = ncol, crs = canonical_crs)

  # Deterministic fill based on feature_id hash
  seed_val <- sum(utf8ToInt(feature_id))
  set.seed(seed_val)
  terra::values(r) <- runif(terra::ncell(r), min = 0, max = 1000)
  names(r) <- feature_id
  r
}

#' Load a raster from disk or create a mock
#'
#' @param source_cfg Parsed source config list.
#' @param feature_entry Feature registry entry.
#' @param panel_sf sf panel for mock extent.
#' @param canonical_crs Canonical CRS string.
#' @param root_dir Project root.
#' @param mock_mode Logical; use mock if raster not found.
#' @return A terra SpatRaster.
load_or_mock_raster <- function(source_cfg, feature_entry, panel_sf,
                                canonical_crs = "EPSG:3035",
                                root_dir = ".", mock_mode = TRUE) {
  processed_path <- file.path(root_dir, source_cfg$storage$processed_path %||% "")
  raw_path <- file.path(root_dir, source_cfg$storage$raw_path %||% "")

  # Try processed first, then raw
  raster_path <- NULL
  if (nzchar(processed_path) && file.exists(processed_path) && !dir.exists(processed_path)) {
    raster_path <- processed_path
  } else if (nzchar(raw_path) && file.exists(raw_path)) {
    # raw_path might be a directory with tiles
    tif_files <- list.files(raw_path, pattern = "\\.tif$", full.names = TRUE, recursive = TRUE)
    if (length(tif_files) > 0) {
      raster_path <- tif_files[1]
    }
  }

  if (!is.null(raster_path)) {
    r <- terra::rast(raster_path)
    # Reproject to canonical CRS if needed
    if (!identical(terra::crs(r, describe = TRUE)$code, sf::st_crs(canonical_crs)$epsg)) {
      r <- terra::project(r, canonical_crs)
    }
    return(r)
  }

  if (isTRUE(mock_mode)) {
    return(make_mock_raster(
      panel_sf,
      feature_id = feature_entry$id %||% "mock",
      canonical_crs = canonical_crs
    ))
  }

  warning(
    sprintf("Raster not found for feature '%s'; using mock raster as fallback",
            feature_entry$id),
    call. = FALSE
  )
  make_mock_raster(
    panel_sf,
    feature_id = feature_entry$id %||% "mock",
    canonical_crs = canonical_crs
  )
}

#' Extract zonal statistics from a raster for admin polygons
#'
#' @param raster A terra SpatRaster.
#' @param panel_sf sf object with polygon/point geometries.
#' @param stat Zonal statistic: "mean", "sum", "min", "max", "median".
#' @param feature_col Name for the output column.
#' @return Numeric vector of extracted values (one per row of panel_sf).
extract_zonal_stat <- function(raster, panel_sf, stat = "mean", feature_col = "feature") {
  # Handle empty geometries: assign NA to empty rows, extract from non-empty
  is_empty <- sf::st_is_empty(panel_sf)
  result <- rep(NA_real_, nrow(panel_sf))

  if (all(is_empty)) {
    return(result)
  }

  panel_nonempty <- panel_sf[!is_empty, ]
  geom_types <- unique(as.character(sf::st_geometry_type(panel_nonempty)))

  if (all(geom_types %in% c("POINT", "MULTIPOINT"))) {
    # Point extraction
    pts <- terra::vect(panel_nonempty)
    vals <- terra::extract(raster, pts)
    result[!is_empty] <- vals[[2]]
  } else {
    # Polygon zonal extraction
    polys <- terra::vect(panel_nonempty)
    vals <- terra::extract(raster, polys, fun = stat, na.rm = TRUE, exact = TRUE)
    result[!is_empty] <- vals[[2]]
  }

  result
}

#' Extract all enabled features and join to panel
#'
#' Iterates over the feature registry, loads/mocks rasters, extracts zonal
#' statistics, and joins them as columns to the panel sf object.
#'
#' @param panel_sf sf country panel (in canonical CRS, validated geometry).
#' @param feature_registry List of enabled feature entries from features.yml.
#' @param canonical_crs Canonical CRS string.
#' @param root_dir Project root.
#' @param mock_mode Logical.
#' @return sf object with feature columns appended.
extract_and_join_features <- function(panel_sf, feature_registry,
                                      canonical_crs = "EPSG:3035",
                                      root_dir = ".", mock_mode = TRUE) {
  if (length(feature_registry) == 0) {
    return(panel_sf)
  }

  for (feature_entry in feature_registry) {
    feature_id <- feature_entry$id
    source_config <- feature_entry$source_config

    if (is.null(source_config) || !nzchar(source_config)) {
      warning(sprintf("Feature '%s' has no source_config, skipping", feature_id))
      next
    }

    source_cfg <- tryCatch(
      load_source_config(source_config, root_dir = root_dir),
      error = function(e) {
        if (isTRUE(mock_mode)) return(list())
        stop(e)
      }
    )

    raster <- load_or_mock_raster(
      source_cfg = source_cfg,
      feature_entry = feature_entry,
      panel_sf = panel_sf,
      canonical_crs = canonical_crs,
      root_dir = root_dir,
      mock_mode = mock_mode
    )

    stat <- source_cfg$processing$zonal_stat %||% "mean"
    panel_sf[[feature_id]] <- extract_zonal_stat(
      raster = raster,
      panel_sf = panel_sf,
      stat = stat,
      feature_col = feature_id
    )
  }

  panel_sf
}
