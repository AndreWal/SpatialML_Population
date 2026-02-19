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

#' Load a raster from disk
#'
#' Tries the processed path first, then the raw path (directory with .tif tiles).
#' Stops with an informative error if neither path yields a raster file.
#'
#' @param source_cfg Parsed source config list.
#' @param feature_entry Feature registry entry.
#' @param canonical_crs Canonical CRS string.
#' @param root_dir Project root.
#' @return A terra SpatRaster.
load_raster <- function(source_cfg, feature_entry,
                        canonical_crs = "EPSG:3035", root_dir = ".") {
  processed_path <- file.path(root_dir, source_cfg$storage$processed_path %||% "")
  raw_path <- file.path(root_dir, source_cfg$storage$raw_path %||% "")

  # Try processed first, then raw
  raster_path <- NULL
  if (nzchar(processed_path) && file.exists(processed_path) && !dir.exists(processed_path)) {
    raster_path <- processed_path
  } else if (nzchar(raw_path) && file.exists(raw_path)) {
    tif_files <- list.files(raw_path, pattern = "\\.tif$", full.names = TRUE, recursive = TRUE)
    if (length(tif_files) > 0) {
      raster_path <- tif_files[1]
    }
  }

  if (is.null(raster_path)) {
    stop(
      sprintf(
        "Raster not found for feature '%s'. Checked processed='%s', raw='%s'.",
        feature_entry$id %||% "?",
        source_cfg$storage$processed_path %||% "(none)",
        source_cfg$storage$raw_path %||% "(none)"
      ),
      call. = FALSE
    )
  }

  r <- terra::rast(raster_path)
  if (!identical(terra::crs(r, describe = TRUE)$code, sf::st_crs(canonical_crs)$epsg)) {
    r <- terra::project(r, canonical_crs)
  }
  r
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

#' Compute and attach geometric features
#'
#' Appends three derived columns to the panel:
#' \describe{
#'   \item{\code{log_area}}{log1p of polygon area in m², via \code{sf::st_area()}
#'     in the canonical projected CRS.}
#'   \item{\code{lon}}{WGS84 longitude of the polygon centroid (degrees).}
#'   \item{\code{lat}}{WGS84 latitude  of the polygon centroid (degrees).}
#' }
#'
#' @param panel_sf sf object in a metric projected CRS (e.g. EPSG:3035).
#' @return sf object with \code{log_area}, \code{lon}, \code{lat} appended.
add_geometric_features <- function(panel_sf) {
  # log_area
  areas <- tryCatch(
    as.numeric(sf::st_area(panel_sf)),
    error = function(e) rep(NA_real_, nrow(panel_sf))
  )
  panel_sf$log_area <- log1p(areas)

  # lat / lon: centroid coordinates reprojected to WGS84
  centroids_wgs84 <- tryCatch(
    sf::st_transform(sf::st_centroid(sf::st_geometry(panel_sf)), "EPSG:4326"),
    error = function(e) sf::st_centroid(sf::st_geometry(panel_sf))
  )
  coords <- sf::st_coordinates(centroids_wgs84)
  panel_sf$lon <- coords[, "X"]
  panel_sf$lat <- coords[, "Y"]

  panel_sf
}

#' Extract all enabled features and join to panel
#'
#' Iterates over the feature registry, loads rasters, extracts zonal
#' statistics, and joins them as columns to the panel sf object.
#' After raster extraction, geometric features (\code{log_area}) are appended.
#'
#' @param panel_sf sf country panel (in canonical CRS, validated geometry).
#' @param feature_registry List of enabled feature entries from features.yml.
#' @param canonical_crs Canonical CRS string.
#' @param root_dir Project root.
#' @return sf object with feature columns and \code{log_area} appended.
extract_and_join_features <- function(panel_sf, feature_registry,
                                      canonical_crs = "EPSG:3035",
                                      root_dir = ".") {
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

    source_cfg <- load_source_config(source_config, root_dir = root_dir)

    raster <- load_raster(
      source_cfg = source_cfg,
      feature_entry = feature_entry,
      canonical_crs = canonical_crs,
      root_dir = root_dir
    )

    stat <- source_cfg$processing$zonal_stat %||% "mean"
    panel_sf[[feature_id]] <- extract_zonal_stat(
      raster = raster,
      panel_sf = panel_sf,
      stat = stat,
      feature_col = feature_id
    )
  }

  # Always append derived geometric features
  panel_sf <- add_geometric_features(panel_sf)

  panel_sf
}
