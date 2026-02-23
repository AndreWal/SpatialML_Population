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

#' Resolve configured runtime worker count
#'
#' Reads `config/global/project.yml::runtime.parallel_workers` and returns
#' a safe integer >= 1. Falls back to 1 if config is missing or invalid.
#'
#' @param root_dir Project root.
#' @return Integer worker count.
get_runtime_parallel_workers <- function(root_dir = ".") {
  cfg_path <- file.path(root_dir, "config", "global", "project.yml")
  if (!file.exists(cfg_path)) {
    return(1L)
  }

  workers <- tryCatch({
    cfg <- yaml::read_yaml(cfg_path, eval.expr = FALSE)
    as.integer(cfg$runtime$parallel_workers %||% 1L)
  }, error = function(e) 1L)

  if (is.na(workers) || workers < 1L) {
    return(1L)
  }
  workers
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
  # Feature-level processed_path override: allows multiple features

  # (e.g. slope_mean, tri_mean) to share one source config while
  # pointing to different raster files.
  override_path <- feature_entry$processed_path
  if (!is.null(override_path) && nzchar(override_path)) {
    processed_path <- file.path(root_dir, override_path)
  } else {
    processed_path <- file.path(root_dir, source_cfg$storage$processed_path %||% "")
  }
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
  if (!terra::same.crs(r, terra::crs(canonical_crs))) {
    r <- terra::project(r, canonical_crs)
  }
  r
}

#' Extract zonal statistics from a raster for admin polygons
#'
#' Uses \code{exactextractr::exact_extract()} for polygon geometries
#' (much faster than \code{terra::extract()}, C++-threaded, area-weighted).
#' Falls back to \code{terra::extract()} for point geometries.
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

  # Guard: skip extraction if raster has zero extent or no overlap
  raster_ext <- tryCatch(terra::ext(raster), error = function(e) NULL)
  if (is.null(raster_ext) || all(terra::res(raster) == 0)) {
    warning(sprintf("[extract] Raster for '%s' has zero extent, returning NA", feature_col))
    return(result)
  }

  if (all(geom_types %in% c("POINT", "MULTIPOINT"))) {
    # Point extraction — exactextractr does not support points
    pts <- terra::vect(panel_nonempty)
    vals <- terra::extract(raster, pts)
    if (nrow(vals) > 0 && ncol(vals) >= 2) {
      result[!is_empty] <- vals[[2]]
    }
  } else {
    # Polygon zonal extraction via exactextractr (C++, area-weighted, fast)
    vals <- exactextractr::exact_extract(raster, panel_nonempty, fun = stat,
                                         progress = FALSE)
    result[!is_empty] <- vals
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

#' Download and prepare the study-area elevation raster
#'
#' All source metadata (URL, format, bounding box, timeout) is read from
#' \code{elevation_cfg} — nothing is hardcoded here. The function supports
#' both a direct GeoTIFF download (\code{source.format = "tif"}) and a
#' zip-wrapped TIF (\code{source.format = "zip"}).
#' Skips the download entirely if \code{storage.processed_path} already exists.
#'
#' @param elevation_cfg Parsed elevation source config list.
#' @param canonical_crs Canonical CRS string.
#' @param root_dir Project root directory.
#' @return Character path to the written (or already existing) raster.
download_elevation_raster <- function(elevation_cfg,
                                      canonical_crs = "EPSG:3035",
                                      root_dir = ".") {
  processed_path <- file.path(root_dir,
                              elevation_cfg$storage$processed_path %||% "")

  if (file.exists(processed_path)) {
    # Validate that the existing file is in the canonical CRS.
    # A pre-existing raw download (e.g. EPSG:4326) would silently corrupt
    # all downstream rasters that use the DEM as a spatial template.
    existing_r <- terra::rast(processed_path)
    if (terra::same.crs(existing_r, terra::crs(canonical_crs))) {
      message(sprintf("[elevation] Raster already present: %s", processed_path))
      return(processed_path)
    }
    message(sprintf(
      "[elevation] Existing raster at '%s' is not in %s — re-processing.",
      processed_path, canonical_crs
    ))
  }

  dir.create(dirname(processed_path), recursive = TRUE, showWarnings = FALSE)

  url     <- elevation_cfg$source$url
  format  <- tolower(elevation_cfg$source$format %||% "tif")
  timeout <- as.integer(elevation_cfg$source$download_timeout_s %||% 300L)
  bb      <- as.numeric(unlist(
    elevation_cfg$processing$bbox_wgs84 %||% c(-10, 40, 20, 60)
  ))

  if (is.null(url) || !nzchar(url)) {
    stop("[elevation] source.url is not set in elevation.yml", call. = FALSE)
  }

  message(sprintf(
    "[elevation] Downloading elevation raster from %s (bbox: %s) ...",
    url, paste(bb, collapse = ", ")
  ))

  prev_timeout <- getOption("timeout")
  on.exit(options(timeout = prev_timeout), add = TRUE)
  options(timeout = timeout)

  if (identical(format, "zip")) {
    tmp_z <- tempfile(fileext = ".zip")
    tmp_d <- tempfile()
    utils::download.file(url, destfile = tmp_z, mode = "wb", quiet = FALSE)
    utils::unzip(tmp_z, exdir = tmp_d)
    tif_path <- list.files(tmp_d, pattern = "\\.tif$",
                           full.names = TRUE, recursive = TRUE)[1L]
    if (is.na(tif_path) || !nzchar(tif_path)) {
      stop("[elevation] Downloaded zip contained no .tif file", call. = FALSE)
    }
  } else {
    tif_path <- tempfile(fileext = ".tif")
    utils::download.file(url, destfile = tif_path, mode = "wb", quiet = FALSE)
  }

  dem      <- terra::rast(tif_path)
  # terra::ext() order: xmin, xmax, ymin, ymax  (note: bb is xmin, ymin, xmax, ymax)
  dem_crop <- terra::crop(dem, terra::ext(bb[c(1L, 3L, 2L, 4L)]))
  dem_proj <- terra::project(dem_crop, canonical_crs)

  terra::writeRaster(dem_proj, processed_path, overwrite = TRUE)
  message(sprintf("[elevation] Written to: %s", processed_path))
  processed_path
}


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

  workers <- get_runtime_parallel_workers(root_dir = root_dir)
  # Cap terra threads: high thread counts on multi-layer stacks can
  # cause GDAL handle corruption leading to [extent=0] errors.
  # Note: terra::terraOptions() does NOT expose thread count, so
  # save/restore via $threads returns NULL and crashes.  Just reset
  # to 1 on exit (runs in isolated callr process anyway).
  safe_threads <- min(workers, 4L)
  terra::terraOptions(threads = safe_threads)
  on.exit(terra::terraOptions(threads = 1L), add = TRUE)

  message(sprintf("[features] Using %d terra thread(s) (requested %d, capped at 4).",
                  safe_threads, workers))

  for (feature_entry in feature_registry) {
    feature_id <- feature_entry$id
    source_config <- feature_entry$source_config

    if (is.null(source_config) || !nzchar(source_config)) {
      warning(sprintf("Feature '%s' has no source_config, skipping", feature_id))
      next
    }

    message(sprintf("[features] Loading raster for '%s' ...", feature_id))
    source_cfg <- load_source_config(source_config, root_dir = root_dir)

    raster <- load_raster(
      source_cfg = source_cfg,
      feature_entry = feature_entry,
      canonical_crs = canonical_crs,
      root_dir = root_dir
    )
    message(sprintf("[features] Extracting '%s' (nlyr=%d, ext=%s) ...",
                    feature_id, terra::nlyr(raster),
                    paste(round(as.vector(terra::ext(raster)), 0), collapse=",")))

    stat <- source_cfg$processing$zonal_stat %||% "mean"
    panel_sf[[feature_id]] <- extract_zonal_stat(
      raster = raster,
      panel_sf = panel_sf,
      stat = stat,
      feature_col = feature_id
    )
    message(sprintf("[features] '%s' done. Non-NA: %d/%d",
                    feature_id, sum(!is.na(panel_sf[[feature_id]])), nrow(panel_sf)))
  }

  # Extract SoilGrids zonal features (if source config exists)
  soilgrids_cfg_path <- file.path(root_dir, "config", "sources", "soilgrids.yml")
  if (file.exists(soilgrids_cfg_path)) {
    message("[features] Starting SoilGrids extraction ...")
    soilgrids_cfg <- yaml::read_yaml(soilgrids_cfg_path, eval.expr = FALSE)
    panel_sf <- extract_soil_zonal_features(
      panel_sf      = panel_sf,
      soilgrids_cfg = soilgrids_cfg,
      canonical_crs = canonical_crs,
      root_dir      = root_dir,
      workers       = min(workers, 4L)
    )
    message("[features] SoilGrids extraction complete.")
  }

  # Always append derived geometric features
  message("[features] Adding geometric features ...")
  panel_sf <- add_geometric_features(panel_sf)
  message("[features] All feature extraction complete.")

  panel_sf
}
