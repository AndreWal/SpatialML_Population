`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

# ── Water distance features ─────────────────────────────────────
#
# Two raster features:
#   dist_coast_km  – terrain-weighted distance (km) to nearest coastline
#   dist_river_km  – terrain-weighted distance (km) to nearest major river
#
# Distances are computed via least-cost path analysis (terra::costDist)
# with slope as a friction surface, so traversing steep terrain incurs
# higher cost than crossing flat ground.
#
# Data sources:
#   Coast: Natural Earth 10m coastline (public domain)
#   Rivers: HydroRIVERS v1.0 (HydroSHEDS / WWF), filtered by Strahler order
#   Slope: derived from WorldClim DEM (computed in terrain_features.R)
#
# Both distance rasters are aligned to the processed DEM grid.
# ─────────────────────────────────────────────────────────────────

#' Download Natural Earth 10m coastline shapefile
#'
#' Downloads the zip archive, extracts to \code{storage.coastline_raw_dir},
#' and returns the path to the \code{.shp} file.
#' Skips if the shapefile already exists on disk.
#'
#' @param water_cfg Parsed water-distance source config list.
#' @param root_dir Project root.
#' @return Path to the coastline \code{.shp} file.
download_coastline <- function(water_cfg, root_dir = ".") {
  raw_dir <- file.path(root_dir, water_cfg$storage$coastline_raw_dir)

  shp_files <- list.files(raw_dir, pattern = "\\.shp$",
                          full.names = TRUE, recursive = TRUE)
  if (length(shp_files) > 0) {
    message("[water] Coastline shapefile already present.")
    return(shp_files[1])
  }

  dir.create(raw_dir, recursive = TRUE, showWarnings = FALSE)
  if (!dir.exists(raw_dir)) {
    stop(sprintf(
      "[water] Cannot create directory '%s' — check filesystem permissions",
      raw_dir
    ), call. = FALSE)
  }

  url     <- water_cfg$source$coastline_url
  timeout <- as.integer(water_cfg$source$download_timeout_s %||% 300L)

  if (is.null(url) || !nzchar(url)) {
    stop("[water] source.coastline_url is not set in water_distance.yml",
         call. = FALSE)
  }

  prev_timeout <- getOption("timeout")
  on.exit(options(timeout = prev_timeout), add = TRUE)
  options(timeout = timeout)

  tmp_zip <- tempfile(fileext = ".zip")
  message(sprintf("[water] Downloading coastline from %s ...", url))
  utils::download.file(url, destfile = tmp_zip, mode = "wb", quiet = FALSE)
  utils::unzip(tmp_zip, exdir = raw_dir)
  unlink(tmp_zip)

  shp_files <- list.files(raw_dir, pattern = "\\.shp$",
                          full.names = TRUE, recursive = TRUE)
  if (length(shp_files) == 0) {
    stop("[water] No .shp file found in coastline download", call. = FALSE)
  }

  message(sprintf("[water] Coastline extracted to: %s", shp_files[1]))
  shp_files[1]
}

#' Download HydroRIVERS shapefile for a continent
#'
#' Downloads the zip archive, extracts to \code{storage.hydrorivers_raw_dir},
#' and returns the path to the \code{.shp} file.
#' Skips if the shapefile already exists on disk.
#'
#' @param water_cfg Parsed water-distance source config list.
#' @param root_dir Project root.
#' @return Path to the HydroRIVERS \code{.shp} file.
download_hydrorivers <- function(water_cfg, root_dir = ".") {
  raw_dir <- file.path(root_dir, water_cfg$storage$hydrorivers_raw_dir)

  shp_files <- list.files(raw_dir, pattern = "\\.shp$",
                          full.names = TRUE, recursive = TRUE)
  if (length(shp_files) > 0) {
    message("[water] HydroRIVERS shapefile already present.")
    return(shp_files[1])
  }

  dir.create(raw_dir, recursive = TRUE, showWarnings = FALSE)
  if (!dir.exists(raw_dir)) {
    stop(sprintf(
      "[water] Cannot create directory '%s' — check filesystem permissions",
      raw_dir
    ), call. = FALSE)
  }

  url     <- water_cfg$source$hydrorivers_url
  timeout <- as.integer(water_cfg$source$download_timeout_s %||% 600L)

  if (is.null(url) || !nzchar(url)) {
    stop("[water] source.hydrorivers_url is not set in water_distance.yml",
         call. = FALSE)
  }

  prev_timeout <- getOption("timeout")
  on.exit(options(timeout = prev_timeout), add = TRUE)
  options(timeout = timeout)

  tmp_zip <- tempfile(fileext = ".zip")
  message(sprintf("[water] Downloading HydroRIVERS from %s ...", url))
  utils::download.file(url, destfile = tmp_zip, mode = "wb", quiet = FALSE)
  utils::unzip(tmp_zip, exdir = raw_dir)
  unlink(tmp_zip)

  shp_files <- list.files(raw_dir, pattern = "\\.shp$",
                          full.names = TRUE, recursive = TRUE)
  if (length(shp_files) == 0) {
    stop("[water] No .shp file found in HydroRIVERS download", call. = FALSE)
  }

  message(sprintf("[water] HydroRIVERS extracted to: %s", shp_files[1]))
  shp_files[1]
}

#' Build a slope-based friction surface for cost-distance analysis
#'
#' Creates a cost raster where each cell's value represents the traversal cost
#' in "effective metres".  Flat cells cost exactly \code{res_m} (the cell
#' resolution); steeper cells cost proportionally more:
#'
#' \deqn{
#'   \text{cost} = \text{res}_m \times
#'     \bigl(1 + \text{penalty} \times \text{slope}_\text{deg} / 90\bigr)
#' }
#'
#' This means a cell with 45° slope costs \eqn{1 + 0.5 \times \text{penalty}}
#' times as much as a flat cell, and a vertical cliff (90°) costs
#' \eqn{1 + \text{penalty}} times as much.
#'
#' @param slope_raster SpatRaster of slope in degrees.
#' @param res_m Cell resolution in metres (scalar).
#' @param penalty Numeric slope penalty weight (default 5).
#' @return SpatRaster cost surface (effective-metres per cell).
build_cost_surface <- function(slope_raster, res_m, penalty = 5) {
  # Clamp negative or NA slopes to 0
  slope_clamped <- terra::clamp(slope_raster, lower = 0, values = TRUE)
  # Replace remaining NAs (e.g. at ocean cells) with 0
  slope_clamped[is.na(slope_clamped)] <- 0

  cost <- res_m * (1 + penalty * slope_clamped / 90)
  names(cost) <- "cost"
  cost
}

#' Compute cost-distance from target vector features
#'
#' Rasterizes the target features (coast or river lines) onto the DEM grid,
#' builds a slope-based friction surface, and runs \code{terra::costDist()} to
#' compute the accumulated least-cost distance from every cell to the nearest
#' target cell.
#'
#' @param dem SpatRaster DEM (used as spatial template).
#' @param slope_raster SpatRaster of slope in degrees (same extent/res as DEM).
#' @param target_vect SpatVector of target features (lines or polygons).
#' @param penalty Numeric slope penalty weight.
#' @return SpatRaster of cost-distance in kilometres.
compute_cost_distance <- function(dem, slope_raster, target_vect, penalty = 5) {
  res_m <- mean(terra::res(dem))

  # Align slope raster to the DEM grid (terrain() can subtly alter extent)
  if (!terra::compareGeom(dem, slope_raster, stopOnError = FALSE)) {
    message("[water]   Resampling slope raster to match DEM grid...")
    slope_raster <- terra::resample(slope_raster, dem, method = "bilinear")
  }

  # Build friction surface (now guaranteed same grid as DEM)
  cost_surface <- build_cost_surface(slope_raster, res_m = res_m, penalty = penalty)

  # Rasterize targets onto the same grid
  target_raster <- terra::rasterize(target_vect, dem, field = 1, touches = TRUE)

  # Set target cells in the cost surface to 0 (free to enter)
  cost_surface[!is.na(target_raster)] <- 0

  # Run least-cost distance (accumulated cost from target cells)
  message("[water]   Running costDist (least-cost path analysis)...")
  dist_r <- terra::costDist(cost_surface, target = 0)

  # Convert effective metres → kilometres
  dist_r <- dist_r / 1000
  dist_r
}

#' Compute distance-to-water rasters via least-cost path analysis
#'
#' Creates two GeoTIFF rasters aligned to the processed DEM grid:
#' \enumerate{
#'   \item \code{dist_coast_km} – terrain-weighted distance to nearest
#'         coastline (km), penalised by slope
#'   \item \code{dist_river_km} – terrain-weighted distance to nearest
#'         major river (km), filtered by Strahler stream order, penalised
#'         by slope
#' }
#'
#' Distances are computed with \code{terra::costDist()} over a friction
#' surface derived from slope.  Because the canonical CRS (EPSG:3035) is
#' metric and equal-area, the cost surface is expressed in effective metres
#' (flat distance × slope penalty).
#'
#' @param water_cfg Parsed water-distance source config list.
#' @param dem_path Path to the processed DEM raster (used as spatial template).
#' @param slope_path Path to the processed slope raster (degrees).
#' @param canonical_crs Canonical CRS string.
#' @param root_dir Project root.
#' @return Character vector of output raster paths.
compute_water_distance_rasters <- function(water_cfg,
                                           dem_path,
                                           slope_path,
                                           canonical_crs = "EPSG:3035",
                                           root_dir = ".") {
  coast_path <- file.path(root_dir, water_cfg$storage$dist_coast_path)
  river_path <- file.path(root_dir, water_cfg$storage$dist_river_path)

  # Early return if both outputs exist
  if (file.exists(coast_path) && file.exists(river_path)) {
    message("[water] Distance rasters already exist; skipping.")
    return(c(coast_path, river_path))
  }

  dir.create(dirname(coast_path), recursive = TRUE, showWarnings = FALSE)
  dir.create(dirname(river_path), recursive = TRUE, showWarnings = FALSE)

  dem         <- terra::rast(dem_path)
  slope_r     <- terra::rast(slope_path)
  penalty     <- as.numeric(water_cfg$processing$slope_penalty %||% 5)

  bb <- as.numeric(unlist(
    water_cfg$processing$bbox_wgs84 %||% c(-10, 40, 20, 60)
  ))
  # bbox as WGS84 sfc for spatial filtering
  bbox_wgs84 <- sf::st_bbox(
    c(xmin = bb[1], ymin = bb[2], xmax = bb[3], ymax = bb[4]),
    crs = sf::st_crs("EPSG:4326")
  )

  # ── Coast distance (least-cost path) ─────────────────────────
  if (!file.exists(coast_path)) {
    coast_shp <- download_coastline(water_cfg, root_dir)
    message("[water] Loading coastline and cropping to study area...")
    coast_sf <- sf::st_read(coast_shp, quiet = TRUE)
    coast_sf <- sf::st_crop(coast_sf, bbox_wgs84)
    coast_sf <- sf::st_transform(coast_sf, canonical_crs)

    # Ensure valid geometry
    if (any(!sf::st_is_valid(coast_sf))) {
      coast_sf <- sf::st_make_valid(coast_sf)
    }

    coast_vect <- terra::vect(coast_sf)

    message("[water] Computing cost-distance to coast (slope penalty = ",
            penalty, ") ...")
    dist_coast <- compute_cost_distance(dem, slope_r, coast_vect,
                                        penalty = penalty)
    names(dist_coast) <- "dist_coast_km"

    terra::writeRaster(dist_coast, coast_path, overwrite = TRUE)
    message(sprintf("[water] Coast distance written to: %s", coast_path))
  }

  # ── River distance (least-cost path) ────────────────────────
  if (!file.exists(river_path)) {
    strahler_min <- as.integer(
      water_cfg$processing$strahler_min_order %||% 4L
    )

    river_shp  <- download_hydrorivers(water_cfg, root_dir)
    layer_name <- tools::file_path_sans_ext(basename(river_shp))

    # Read with attribute filter (Strahler order) and spatial bbox filter
    # to minimise memory usage.
    bbox_wkt <- sf::st_as_text(sf::st_as_sfc(bbox_wgs84))
    query    <- sprintf(
      "SELECT * FROM \"%s\" WHERE ORD_STRA >= %d",
      layer_name, strahler_min
    )
    message(sprintf(
      "[water] Loading HydroRIVERS (Strahler >= %d) within study bbox...",
      strahler_min
    ))
    rivers_sf <- sf::st_read(
      river_shp, query = query,
      wkt_filter = bbox_wkt, quiet = TRUE
    )

    if (nrow(rivers_sf) == 0) {
      stop(
        sprintf(
          "[water] No HydroRIVERS features with Strahler >= %d in study area",
          strahler_min
        ),
        call. = FALSE
      )
    }

    rivers_sf <- sf::st_transform(rivers_sf, canonical_crs)
    if (any(!sf::st_is_valid(rivers_sf))) {
      rivers_sf <- sf::st_make_valid(rivers_sf)
    }

    rivers_vect <- terra::vect(rivers_sf)

    message("[water] Computing cost-distance to major rivers (slope penalty = ",
            penalty, ") ...")
    dist_river <- compute_cost_distance(dem, slope_r, rivers_vect,
                                        penalty = penalty)
    names(dist_river) <- "dist_river_km"

    terra::writeRaster(dist_river, river_path, overwrite = TRUE)
    message(sprintf("[water] River distance written to: %s", river_path))
  }

  c(coast_path, river_path)
}
