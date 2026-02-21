`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

#' Compute terrain-derived rasters (slope and TRI) from a DEM
#'
#' Reads the processed elevation raster, computes slope (in degrees) and
#' Terrain Ruggedness Index (TRI), and writes them as GeoTIFFs.
#' Skips computation if output files already exist.
#'
#' @param dem_path Path to the processed DEM raster (GeoTIFF).
#' @param terrain_cfg Parsed terrain source config list.
#' @param canonical_crs Canonical CRS string (used for validation).
#' @param root_dir Project root directory.
#' @return Character vector of output raster paths (slope, TRI).
compute_terrain_rasters <- function(dem_path,
                                    terrain_cfg,
                                    canonical_crs = "EPSG:3035",
                                    root_dir = ".") {
  slope_path <- file.path(root_dir, terrain_cfg$storage$slope_path)
  tri_path   <- file.path(root_dir, terrain_cfg$storage$tri_path)

  # Early return if both outputs exist
  if (file.exists(slope_path) && file.exists(tri_path)) {
    message("[terrain] Slope and TRI rasters already exist; skipping.")
    return(c(slope_path, tri_path))
  }

  dir.create(dirname(slope_path), recursive = TRUE, showWarnings = FALSE)
  dir.create(dirname(tri_path),   recursive = TRUE, showWarnings = FALSE)

  dem <- terra::rast(dem_path)

  # Validate CRS
  dem_crs <- terra::crs(dem, describe = TRUE)$code
  if (!is.null(dem_crs) && dem_crs != sf::st_crs(canonical_crs)$epsg) {
    message("[terrain] Reprojecting DEM to canonical CRS before terrain computation.")
    dem <- terra::project(dem, canonical_crs)
  }

  # Compute slope (degrees)
  if (!file.exists(slope_path)) {
    message("[terrain] Computing slope from DEM...")
    slope_r <- terra::terrain(dem, v = "slope", unit = "degrees")
    terra::writeRaster(slope_r, slope_path, overwrite = TRUE)
    message(sprintf("[terrain] Slope written to: %s", slope_path))
  }

  # Compute Terrain Ruggedness Index (Riley et al. 1999)
  if (!file.exists(tri_path)) {
    message("[terrain] Computing TRI from DEM...")
    tri_r <- terra::terrain(dem, v = "TRI")
    terra::writeRaster(tri_r, tri_path, overwrite = TRUE)
    message(sprintf("[terrain] TRI written to: %s", tri_path))
  }

  c(slope_path, tri_path)
}
