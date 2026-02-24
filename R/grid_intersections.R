if (!exists("%||%", mode = "function")) {
  `%||%` <- function(x, y) {
    if (is.null(x)) y else x
  }
}

#' Build grid-cell polygons from a prediction raster template
#'
#' @param grid_template SpatRaster template defining grid geometry.
#' @return sf object with one polygon per cell and metadata columns.
build_grid_cells <- function(grid_template) {
  if (!inherits(grid_template, "SpatRaster")) {
    stop("grid_template must be a terra::SpatRaster", call. = FALSE)
  }

  grid_sf <- sf::st_as_sf(terra::as.polygons(grid_template, values = FALSE, na.rm = FALSE))
  n <- nrow(grid_sf)
  if (n == 0) {
    grid_sf$cell_index <- integer(0)
    grid_sf$cell_id <- character(0)
    grid_sf$cell_area_m2 <- numeric(0)
    return(grid_sf)
  }

  grid_sf$cell_index <- seq_len(n)
  grid_sf$cell_id <- sprintf("cell_%d", grid_sf$cell_index)
  grid_sf$cell_area_m2 <- as.numeric(sf::st_area(grid_sf))
  grid_sf
}

allocation_key_cols <- function(df) {
  cols <- c("country_code", "admin_unit_harmonized", "year")
  cols[cols %in% names(df)]
}

empty_intersections_df <- function(with_geometry = FALSE, crs = NA) {
  base <- data.frame(
    country_code = character(0),
    admin_unit_harmonized = character(0),
    year = integer(0),
    population = numeric(0),
    polygon_area_m2 = numeric(0),
    cell_index = integer(0),
    cell_id = character(0),
    cell_area_m2 = numeric(0),
    overlap_area_m2 = numeric(0),
    stringsAsFactors = FALSE
  )
  if (!with_geometry) return(base)
  sf::st_sf(base, geometry = sf::st_sfc(crs = crs))
}

#' Build exact polygon-grid intersections in canonical CRS
#'
#' @param panel_sf sf polygon panel in canonical equal-area CRS.
#' @param grid_template SpatRaster template defining the prediction grid.
#' @param canonical_crs Optional CRS string to enforce for both inputs.
#' @param keep_geometry Logical; keep intersection geometry when TRUE.
#' @return sf/data.frame of polygon-year x cell intersections with exact overlap area.
build_polygon_grid_intersections <- function(panel_sf,
                                             grid_template,
                                             canonical_crs = NULL,
                                             keep_geometry = FALSE) {
  if (!inherits(panel_sf, "sf")) {
    stop("panel_sf must be an sf object", call. = FALSE)
  }
  if (!inherits(grid_template, "SpatRaster")) {
    stop("grid_template must be a terra::SpatRaster", call. = FALSE)
  }
  if (nrow(panel_sf) == 0) {
    return(empty_intersections_df(with_geometry = keep_geometry,
                                  crs = sf::st_crs(panel_sf)))
  }

  panel_work <- panel_sf
  if (!is.null(canonical_crs)) {
    panel_work <- sf::st_transform(panel_work, canonical_crs)
  }

  keep_cols <- unique(c(
    allocation_key_cols(panel_work),
    intersect(c("population"), names(panel_work))
  ))
  panel_work <- panel_work[, keep_cols, drop = FALSE]
  panel_work$polygon_row_id <- seq_len(nrow(panel_work))
  panel_work$polygon_area_m2 <- as.numeric(sf::st_area(panel_work))

  is_empty <- sf::st_is_empty(panel_work)
  if (all(is_empty)) {
    return(empty_intersections_df(with_geometry = keep_geometry,
                                  crs = sf::st_crs(panel_work)))
  }
  panel_work <- panel_work[!is_empty, , drop = FALSE]

  # Only polygon support rows can be intersected against the grid.
  geom_types <- unique(as.character(sf::st_geometry_type(panel_work)))
  if (any(!geom_types %in% c("POLYGON", "MULTIPOLYGON"))) {
    stop("build_polygon_grid_intersections() requires polygon geometries", call. = FALSE)
  }

  grid_sf <- build_grid_cells(grid_template)
  if (!is.null(canonical_crs)) {
    grid_sf <- sf::st_transform(grid_sf, canonical_crs)
  }

  if (nrow(grid_sf) == 0) {
    return(empty_intersections_df(with_geometry = keep_geometry,
                                  crs = sf::st_crs(panel_work)))
  }

  intersections <- suppressWarnings(sf::st_intersection(panel_work, grid_sf))
  if (nrow(intersections) == 0) {
    return(empty_intersections_df(with_geometry = keep_geometry,
                                  crs = sf::st_crs(panel_work)))
  }

  intersections$overlap_area_m2 <- as.numeric(sf::st_area(intersections))
  keep <- is.finite(intersections$overlap_area_m2) & intersections$overlap_area_m2 > 0
  intersections <- intersections[keep, , drop = FALSE]

  if (nrow(intersections) == 0) {
    return(empty_intersections_df(with_geometry = keep_geometry,
                                  crs = sf::st_crs(panel_work)))
  }

  # Keep a consistent column order for downstream allocation helpers.
  ordered_cols <- c(
    "country_code", "admin_unit_harmonized", "year", "population",
    "polygon_row_id", "polygon_area_m2",
    "cell_index", "cell_id", "cell_area_m2", "overlap_area_m2"
  )
  ordered_cols <- ordered_cols[ordered_cols %in% names(intersections)]
  intersections <- intersections[, c(ordered_cols, setdiff(names(intersections), ordered_cols)), drop = FALSE]

  if (!keep_geometry) {
    intersections <- sf::st_drop_geometry(intersections)
  }

  intersections
}

#' Rasterize cell-level values onto an existing grid template
#'
#' @param grid_template SpatRaster template.
#' @param cell_values data.frame with `cell_index` and a numeric value column.
#' @param value_col Column to rasterize.
#' @param fun Aggregation function for duplicate cell rows (`sum` by default).
#' @param background Background value for cells without data.
#' @param layer_name Output layer name.
#' @return SpatRaster with one layer.
rasterize_cell_values <- function(grid_template,
                                  cell_values,
                                  value_col,
                                  fun = sum,
                                  background = NA_real_,
                                  layer_name = "value") {
  if (!inherits(grid_template, "SpatRaster")) {
    stop("grid_template must be a terra::SpatRaster", call. = FALSE)
  }
  if (!is.data.frame(cell_values)) {
    stop("cell_values must be a data.frame", call. = FALSE)
  }
  if (!all(c("cell_index", value_col) %in% names(cell_values))) {
    stop("cell_values must contain `cell_index` and the requested value column", call. = FALSE)
  }

  out <- grid_template
  vals <- rep(background, terra::ncell(grid_template))

  if (nrow(cell_values) > 0) {
    idx <- as.integer(cell_values$cell_index)
    v <- as.numeric(cell_values[[value_col]])
    keep <- is.finite(idx) & idx >= 1L & idx <= length(vals) & is.finite(v)
    if (any(keep)) {
      agg <- stats::aggregate(
        x = v[keep],
        by = list(cell_index = idx[keep]),
        FUN = fun
      )
      vals[agg$cell_index] <- as.numeric(agg$x)
    }
  }

  terra::values(out) <- vals
  names(out) <- layer_name
  out
}
