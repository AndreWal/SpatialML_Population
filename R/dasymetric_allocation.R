if (!exists("%||%", mode = "function")) {
  `%||%` <- function(x, y) {
    if (is.null(x)) y else x
  }
}

allocation_group_id <- function(df, key_cols) {
  if (length(key_cols) == 0) {
    return(rep.int("all", nrow(df)))
  }
  do.call(interaction, c(df[key_cols], list(drop = TRUE, lex.order = TRUE)))
}

as_nonnegative_numeric <- function(x) {
  out <- suppressWarnings(as.numeric(x))
  out[!is.finite(out)] <- NA_real_
  out[out < 0] <- 0
  out
}

base_allocation_output <- function(intersections_df) {
  out <- as.data.frame(intersections_df, stringsAsFactors = FALSE)
  if ("geometry" %in% names(out)) out$geometry <- NULL

  out$score_raw <- NA_real_
  out$area_denominator_m2 <- NA_real_
  out$weight_raw <- NA_real_
  out$weight_norm <- NA_real_
  out$population_polygon <- if ("population" %in% names(out)) as.numeric(out$population) else NA_real_
  out$pop_allocated <- NA_real_
  out$allocation_mode <- NA_character_
  out$fallback_flag <- FALSE
  out$quality_flag <- NA_character_
  out
}

normalize_group_weights <- function(w) {
  w <- as.numeric(w)
  if (length(w) == 0) return(w)
  s <- sum(w, na.rm = TRUE)
  if (!is.finite(s) || s <= 0) {
    return(rep(NA_real_, length(w)))
  }
  w / s
}

#' Allocate polygon totals uniformly by exact overlap area
#'
#' @param intersections_df data.frame from `build_polygon_grid_intersections()`.
#' @param area_denominator_col Area column to use for weighting.
#' @param polygon_total_col Column containing source polygon totals.
#' @return Cell-allocation data.frame with normalized weights and allocated counts.
allocate_uniform_by_area <- function(intersections_df,
                                     area_denominator_col = "overlap_area_m2",
                                     polygon_total_col = "population") {
  if (!is.data.frame(intersections_df)) {
    stop("intersections_df must be a data.frame", call. = FALSE)
  }
  if (!all(c(area_denominator_col, polygon_total_col) %in% names(intersections_df))) {
    stop("intersections_df is missing required columns", call. = FALSE)
  }

  out <- base_allocation_output(intersections_df)
  if (nrow(out) == 0) return(out)

  key_cols <- c("country_code", "admin_unit_harmonized", "year")
  key_cols <- key_cols[key_cols %in% names(out)]
  grp <- allocation_group_id(out, key_cols)

  area_vals <- as_nonnegative_numeric(out[[area_denominator_col]])
  pop_vals <- suppressWarnings(as.numeric(out[[polygon_total_col]]))
  out$score_raw <- 1
  out$area_denominator_m2 <- area_vals
  out$population_polygon <- pop_vals

  for (g in levels(grp)) {
    idx <- which(grp == g)
    if (length(idx) == 0) next

    a <- area_vals[idx]
    weight_raw <- ifelse(is.finite(a), a, NA_real_)
    has_positive_area <- any(is.finite(weight_raw) & weight_raw > 0)

    if (has_positive_area) {
      mode <- "uniform_area"
      qf <- "ok"
      fb <- FALSE
      w_norm <- normalize_group_weights(weight_raw)
    } else {
      mode <- "fallback_equal_cell"
      qf <- "empty_support"
      fb <- TRUE
      weight_raw <- rep(1, length(idx))
      w_norm <- rep(1 / length(idx), length(idx))
    }

    pop_g <- unique(pop_vals[idx])
    pop_g <- pop_g[is.finite(pop_g)]
    pop_one <- if (length(pop_g) > 0) pop_g[1] else NA_real_

    out$weight_raw[idx] <- weight_raw
    out$weight_norm[idx] <- w_norm
    out$allocation_mode[idx] <- mode
    out$fallback_flag[idx] <- fb
    out$quality_flag[idx] <- qf
    out$pop_allocated[idx] <- if (is.finite(pop_one)) pop_one * w_norm else NA_real_
  }

  out
}

#' Allocate polygon totals from nonnegative scores with fallback
#'
#' Weights are `score * area_denominator`, normalized within each polygon-year.
#' If all scores are missing/nonpositive within a polygon-year, the function
#' falls back to uniform area weighting.
#'
#' @param intersections_df data.frame with polygon-cell intersections.
#' @param score_col Column containing raw scores (nonnegative preferred).
#' @param area_denominator_col Area column to use in weight construction.
#' @param polygon_total_col Column with source polygon totals.
#' @return Cell-allocation data.frame.
allocate_constrained_from_scores <- function(intersections_df,
                                             score_col = "score_raw",
                                             area_denominator_col = "overlap_area_m2",
                                             polygon_total_col = "population") {
  if (!is.data.frame(intersections_df)) {
    stop("intersections_df must be a data.frame", call. = FALSE)
  }
  req <- c(score_col, area_denominator_col, polygon_total_col)
  if (!all(req %in% names(intersections_df))) {
    stop("intersections_df is missing required columns", call. = FALSE)
  }

  out <- base_allocation_output(intersections_df)
  if (nrow(out) == 0) return(out)

  key_cols <- c("country_code", "admin_unit_harmonized", "year")
  key_cols <- key_cols[key_cols %in% names(out)]
  grp <- allocation_group_id(out, key_cols)

  scores <- as_nonnegative_numeric(intersections_df[[score_col]])
  area_vals <- as_nonnegative_numeric(out[[area_denominator_col]])
  pop_vals <- suppressWarnings(as.numeric(out[[polygon_total_col]]))

  out$score_raw <- scores
  out$area_denominator_m2 <- area_vals
  out$population_polygon <- pop_vals

  for (g in levels(grp)) {
    idx <- which(grp == g)
    if (length(idx) == 0) next

    s <- scores[idx]
    a <- area_vals[idx]
    weight_raw <- s * a
    positive_weight <- is.finite(weight_raw) & weight_raw > 0

    if (any(positive_weight)) {
      mode <- "ml_weighted"
      qf <- "ok"
      fb <- FALSE
      w_norm <- normalize_group_weights(weight_raw)
    } else {
      # Fall back to uniform area if score support is zero/NA.
      fb_alloc <- allocate_uniform_by_area(
        intersections_df[idx, , drop = FALSE],
        area_denominator_col = area_denominator_col,
        polygon_total_col = polygon_total_col
      )
      mode <- "fallback_uniform_area"
      qf <- if (all(!is.finite(s))) "no_covariates" else "zero_weight"
      fb <- TRUE

      out$weight_raw[idx] <- fb_alloc$weight_raw
      out$weight_norm[idx] <- fb_alloc$weight_norm
      out$pop_allocated[idx] <- fb_alloc$pop_allocated
      out$allocation_mode[idx] <- mode
      out$fallback_flag[idx] <- fb
      out$quality_flag[idx] <- qf
      next
    }

    pop_g <- unique(pop_vals[idx])
    pop_g <- pop_g[is.finite(pop_g)]
    pop_one <- if (length(pop_g) > 0) pop_g[1] else NA_real_

    out$weight_raw[idx] <- weight_raw
    out$weight_norm[idx] <- w_norm
    out$pop_allocated[idx] <- if (is.finite(pop_one)) pop_one * w_norm else NA_real_
    out$allocation_mode[idx] <- mode
    out$fallback_flag[idx] <- fb
    out$quality_flag[idx] <- qf
  }

  # Guarantee nonnegative allocations (allow tiny negative numerical drift).
  out$pop_allocated[is.finite(out$pop_allocated) & out$pop_allocated < 0] <- 0
  out
}

semantic_raster_filename <- function(label, model_id, kind = c("intensity", "constrained", "calibrated")) {
  kind <- match.arg(kind)
  suffix <- switch(
    kind,
    intensity = paste0("_intensity_", model_id, ".tif"),
    constrained = paste0("_population_count_constrained_", model_id, ".tif"),
    calibrated = paste0("_population_count_calibrated_", model_id, ".tif")
  )
  paste0(label, suffix)
}

write_semantic_raster <- function(raster,
                                  label,
                                  model_id,
                                  kind = c("intensity", "constrained", "calibrated"),
                                  output_dir = "data/final/predictions",
                                  root_dir = ".") {
  kind <- match.arg(kind)
  if (!inherits(raster, "SpatRaster")) {
    stop("raster must be a terra::SpatRaster", call. = FALSE)
  }

  out_dir <- file.path(root_dir, output_dir)
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  out_path <- file.path(out_dir, semantic_raster_filename(label, model_id, kind = kind))
  terra::writeRaster(raster, out_path, overwrite = TRUE)
  out_path
}

#' Convert cell-allocation rows to a constrained population-count raster
#'
#' @param allocation_df Cell-allocation table with `cell_index` and `pop_allocated`.
#' @param grid_template SpatRaster template.
#' @param label File prefix (e.g. `global_1900`).
#' @param model_id Model/allocation identifier (e.g. `uniform_area`).
#' @param output_dir Output directory.
#' @param root_dir Project root.
#' @return Character path of written GeoTIFF.
write_constrained_population_raster <- function(allocation_df,
                                                grid_template,
                                                label,
                                                model_id,
                                                output_dir = "data/final/predictions",
                                                root_dir = ".") {
  r <- rasterize_cell_values(
    grid_template = grid_template,
    cell_values = allocation_df,
    value_col = "pop_allocated",
    fun = sum,
    background = NA_real_,
    layer_name = "population_count_constrained"
  )
  write_semantic_raster(
    raster = r,
    label = label,
    model_id = model_id,
    kind = "constrained",
    output_dir = output_dir,
    root_dir = root_dir
  )
}

#' Write an unconstrained relative intensity raster
#'
#' @param score_raster SpatRaster of relative intensity / score values.
#' @param label File prefix (e.g. `global_1900`).
#' @param model_id Model/allocation identifier.
#' @param output_dir Output directory.
#' @param root_dir Project root.
#' @return Character path of written GeoTIFF.
predict_unconstrained_intensity <- function(score_raster,
                                            label,
                                            model_id,
                                            output_dir = "data/final/predictions",
                                            root_dir = ".") {
  names(score_raster) <- "intensity"
  write_semantic_raster(
    raster = score_raster,
    label = label,
    model_id = model_id,
    kind = "intensity",
    output_dir = output_dir,
    root_dir = root_dir
  )
}

#' Calibrate a relative intensity raster to a known total
#'
#' @param score_raster SpatRaster of nonnegative scores.
#' @param target_total Numeric total to match after scaling.
#' @return Scaled SpatRaster with count semantics.
calibrate_intensity_raster <- function(score_raster, target_total) {
  if (!inherits(score_raster, "SpatRaster")) {
    stop("score_raster must be a terra::SpatRaster", call. = FALSE)
  }
  vals <- terra::values(score_raster, mat = FALSE)
  s <- sum(vals, na.rm = TRUE)
  if (!is.finite(s) || s <= 0) {
    stop("Cannot calibrate intensity raster with nonpositive total score", call. = FALSE)
  }
  out <- score_raster
  terra::values(out) <- vals * (as.numeric(target_total) / s)
  names(out) <- "population_count_calibrated"
  out
}

join_cell_scores_from_raster <- function(intersections_df,
                                         score_raster,
                                         score_col = "score_raw") {
  if (!is.data.frame(intersections_df)) {
    stop("intersections_df must be a data.frame", call. = FALSE)
  }
  if (!inherits(score_raster, "SpatRaster")) {
    stop("score_raster must be a terra::SpatRaster", call. = FALSE)
  }
  if (!"cell_index" %in% names(intersections_df)) {
    stop("intersections_df must contain `cell_index`", call. = FALSE)
  }

  out <- as.data.frame(intersections_df, stringsAsFactors = FALSE)
  vals <- terra::values(score_raster, mat = FALSE)
  idx <- as.integer(out$cell_index)
  ok <- is.finite(idx) & idx >= 1L & idx <= length(vals)
  out[[score_col]] <- NA_real_
  out[[score_col]][ok] <- as.numeric(vals[idx[ok]])
  out
}
