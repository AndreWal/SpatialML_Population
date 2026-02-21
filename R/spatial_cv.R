`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

#' Create spatial block CV resamples using spatialsample
#'
#' Partitions space into \code{v} contiguous blocks using
#' \code{spatialsample::spatial_block_cv}, which provides a stronger
#' guard against spatial autocorrelation leakage than random k-fold.
#' Returns an \code{rsample} rset indexed into the plain (no-geometry)
#' model data.frame — ready for \code{tune::tune_bayes} and
#' \code{tune::fit_resamples} without any further conversion.
#'
#' @param panel_sf sf object for the training countries (projected CRS).
#' @param model_data List from prepare_model_data.
#' @param ml_cfg Parsed ML config list.
#' @param seed Random seed.
#' @return An rsample manual_rset.
create_spatial_resamples <- function(panel_sf, model_data, ml_cfg, seed = 42L) {
  n_folds <- ml_cfg$ml$split$folds %||% 5L

  # Minimal sf: a row_id column + geometry for the complete-case rows.
  # row_id lets us map block assignments back to the plain model data.frame.
  geom_sf <- sf::st_sf(
    data.frame(row_id = seq_along(model_data$y), check.names = FALSE),
    geometry = sf::st_geometry(panel_sf)[model_data$complete_idx]
  )

  set.seed(seed)
  sp_cv <- spatialsample::spatial_block_cv(geom_sf, v = n_folds)

  # Plain (no-geometry) data.frame that the workflow will be trained on
  df <- cbind(
    data.frame(.outcome = model_data$y, check.names = FALSE),
    model_data$X
  )

  # Re-index each split into df (not geom_sf), preserving the spatial blocking
  splits <- lapply(sp_cv$splits, function(sp) {
    tr_ids  <- rsample::analysis(sp)$row_id
    tst_ids <- rsample::assessment(sp)$row_id
    rsample::make_splits(list(analysis = tr_ids, assessment = tst_ids), data = df)
  })
  rsample::manual_rset(splits, sp_cv$id)
}

#' Prepare model data from a panel sf object
#'
#' Extracts the target variable and feature columns, drops geometry and
#' key columns. Returns a list with `y` (numeric vector) and `X` (data.frame).
#'
#' @param panel_sf sf object with target and feature columns.
#' @param ml_cfg Parsed ML config list.
#' @param feature_registry List of enabled feature entries.
#' @return List with elements `y`, `X`, `feature_names`, and `complete_idx`.
prepare_model_data <- function(panel_sf, ml_cfg, feature_registry) {
  target_var <- ml_cfg$ml$target_variable %||% "population"
  feature_names <- vapply(feature_registry, `[[`, character(1), "id")

  df <- sf::st_drop_geometry(panel_sf)

  # Check that columns exist

  missing_target <- !target_var %in% names(df)
  if (missing_target) {
    stop(sprintf("Target variable '%s' not found in panel", target_var), call. = FALSE)
  }
  missing_features <- setdiff(feature_names, names(df))
  if (length(missing_features) > 0) {
    stop(
      sprintf("Missing feature columns: %s", paste(missing_features, collapse = ", ")),
      call. = FALSE
    )
  }

  # Append built-in scalar derived features if present in the panel
  derived_scalar <- c("log_area", "lon", "lat")
  for (d in derived_scalar) {
    if (d %in% names(df) && !d %in% feature_names) {
      feature_names <- c(feature_names, d)
    }
  }

  # Append soil PCA components if present (from soil PCA step)
  soil_pc_cols <- sort(grep("^soil_pc\\d+$", names(df), value = TRUE))
  for (d in soil_pc_cols) {
    if (!d %in% feature_names) {
      feature_names <- c(feature_names, d)
    }
  }

  # Expand 'year' into per-decade dummy columns (year_1850, year_1860, …).
  # Each column is 1 for observations of that decade and 0 otherwise.
  # This lets every tree-based engine treat decades as unordered categories
  # without relying on engine-specific categorical support.
  if ("year" %in% names(df)) {
    year_levels     <- sort(unique(as.character(df$year)))
    year_dummy_names <- paste0("year_", year_levels)
    for (lvl in year_levels) {
      col_name    <- paste0("year_", lvl)
      df[[col_name]] <- as.integer(as.character(df$year) == lvl)
    }
    feature_names <- c(feature_names, year_dummy_names)
  }

  y <- as.numeric(df[[target_var]])
  X <- df[, feature_names, drop = FALSE]

  # Handle complete cases only
  complete <- complete.cases(cbind(y, X))

  list(
    y = y[complete],
    X = X[complete, , drop = FALSE],
    feature_names = feature_names,
    complete_idx = which(complete)
  )
}
