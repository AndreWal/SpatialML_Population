`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

#' Create spatial block folds for cross-validation using CAST
#'
#' Clusters observations into spatial k-means blocks and then uses
#' \code{CAST::CreateSpacetimeFolds} to produce balanced,
#' spatially-aware CV splits.
#'
#' @param panel_sf sf object with geometries (must have a projected CRS).
#' @param ml_cfg Parsed ML config list from ml.yml.
#' @param seed Random seed for fold assignment.
#' @return Integer vector of fold assignments (same length as nrow(panel_sf)).
create_spatial_folds <- function(panel_sf, ml_cfg, seed = 42L) {
  n_folds <- ml_cfg$ml$split$folds %||% 5L

  # Get centroids in the projected CRS
  centroids <- sf::st_coordinates(sf::st_centroid(sf::st_geometry(panel_sf)))

  # K-means spatial clustering: more clusters than folds so CAST can balance them
  n_clusters <- min(n_folds * 5L, nrow(centroids))
  n_clusters <- max(n_clusters, n_folds)  # at least n_folds clusters
  set.seed(seed)
  km <- stats::kmeans(centroids, centers = n_clusters, nstart = 10L)

  # Attach spatial block labels (must be character for CreateSpacetimeFolds)
  x_df <- data.frame(spatial_block = as.character(km$cluster),
                     stringsAsFactors = FALSE)

  # CAST::CreateSpacetimeFolds returns a list with $index (train) and $indexOut (test)
  folds_cast <- CAST::CreateSpacetimeFolds(
    x     = x_df,
    spacevar = "spatial_block",
    k     = n_folds,
    seed  = seed
  )

  # Convert to a plain integer fold vector: each obs gets the fold id where
  # it appears in indexOut (i.e. as the held-out / test portion)
  fold_vec <- integer(nrow(panel_sf))
  for (f in seq_along(folds_cast$indexOut)) {
    fold_vec[folds_cast$indexOut[[f]]] <- f
  }
  # Safety: any unassigned observation joins fold 1
  fold_vec[fold_vec == 0L] <- 1L
  fold_vec
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

#' Split model data by spatial folds
#'
#' @param model_data List from prepare_model_data.
#' @param folds Integer vector of fold assignments.
#' @param fold_id Which fold to hold out for testing.
#' @return List with train_X, train_y, test_X, test_y, test_idx.
split_by_fold <- function(model_data, folds, fold_id) {
  # folds is for original rows; model_data uses complete_idx subset
  fold_subset <- folds[model_data$complete_idx]

  is_test <- fold_subset == fold_id
  is_train <- !is_test

  list(
    train_X = model_data$X[is_train, , drop = FALSE],
    train_y = model_data$y[is_train],
    test_X = model_data$X[is_test, , drop = FALSE],
    test_y = model_data$y[is_test],
    test_idx = model_data$complete_idx[is_test]
  )
}
