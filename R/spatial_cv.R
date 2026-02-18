`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

#' Create spatial block folds for cross-validation
#'
#' Divides the study area into regular blocks and assigns each block to a fold.
#' Observations are assigned to the fold of the block their centroid falls in.
#'
#' @param panel_sf sf object with geometries (must have a projected CRS).
#' @param ml_cfg Parsed ML config list from ml.yml.
#' @param seed Random seed for fold assignment.
#' @return Integer vector of fold assignments (same length as nrow(panel_sf)).
create_spatial_folds <- function(panel_sf, ml_cfg, seed = 42L) {
  n_folds <- ml_cfg$ml$split$folds %||% 5L
  block_size_km <- ml_cfg$ml$split$block_size_km %||% 100
  block_size_m <- block_size_km * 1000

  # Get centroids in the projected CRS
  centroids <- sf::st_coordinates(sf::st_centroid(sf::st_geometry(panel_sf)))

  # Create grid cell indices for each centroid
  x_idx <- floor(centroids[, 1] / block_size_m)
  y_idx <- floor(centroids[, 2] / block_size_m)
  block_id <- paste(x_idx, y_idx, sep = "_")

  # Assign each unique block to a fold

  unique_blocks <- unique(block_id)
  set.seed(seed)
  block_fold <- sample(rep_len(seq_len(n_folds), length(unique_blocks)))
  names(block_fold) <- unique_blocks

  # Map each observation to its block's fold
  fold_assignment <- unname(block_fold[block_id])
  fold_assignment
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
