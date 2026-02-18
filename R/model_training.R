`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

#' Train a ranger random forest model
#'
#' @param train_X Feature data.frame.
#' @param train_y Numeric response vector.
#' @param seed Random seed.
#' @return A fitted ranger model object.
train_ranger <- function(train_X, train_y, seed = 42L) {
  df <- data.frame(y = train_y, train_X, check.names = FALSE)
  ranger::ranger(
    y ~ .,
    data = df,
    num.trees = 500,
    importance = "impurity",
    seed = seed,
    num.threads = 1L
  )
}

#' Train an xgboost gradient boosting model
#'
#' @param train_X Feature data.frame.
#' @param train_y Numeric response vector.
#' @param seed Random seed.
#' @return A fitted xgb.Booster object.
train_xgboost <- function(train_X, train_y, seed = 42L) {
  dtrain <- xgboost::xgb.DMatrix(data = as.matrix(train_X), label = train_y)
  params <- list(
    objective = "reg:squarederror",
    eval_metric = "rmse",
    max_depth = 6,
    eta = 0.1,
    subsample = 0.8,
    colsample_bytree = 0.8,
    nthread = 1L
  )
  set.seed(seed)
  xgboost::xgb.train(
    params = params,
    data = dtrain,
    nrounds = 200,
    verbose = 0
  )
}

#' Predict from a trained model
#'
#' @param model Fitted model object (ranger or xgb.Booster).
#' @param new_data Feature data.frame for prediction.
#' @return Numeric vector of predictions.
predict_model <- function(model, new_data) {
  if (inherits(model, "ranger")) {
    return(stats::predict(model, data = new_data)$predictions)
  }
  if (inherits(model, "xgb.Booster")) {
    dmat <- xgboost::xgb.DMatrix(data = as.matrix(new_data))
    return(stats::predict(model, newdata = dmat))
  }
  stop(sprintf("Unknown model class: %s", paste(class(model), collapse = ", ")), call. = FALSE)
}

#' Train a model by engine id
#'
#' Dispatches to the appropriate training function based on engine name.
#'
#' @param engine Character: "ranger" or "xgboost".
#' @param train_X Feature data.frame.
#' @param train_y Numeric response vector.
#' @param seed Random seed.
#' @return Fitted model object.
train_model <- function(engine, train_X, train_y, seed = 42L) {
  if (identical(engine, "ranger")) {
    return(train_ranger(train_X, train_y, seed = seed))
  }
  if (identical(engine, "xgboost")) {
    return(train_xgboost(train_X, train_y, seed = seed))
  }
  stop(sprintf("Unsupported model engine: %s", engine), call. = FALSE)
}

#' Run full spatial CV for a single model spec
#'
#' Trains per-fold, collects predictions, computes metrics.
#'
#' @param model_spec A single entry from ml_cfg$ml$models (list with id, engine).
#' @param model_data List from prepare_model_data.
#' @param folds Integer vector of fold assignments.
#' @param ml_cfg Parsed ML config.
#' @param seed Project seed.
#' @return List with model_id, engine, fold_results, overall_metrics, final_model.
run_spatial_cv <- function(model_spec, model_data, folds, ml_cfg, seed = 42L) {
  model_id <- model_spec$id %||% model_spec$engine
  engine <- model_spec$engine
  n_folds <- length(unique(folds[model_data$complete_idx]))

  fold_results <- list()
  all_preds <- numeric(length(model_data$y))
  all_obs <- numeric(length(model_data$y))
  pred_assigned <- logical(length(model_data$y))

  fold_ids <- sort(unique(folds[model_data$complete_idx]))

  for (fold_id in fold_ids) {
    split <- split_by_fold(model_data, folds, fold_id)

    # Skip if train or test set is empty
    if (length(split$train_y) < 2 || length(split$test_y) < 1) {
      next
    }

    fit <- train_model(engine, split$train_X, split$train_y, seed = seed + fold_id)
    preds <- predict_model(fit, split$test_X)

    fold_metrics <- compute_metrics(split$test_y, preds)
    fold_results[[length(fold_results) + 1]] <- list(
      fold = fold_id,
      n_train = length(split$train_y),
      n_test = length(split$test_y),
      metrics = fold_metrics,
      predictions = preds,
      observed = split$test_y
    )

    # Map back to complete_idx position
    test_pos <- match(split$test_idx, model_data$complete_idx)
    all_preds[test_pos] <- preds
    all_obs[test_pos] <- split$test_y
    pred_assigned[test_pos] <- TRUE
  }

  # Overall metrics from all held-out predictions
  overall_metrics <- if (sum(pred_assigned) > 0) {
    compute_metrics(all_obs[pred_assigned], all_preds[pred_assigned])
  } else {
    list(rmse = NA_real_, mae = NA_real_, rsq = NA_real_)
  }

  # Train final model on all data
  final_model <- train_model(engine, model_data$X, model_data$y, seed = seed)

  list(
    model_id = model_id,
    engine = engine,
    fold_results = fold_results,
    overall_metrics = overall_metrics,
    final_model = final_model,
    feature_names = model_data$feature_names
  )
}
