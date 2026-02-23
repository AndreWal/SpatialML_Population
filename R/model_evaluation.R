`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

#' Compute regression evaluation metrics
#'
#' @param observed Numeric vector of observed values.
#' @param predicted Numeric vector of predicted values.
#' @return Named list with rmse, mae, rsq.
compute_metrics <- function(observed, predicted) {
  if (length(observed) == 0 || length(predicted) == 0) {
    return(list(rmse = NA_real_, mae = NA_real_, rsq = NA_real_))
  }
  residuals <- observed - predicted
  rmse <- sqrt(mean(residuals^2, na.rm = TRUE))
  mae <- mean(abs(residuals), na.rm = TRUE)

  ss_res <- sum(residuals^2, na.rm = TRUE)
  ss_tot <- sum((observed - mean(observed, na.rm = TRUE))^2, na.rm = TRUE)
  rsq <- if (ss_tot > 0) 1 - ss_res / ss_tot else NA_real_

  list(rmse = rmse, mae = mae, rsq = rsq)
}

default_target_info <- function(target_var = "population") {
  list(
    target_var = target_var,
    response_kind = "raw_target",
    transform = "identity",
    needs_area = FALSE
  )
}

extract_variable_importance_df <- function(cv_result) {
  engine_fit <- workflows::extract_fit_engine(cv_result$final_model)
  engine <- cv_result$engine %||% ""

  if (identical(engine, "ranger")) {
    vi <- engine_fit$variable.importance %||% numeric(0)
    if (!length(vi)) return(NULL)
    out <- data.frame(
      feature = names(vi),
      importance = as.numeric(vi),
      stringsAsFactors = FALSE
    )
  } else if (identical(engine, "xgboost")) {
    vi_tbl <- tryCatch(xgboost::xgb.importance(model = engine_fit), error = function(e) NULL)
    if (is.null(vi_tbl) || !nrow(vi_tbl)) return(NULL)
    score_col <- if ("Gain" %in% names(vi_tbl)) "Gain" else names(vi_tbl)[2]
    out <- data.frame(
      feature = as.character(vi_tbl$Feature),
      importance = as.numeric(vi_tbl[[score_col]]),
      stringsAsFactors = FALSE
    )
  } else if (identical(engine, "lightgbm")) {
    vi_tbl <- tryCatch(lightgbm::lgb.importance(engine_fit), error = function(e) NULL)
    if (is.null(vi_tbl) || !nrow(vi_tbl)) return(NULL)
    feat_col <- if ("Feature" %in% names(vi_tbl)) "Feature" else names(vi_tbl)[1]
    score_col <- if ("Gain" %in% names(vi_tbl)) "Gain" else
      if ("Importance" %in% names(vi_tbl)) "Importance" else names(vi_tbl)[2]
    out <- data.frame(
      feature = as.character(vi_tbl[[feat_col]]),
      importance = as.numeric(vi_tbl[[score_col]]),
      stringsAsFactors = FALSE
    )
  } else {
    return(NULL)
  }

  out <- out[is.finite(out$importance) & !is.na(out$feature), , drop = FALSE]
  out <- out[order(out$importance, decreasing = TRUE), , drop = FALSE]
  rownames(out) <- NULL
  out
}

save_variable_importance_plot <- function(cv_result, output_dir = "models",
                                          root_dir = ".", top_n = 20L) {
  vi_df <- extract_variable_importance_df(cv_result)
  if (is.null(vi_df) || nrow(vi_df) < 1L) {
    warning(sprintf("Skipping variable-importance plot for %s: no importances available",
                    cv_result$model_id), call. = FALSE)
    return(NA_character_)
  }

  vi_dir <- file.path(root_dir, output_dir, "variable_importance")
  dir.create(vi_dir, recursive = TRUE, showWarnings = FALSE)

  vi_top <- head(vi_df, max(1L, as.integer(top_n)))
  out_path <- file.path(vi_dir, paste0(cv_result$model_id, "_var_importance.png"))

  grDevices::png(out_path, width = 1200, height = 900, res = 120)
  op <- graphics::par(no.readonly = TRUE)
  on.exit({
    graphics::par(op)
    grDevices::dev.off()
  }, add = TRUE)

  graphics::par(mar = c(5, 14, 4, 2))
  graphics::barplot(
    height = rev(vi_top$importance),
    names.arg = rev(vi_top$feature),
    horiz = TRUE,
    las = 1,
    col = "#3B7EA1",
    border = NA,
    cex.names = 0.8,
    main = sprintf("Variable Importance: %s (%s)", cv_result$model_id, cv_result$engine),
    xlab = "Importance"
  )
  graphics::box()

  out_path
}

#' Summarize cross-validation results across models
#'
#' Returns a data.frame where metric columns are prefixed with \code{cv_} and
#' provenance columns (\code{eval_set}, \code{n_cv_folds}, \code{n_cv_obs},
#' \code{train_countries}) make clear which data split was used.
#'
#' @param cv_results_list List of cv result objects (from run_spatial_cv).
#' @param train_countries Optional character vector of country codes used for
#'   training (to fill the \code{train_countries} column).
#' @return data.frame with one row per model and provenance + metric columns.
summarize_cv_results <- function(cv_results_list, train_countries = NULL) {
  train_ctry_str <- if (!is.null(train_countries) && length(train_countries) > 0) {
    paste(sort(unique(train_countries)), collapse = "+")
  } else NA_character_

  rows <- lapply(cv_results_list, function(res) {
    ti <- res$target_info %||% default_target_info()
    data.frame(
      model_id        = res$model_id,
      engine          = res$engine,
      eval_set        = "spatial_cv",
      response_kind   = ti$response_kind %||% "raw_target",
      response_transform = ti$transform %||% "identity",
      n_cv_folds      = res$n_folds %||% NA_integer_,
      n_cv_obs        = res$n_obs   %||% NA_integer_,
      train_countries = train_ctry_str,
      cv_rmse         = res$overall_metrics$rmse %||% NA_real_,
      cv_mae          = res$overall_metrics$mae  %||% NA_real_,
      cv_rsq          = res$overall_metrics$rsq  %||% NA_real_,
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

#' Select the best model based on a metric
#'
#' @param cv_results_list List of cv result objects.
#' @param metric Character metric name ("rmse", "mae", "rsq").
#' @return The best cv result object.
select_best_model <- function(cv_results_list, metric = "rmse") {
  if (length(cv_results_list) == 0) {
    stop("No CV results to compare", call. = FALSE)
  }

  scores <- vapply(cv_results_list, function(res) {
    res$overall_metrics[[metric]] %||% NA_real_
  }, numeric(1))

  # For rmse/mae: lower is better. For rsq: higher is better.
  if (metric %in% c("rmse", "mae")) {
    best_idx <- which.min(scores)
  } else {
    best_idx <- which.max(scores)
  }

  if (length(best_idx) == 0) {
    best_idx <- 1L
  }

  cv_results_list[[best_idx]]
}

#' Build a per-fold summary data.frame
#'
#' Metric columns are named \code{fold_rmse/mae/rsq} and a \code{split_type}
#' column records that these are held-out spatial CV validation folds
#' (not training, not the final holdout test country).
#'
#' @param cv_result Single cv result from run_spatial_cv.
#' @return data.frame with fold-level metrics.
fold_summary <- function(cv_result) {
  rows <- lapply(cv_result$fold_results, function(fr) {
    data.frame(
      model_id   = cv_result$model_id,
      split_type = "spatial_cv_validation",
      fold       = fr$fold,
      n_train    = fr$n_train,
      n_test     = fr$n_test,
      fold_rmse  = fr$metrics$rmse %||% NA_real_,
      fold_mae   = fr$metrics$mae  %||% NA_real_,
      fold_rsq   = fr$metrics$rsq  %||% NA_real_,
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

#' Save CV results to disk
#'
#' Writes summary and fold-level CSV files and the final model RDS.
#' Column names in the CSV files are prefixed so the data split is
#' unambiguous: \code{cv_*} for cross-validation metrics on the training
#' countries, \code{fold_*} for individual fold metrics.
#'
#' @param cv_results_list List of cv result objects.
#' @param output_dir Directory to write results.
#' @param root_dir Project root.
#' @param train_countries Optional character vector of training country codes.
#' @return Character vector of written file paths.
save_cv_results <- function(cv_results_list, output_dir = "models", root_dir = ".",
                            train_countries = NULL) {
  out_dir <- file.path(root_dir, output_dir)
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  files_written <- character(0)

  # Overall CV summary (metrics are prefixed cv_)
  summary_df <- summarize_cv_results(cv_results_list, train_countries = train_countries)
  summary_path <- file.path(out_dir, "cv_summary.csv")
  utils::write.csv(summary_df, summary_path, row.names = FALSE)
  files_written <- c(files_written, summary_path)

  # Per-model fold details and final model

  for (res in cv_results_list) {
    fold_df <- fold_summary(res)
    fold_path <- file.path(out_dir, paste0(res$model_id, "_folds.csv"))
    utils::write.csv(fold_df, fold_path, row.names = FALSE)
    files_written <- c(files_written, fold_path)

    model_path <- file.path(out_dir, paste0(res$model_id, "_final.rds"))
    saveRDS(res$final_model, model_path)
    files_written <- c(files_written, model_path)

    vi_path <- save_variable_importance_plot(
      cv_result = res,
      output_dir = output_dir,
      root_dir = root_dir
    )
    if (!is.na(vi_path)) {
      files_written <- c(files_written, vi_path)
    }
  }

  files_written
}

#' Evaluate the best model on the held-out test country
#'
#' Uses the final model (trained on all non-holdout data) to predict on
#' the holdout panel and computes RMSE, MAE, and R².
#'
#' @param best_cv_result Best model object from select_best_model.
#' @param holdout_panel_sf sf object for the holdout country.
#' @param ml_cfg Parsed ML config (for target_variable).
#' @param feature_registry List of enabled feature entries.
#' @return Named list with country, model_id, n, and metrics sub-list.
evaluate_holdout <- function(best_cv_result, holdout_panel_sf, ml_cfg,
                             feature_registry) {
  target_var <- ml_cfg$ml$target_variable %||% "population"
  target_info <- best_cv_result$target_info %||% default_target_info(target_var)
  # Use the exact feature names the final model was trained on (includes log_area, year, etc.)
  feature_names <- best_cv_result$feature_names
  holdout_country <- ml_cfg$ml$holdout_test_country %||% "holdout"

  df <- sf::st_drop_geometry(holdout_panel_sf)

  # Expand year into per-decade dummy columns to match the training feature matrix
  if ("year" %in% names(df)) {
    year_dummy_feats <- grep("^year_\\d+$", feature_names, value = TRUE)
    for (yn in year_dummy_feats) {
      lvl        <- sub("^year_", "", yn)
      df[[yn]]   <- as.integer(as.character(df$year) == lvl)
    }
  }

  missing_feat <- setdiff(feature_names, names(df))
  if (length(missing_feat) > 0) {
    stop(sprintf("Holdout panel missing feature columns: %s",
                 paste(missing_feat, collapse = ", ")), call. = FALSE)
  }

  target_vals <- target_response_from_panel(holdout_panel_sf, target_info)
  y_model <- target_vals$y_model
  y_raw <- target_vals$y_raw
  X <- df[, feature_names, drop = FALSE]
  complete <- complete.cases(cbind(y_model, X))

  metrics <- if (sum(complete) < 1L) {
    list(rmse = NA_real_, mae = NA_real_, rsq = NA_real_)
  } else {
    preds_model <- predict(best_cv_result$final_model,
                           new_data = X[complete, , drop = FALSE])$.pred
    area_m2 <- if (isTRUE(target_info$needs_area)) {
      as.numeric(sf::st_area(holdout_panel_sf))[complete]
    } else NULL
    preds_count <- inverse_target_response(preds_model, target_info, area_m2 = area_m2)
    compute_metrics(y_raw[complete], preds_count)
  }

  list(
    country  = holdout_country,
    model_id = best_cv_result$model_id,
    engine   = best_cv_result$engine,
    eval_set = "test_holdout",
    response_kind = "population_count",
    response_transform = "inverse_to_count",
    n        = sum(complete),
    metrics  = metrics
  )
}

#' Write a long-format combined model summary CSV
#'
#' Produces \file{models/model_summary.csv} with one row per model per
#' evaluation split.  The \code{eval_set} column distinguishes:
#' \itemize{
#'   \item \code{"spatial_cv"} — metrics are averages over held-out
#'     spatial CV folds on the training countries.
#'   \item \code{"test_holdout"} — metrics on the held-out test country
#'     (never seen during CV or final-model training).
#' }
#'
#' @param cv_results_list List of cv result objects.
#' @param holdout_metrics_list Return value of evaluate_holdout.
#' @param train_countries Character vector of training country codes.
#' @param output_dir Output directory.
#' @param root_dir Project root.
#' @return Character path of written file.
write_model_summary <- function(cv_results_list, holdout_metrics_list,
                                train_countries = NULL,
                                output_dir = "models", root_dir = ".") {
  out_dir <- file.path(root_dir, output_dir)
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  train_ctry_str <- if (!is.null(train_countries) && length(train_countries) > 0) {
    paste(sort(unique(train_countries)), collapse = "+")
  } else NA_character_

  # One row per model for the CV split
  cv_rows <- lapply(cv_results_list, function(res) {
    ti <- res$target_info %||% default_target_info()
    data.frame(
      model_id   = res$model_id,
      engine     = res$engine,
      eval_set   = "spatial_cv",
      response_kind = ti$response_kind %||% "raw_target",
      response_transform = ti$transform %||% "identity",
      countries  = train_ctry_str,
      n_folds    = res$n_folds %||% NA_integer_,
      n_obs      = res$n_obs   %||% NA_integer_,
      rmse       = res$overall_metrics$rmse %||% NA_real_,
      mae        = res$overall_metrics$mae  %||% NA_real_,
      rsq        = res$overall_metrics$rsq  %||% NA_real_,
      stringsAsFactors = FALSE
    )
  })

  # One row for the holdout test split (best model only)
  hm <- holdout_metrics_list
  holdout_row <- data.frame(
    model_id   = hm$model_id,
    engine     = hm$engine %||% NA_character_,
    eval_set   = "test_holdout",
    response_kind = hm$response_kind %||% "population_count",
    response_transform = hm$response_transform %||% "inverse_to_count",
    countries  = hm$country,
    n_folds    = NA_integer_,
    n_obs      = hm$n,
    rmse       = hm$metrics$rmse %||% NA_real_,
    mae        = hm$metrics$mae  %||% NA_real_,
    rsq        = hm$metrics$rsq  %||% NA_real_,
    stringsAsFactors = FALSE
  )

  combined <- rbind(do.call(rbind, cv_rows), holdout_row)
  out_path <- file.path(out_dir, "model_summary.csv")
  utils::write.csv(combined, out_path, row.names = FALSE)
  out_path
}
