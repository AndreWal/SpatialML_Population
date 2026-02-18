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

#' Summarize cross-validation results across models
#'
#' @param cv_results_list List of cv result objects (from run_spatial_cv).
#' @return data.frame with model_id and overall metrics.
summarize_cv_results <- function(cv_results_list) {
  rows <- lapply(cv_results_list, function(res) {
    data.frame(
      model_id = res$model_id,
      engine = res$engine,
      rmse = res$overall_metrics$rmse %||% NA_real_,
      mae = res$overall_metrics$mae %||% NA_real_,
      rsq = res$overall_metrics$rsq %||% NA_real_,
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
#' @param cv_result Single cv result from run_spatial_cv.
#' @return data.frame with fold-level metrics.
fold_summary <- function(cv_result) {
  rows <- lapply(cv_result$fold_results, function(fr) {
    data.frame(
      model_id = cv_result$model_id,
      fold = fr$fold,
      n_train = fr$n_train,
      n_test = fr$n_test,
      rmse = fr$metrics$rmse %||% NA_real_,
      mae = fr$metrics$mae %||% NA_real_,
      rsq = fr$metrics$rsq %||% NA_real_,
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

#' Save CV results to disk
#'
#' Writes summary and fold-level CSV files and the final model RDS.
#'
#' @param cv_results_list List of cv result objects.
#' @param output_dir Directory to write results.
#' @param root_dir Project root.
#' @return Character vector of written file paths.
save_cv_results <- function(cv_results_list, output_dir = "models", root_dir = ".") {
  out_dir <- file.path(root_dir, output_dir)
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  files_written <- character(0)

  # Overall summary
  summary_df <- summarize_cv_results(cv_results_list)
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
  }

  files_written
}
