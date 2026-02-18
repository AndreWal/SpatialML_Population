`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

#' Check if MLflow is available
#'
#' @return Logical TRUE if the mlflow package is installed and loadable.
mlflow_available <- function() {
  requireNamespace("mlflow", quietly = TRUE)
}

#' Set up MLflow tracking
#'
#' Configures the tracking URI. Falls back to a local ./mlruns directory
#' if MLFLOW_TRACKING_URI is not set.
#'
#' @param tracking_uri URI for MLflow server (or NULL for env/local default).
#' @param experiment_name Name for the MLflow experiment.
#' @return Invisible NULL.
setup_mlflow <- function(tracking_uri = NULL, experiment_name = "spatial_ml") {
  if (!mlflow_available()) {
    message("MLflow not available; tracking disabled")
    return(invisible(NULL))
  }

  uri <- tracking_uri %||% Sys.getenv("MLFLOW_TRACKING_URI", "mlruns")
  mlflow::mlflow_set_tracking_uri(uri)

  tryCatch(
    mlflow::mlflow_set_experiment(experiment_name = experiment_name),
    error = function(e) {
      message(sprintf("MLflow experiment setup warning: %s", conditionMessage(e)))
    }
  )
  invisible(NULL)
}

#' Log a single model CV run to MLflow
#'
#' Logs parameters, metrics, and the final model artifact.
#'
#' @param cv_result A single result from run_spatial_cv.
#' @param ml_cfg Parsed ML config.
#' @param project_cfg Parsed project config.
#' @param model_dir Directory containing saved model files.
#' @return Invisible NULL (no-op if MLflow unavailable).
log_model_run <- function(cv_result, ml_cfg = list(), project_cfg = list(),
                          model_dir = "models") {

  if (!mlflow_available()) {
    return(invisible(NULL))
  }

  tryCatch({
    mlflow::mlflow_start_run()
    on.exit(tryCatch(mlflow::mlflow_end_run(), error = function(e) NULL), add = TRUE)

    # Parameters
    mlflow::mlflow_log_param("model_id", cv_result$model_id)
    mlflow::mlflow_log_param("engine", cv_result$engine)
    mlflow::mlflow_log_param("n_features", length(cv_result$feature_names))
    mlflow::mlflow_log_param("features", paste(cv_result$feature_names, collapse = ","))
    mlflow::mlflow_log_param("target", ml_cfg$ml$target_variable %||% "population")
    mlflow::mlflow_log_param("cv_folds", ml_cfg$ml$split$folds %||% 5)
    mlflow::mlflow_log_param("block_size_km", ml_cfg$ml$split$block_size_km %||% 100)
    mlflow::mlflow_log_param("seed", project_cfg$project$seed %||% 42)

    # Metrics
    metrics <- cv_result$overall_metrics
    if (!is.null(metrics$rmse) && !is.na(metrics$rmse)) {
      mlflow::mlflow_log_metric("rmse", metrics$rmse)
    }
    if (!is.null(metrics$mae) && !is.na(metrics$mae)) {
      mlflow::mlflow_log_metric("mae", metrics$mae)
    }
    if (!is.null(metrics$rsq) && !is.na(metrics$rsq)) {
      mlflow::mlflow_log_metric("rsq", metrics$rsq)
    }

    # Model artifact
    model_file <- file.path(model_dir, paste0(cv_result$model_id, "_final.rds"))
    if (file.exists(model_file)) {
      mlflow::mlflow_log_artifact(model_file)
    }

  }, error = function(e) {
    message(sprintf("MLflow logging failed (non-fatal): %s", conditionMessage(e)))
  })

  invisible(NULL)
}

#' Log all CV results to MLflow
#'
#' Convenience wrapper that calls log_model_run for each model result.
#'
#' @param cv_results_list List of cv result objects.
#' @param ml_cfg Parsed ML config.
#' @param project_cfg Parsed project config.
#' @param model_dir Directory containing saved model files.
#' @return Invisible TRUE.
log_all_model_runs <- function(cv_results_list, ml_cfg = list(),
                               project_cfg = list(), model_dir = "models") {
  if (!mlflow_available()) {
    message("MLflow not available; skipping logging")
    return(invisible(TRUE))
  }

  setup_mlflow(
    experiment_name = project_cfg$project$name %||% "spatial_ml"
  )

  for (res in cv_results_list) {
    log_model_run(
      cv_result = res,
      ml_cfg = ml_cfg,
      project_cfg = project_cfg,
      model_dir = model_dir
    )
  }

  invisible(TRUE)
}
