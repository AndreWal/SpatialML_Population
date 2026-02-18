source(file.path("..", "..", "R", "mlflow_utils.R"))

test_that("mlflow_available returns logical", {
  result <- mlflow_available()
  expect_true(is.logical(result))
})

test_that("log_all_model_runs gracefully handles missing mlflow", {
  # Should not error even if mlflow is not available or not configured
  results <- list(
    list(
      model_id = "rf",
      engine = "ranger",
      feature_names = "feat1",
      overall_metrics = list(rmse = 1.0, mae = 0.8, rsq = 0.9),
      final_model = NULL
    )
  )

  # This should not error — it either logs or silently skips
  expect_no_error(
    suppressMessages(
      log_all_model_runs(
        cv_results_list = results,
        ml_cfg = list(ml = list(target_variable = "pop")),
        project_cfg = list(project = list(name = "test", seed = 42))
      )
    )
  )
})
