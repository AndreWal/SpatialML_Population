source(file.path("..", "..", "R", "model_evaluation.R"))
source(file.path("..", "..", "R", "spatial_cv.R"))
source(file.path("..", "..", "R", "model_training.R"))

test_that("compute_metrics returns rmse, mae, rsq", {
  obs <- c(1, 2, 3, 4, 5)
  pred <- c(1.1, 2.2, 2.8, 4.1, 5.0)

  m <- compute_metrics(obs, pred)

  expect_true(is.list(m))
  expect_true(all(c("rmse", "mae", "rsq") %in% names(m)))
  expect_gt(m$rmse, 0)
  expect_gt(m$mae, 0)
  expect_true(m$rsq > 0 && m$rsq <= 1)
})

test_that("compute_metrics perfect prediction", {
  m <- compute_metrics(c(1, 2, 3), c(1, 2, 3))
  expect_equal(m$rmse, 0)
  expect_equal(m$mae, 0)
  expect_equal(m$rsq, 1)
})

test_that("compute_metrics handles empty input", {
  m <- compute_metrics(numeric(0), numeric(0))
  expect_true(all(is.na(unlist(m))))
})

test_that("make_parsnip_spec supports ranger and xgboost and errors otherwise", {
  skip_if_not_installed("parsnip")
  suppressWarnings(skip_if_not_installed("tune"))
  expect_s3_class(make_parsnip_spec("ranger"), "model_spec")
  expect_s3_class(make_parsnip_spec("xgboost"), "model_spec")
  expect_error(make_parsnip_spec("unknown"), "Unsupported")
})

test_that("make_parsnip_spec supports lightgbm when dependencies are installed", {
  skip_if_not_installed("parsnip")
  suppressWarnings(skip_if_not_installed("tune"))
  skip_if_not_installed("bonsai")
  skip_if_not_installed("lightgbm")
  expect_s3_class(make_parsnip_spec("lightgbm"), "model_spec")
})

test_that("summarize_cv_results builds current schema", {
  results <- list(
    list(
      model_id = "rf", engine = "ranger",
      overall_metrics = list(rmse = 1.0, mae = 0.8, rsq = 0.9),
      target_info = list(response_kind = "population_density_per_m2", transform = "log1p")
    ),
    list(
      model_id = "xgb", engine = "xgboost",
      overall_metrics = list(rmse = 0.9, mae = 0.7, rsq = 0.92),
      target_info = list(response_kind = "population_density_per_m2", transform = "identity")
    )
  )

  summary <- summarize_cv_results(results)

  expect_equal(nrow(summary), 2)
  expect_true(all(c(
    "model_id", "engine", "eval_set", "response_kind", "response_transform",
    "cv_rmse", "cv_mae", "cv_rsq"
  ) %in% names(summary)))
})

test_that("select_best_model picks lowest rmse and highest rsq", {
  results <- list(
    list(model_id = "rf", engine = "ranger",
         overall_metrics = list(rmse = 1.0, mae = 0.8, rsq = 0.95)),
    list(model_id = "xgb", engine = "xgboost",
         overall_metrics = list(rmse = 0.5, mae = 0.4, rsq = 0.90))
  )

  expect_equal(select_best_model(results, metric = "rmse")$model_id, "xgb")
  expect_equal(select_best_model(results, metric = "rsq")$model_id, "rf")
})

test_that("run_spatial_cv produces results with untuned ranger", {
  skip_if_not_installed("ranger")
  skip_if_not_installed("tidymodels")
  skip_if_not_installed("spatialsample")

  set.seed(42)
  coords <- do.call(
    rbind,
    lapply(seq_len(20), function(i) {
      col <- (i - 1) %% 5
      row <- (i - 1) %/% 5
      c(col * 100000, row * 100000)
    })
  )
  pts <- sf::st_sfc(
    lapply(seq_len(nrow(coords)), function(i) sf::st_point(coords[i, ])),
    crs = "EPSG:3035"
  )
  panel <- sf::st_sf(
    population = seq(100, 290, by = 10),
    feat1 = rnorm(20),
    feat2 = rnorm(20),
    geometry = pts
  )
  ml_cfg <- list(ml = list(
    target_variable = "population",
    split = list(folds = 2, block_size_km = 200)
  ))
  registry <- list(list(id = "feat1"), list(id = "feat2"))
  model_data <- prepare_model_data(panel, ml_cfg, registry)
  folds <- create_spatial_resamples(panel, model_data, ml_cfg, seed = 42)

  spec <- list(id = "rf", engine = "ranger", tune = FALSE)
  result <- run_spatial_cv(spec, model_data, folds, ml_cfg, seed = 42)

  expect_equal(result$model_id, "rf")
  expect_equal(result$engine, "ranger")
  expect_true(!is.null(result$final_model))
  expect_true(is.list(result$overall_metrics))
  expect_true(all(c("rmse", "mae", "rsq") %in% names(result$overall_metrics)))
  expect_equal(result$target_info$response_kind, "population_density_per_m2")
})

test_that("save_variable_importance_plot writes a PNG for ranger model", {
  skip_if_not_installed("ranger")
  skip_if_not_installed("workflows")
  skip_if_not_installed("parsnip")

  set.seed(1)
  df <- data.frame(y = rnorm(40), x1 = rnorm(40), x2 = rnorm(40))
  wf <- workflows::workflow() |>
    workflows::add_formula(y ~ .) |>
    workflows::add_model(
      parsnip::rand_forest(trees = 50, mtry = 1, min_n = 2) |>
        parsnip::set_engine("ranger", importance = "impurity", num.threads = 1L) |>
        parsnip::set_mode("regression")
    )
  fit <- parsnip::fit(wf, data = df)

  cv_result <- list(
    model_id = "rf",
    engine = "ranger",
    final_model = fit
  )
  tmp <- tempdir()

  path <- save_variable_importance_plot(cv_result, output_dir = "models", root_dir = tmp)

  expect_true(file.exists(path))
  expect_match(path, "variable_importance/.+_var_importance\\.png$")
})
