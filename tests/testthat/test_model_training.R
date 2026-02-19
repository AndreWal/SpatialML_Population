source(file.path("..", "..", "R", "model_evaluation.R"))
source(file.path("..", "..", "R", "spatial_cv.R"))
source(file.path("..", "..", "R", "model_training.R"))

test_that("compute_metrics returns rmse, mae, rsq", {
  obs <- c(1, 2, 3, 4, 5)
  pred <- c(1.1, 2.2, 2.8, 4.1, 5.0)

  m <- compute_metrics(obs, pred)

  expect_true(is.list(m))
  expect_true(all(c("rmse", "mae", "rsq") %in% names(m)))
  expect_true(m$rmse > 0)
  expect_true(m$mae > 0)
  expect_true(m$rsq > 0 && m$rsq <= 1)
})

test_that("compute_metrics perfect prediction", {
  obs <- c(1, 2, 3)
  pred <- c(1, 2, 3)

  m <- compute_metrics(obs, pred)

  expect_equal(m$rmse, 0)
  expect_equal(m$mae, 0)
  expect_equal(m$rsq, 1)
})

test_that("compute_metrics handles empty input", {
  m <- compute_metrics(numeric(0), numeric(0))

  expect_true(is.na(m$rmse))
  expect_true(is.na(m$mae))
  expect_true(is.na(m$rsq))
})

test_that("train_ranger produces predictions", {
  set.seed(42)
  X <- data.frame(x1 = rnorm(50), x2 = rnorm(50))
  y <- X$x1 * 2 + X$x2 + rnorm(50, sd = 0.1)

  model <- train_ranger(X, y, seed = 42)
  preds <- predict_model(model, X)

  expect_length(preds, 50)
  expect_true(cor(y, preds) > 0.5)
})

test_that("train_xgboost produces predictions", {
  set.seed(42)
  X <- data.frame(x1 = rnorm(50), x2 = rnorm(50))
  y <- X$x1 * 2 + X$x2 + rnorm(50, sd = 0.1)

  model <- train_xgboost(X, y, seed = 42)
  preds <- predict_model(model, X)

  expect_length(preds, 50)
  expect_true(cor(y, preds) > 0.5)
})

test_that("train_model dispatches correctly", {
  X <- data.frame(x = 1:20)
  y <- (1:20) * 2

  rf <- train_model("ranger", X, y)
  xgb <- train_model("xgboost", X, y)

  expect_true(inherits(rf, "ranger"))
  expect_true(inherits(xgb, "xgb.Booster"))
})

test_that("train_catboost produces predictions", {
  skip_if_not_installed("catboost")
  set.seed(42)
  X <- data.frame(x1 = rnorm(50), x2 = rnorm(50))
  y <- X$x1 * 2 + X$x2 + rnorm(50, sd = 0.1)

  model <- train_catboost(X, y, seed = 42)
  preds <- predict_model(model, X)

  expect_length(preds, 50)
  expect_true(cor(y, preds) > 0.5)
})

test_that("train_model dispatches catboost correctly", {
  skip_if_not_installed("catboost")
  X <- data.frame(x = 1:20)
  y <- (1:20) * 2

  cat <- train_model("catboost", X, y)
  expect_true(inherits(cat, "catboost.Model"))
})

test_that("train_model errors on unsupported engine", {
  expect_error(train_model("unknown_engine", data.frame(x = 1), 1), "Unsupported")
})

test_that("summarize_cv_results builds data.frame", {
  results <- list(
    list(model_id = "rf", engine = "ranger",
         overall_metrics = list(rmse = 1.0, mae = 0.8, rsq = 0.9)),
    list(model_id = "xgb", engine = "xgboost",
         overall_metrics = list(rmse = 0.9, mae = 0.7, rsq = 0.92))
  )

  summary <- summarize_cv_results(results)

  expect_equal(nrow(summary), 2)
  expect_true(all(c("model_id", "rmse", "mae", "rsq") %in% names(summary)))
})

test_that("select_best_model picks lowest rmse", {
  results <- list(
    list(model_id = "rf", engine = "ranger",
         overall_metrics = list(rmse = 1.0, mae = 0.8, rsq = 0.9)),
    list(model_id = "xgb", engine = "xgboost",
         overall_metrics = list(rmse = 0.5, mae = 0.4, rsq = 0.95))
  )

  best <- select_best_model(results, metric = "rmse")
  expect_equal(best$model_id, "xgb")
})

test_that("select_best_model picks highest rsq", {
  results <- list(
    list(model_id = "rf", engine = "ranger",
         overall_metrics = list(rmse = 1.0, mae = 0.8, rsq = 0.95)),
    list(model_id = "xgb", engine = "xgboost",
         overall_metrics = list(rmse = 0.5, mae = 0.4, rsq = 0.90))
  )

  best <- select_best_model(results, metric = "rsq")
  expect_equal(best$model_id, "rf")
})

test_that("run_spatial_cv produces results", {
  set.seed(42)
  pts <- sf::st_sfc(
    lapply(1:20, function(i) sf::st_point(c(i * 100000, 0))),
    crs = "EPSG:3035"
  )
  panel <- sf::st_sf(
    population = rnorm(20, mean = 100, sd = 20),
    feat1 = rnorm(20),
    geometry = pts
  )
  ml_cfg <- list(ml = list(
    target_variable = "population",
    split = list(folds = 2, block_size_km = 200)
  ))
  registry <- list(list(id = "feat1"))
  model_data <- prepare_model_data(panel, ml_cfg, registry)
  folds <- create_spatial_folds(panel, ml_cfg, seed = 42)

  spec <- list(id = "rf", engine = "ranger")
  result <- run_spatial_cv(spec, model_data, folds, ml_cfg, seed = 42)

  expect_equal(result$model_id, "rf")
  expect_true(!is.null(result$final_model))
  expect_true(!is.null(result$overall_metrics))
})
