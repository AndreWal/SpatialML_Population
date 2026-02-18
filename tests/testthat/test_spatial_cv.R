source(file.path("..", "..", "R", "spatial_cv.R"))

test_that("create_spatial_folds assigns all observations to folds", {
  pts <- sf::st_sfc(
    sf::st_point(c(0, 0)),
    sf::st_point(c(100000, 0)),
    sf::st_point(c(200000, 0)),
    sf::st_point(c(300000, 0)),
    sf::st_point(c(400000, 0)),
    crs = "EPSG:3035"
  )
  panel <- sf::st_sf(id = 1:5, geometry = pts)

  ml_cfg <- list(ml = list(split = list(folds = 3, block_size_km = 100)))
  folds <- create_spatial_folds(panel, ml_cfg, seed = 42)

  expect_length(folds, 5)
  expect_true(all(folds %in% 1:3))
})

test_that("create_spatial_folds is deterministic", {
  pts <- sf::st_sfc(
    sf::st_point(c(0, 0)),
    sf::st_point(c(200000, 200000)),
    sf::st_point(c(400000, 400000)),
    crs = "EPSG:3035"
  )
  panel <- sf::st_sf(id = 1:3, geometry = pts)

  ml_cfg <- list(ml = list(split = list(folds = 2, block_size_km = 100)))
  f1 <- create_spatial_folds(panel, ml_cfg, seed = 123)
  f2 <- create_spatial_folds(panel, ml_cfg, seed = 123)

  expect_identical(f1, f2)
})

test_that("prepare_model_data extracts target and features", {
  pts <- sf::st_sfc(
    sf::st_point(c(0, 0)),
    sf::st_point(c(1000, 1000)),
    crs = "EPSG:3035"
  )
  panel <- sf::st_sf(
    population = c(100, 200),
    elevation_mean = c(500, 800),
    geometry = pts
  )

  ml_cfg <- list(ml = list(target_variable = "population"))
  registry <- list(list(id = "elevation_mean"))

  md <- prepare_model_data(panel, ml_cfg, registry)

  expect_equal(md$y, c(100, 200))
  expect_equal(md$X$elevation_mean, c(500, 800))
  expect_equal(md$feature_names, "elevation_mean")
  expect_equal(md$complete_idx, 1:2)
})

test_that("prepare_model_data handles missing values", {
  pts <- sf::st_sfc(
    sf::st_point(c(0, 0)),
    sf::st_point(c(1000, 1000)),
    sf::st_point(c(2000, 2000)),
    crs = "EPSG:3035"
  )
  panel <- sf::st_sf(
    population = c(100, NA, 300),
    feat = c(10, 20, 30),
    geometry = pts
  )

  ml_cfg <- list(ml = list(target_variable = "population"))
  registry <- list(list(id = "feat"))

  md <- prepare_model_data(panel, ml_cfg, registry)

  expect_length(md$y, 2)
  expect_equal(md$complete_idx, c(1L, 3L))
})

test_that("split_by_fold produces correct train/test sets", {
  model_data <- list(
    y = c(1, 2, 3, 4),
    X = data.frame(x = c(10, 20, 30, 40)),
    feature_names = "x",
    complete_idx = 1:4
  )
  folds <- c(1, 1, 2, 2)

  split <- split_by_fold(model_data, folds, fold_id = 1)
  expect_equal(split$train_y, c(3, 4))
  expect_equal(split$test_y, c(1, 2))
  expect_equal(split$test_idx, c(1L, 2L))
})
