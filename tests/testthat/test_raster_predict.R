source(file.path("..", "..", "R", "feature_extraction.R"))
source(file.path("..", "..", "R", "model_evaluation.R"))
source(file.path("..", "..", "R", "model_training.R"))
source(file.path("..", "..", "R", "raster_predict.R"))

test_that("create_prediction_grid creates valid raster", {
  pts <- sf::st_sfc(
    sf::st_point(c(0, 0)),
    sf::st_point(c(50000, 50000)),
    crs = "EPSG:3035"
  )
  panel <- sf::st_sf(id = 1:2, geometry = pts)

  grid <- create_prediction_grid(panel, resolution_m = 1000, canonical_crs = "EPSG:3035")

  expect_s4_class(grid, "SpatRaster")
  expect_true(terra::ncell(grid) > 0)
})

test_that("predict_to_raster produces predictions with ranger", {
  set.seed(42)
  X <- data.frame(feat1 = rnorm(30))
  y <- X$feat1 * 2 + 10

  model <- train_ranger(X, y, seed = 42)

  # Create a small feature raster
  r <- terra::rast(nrows = 5, ncols = 5,
                   xmin = 0, xmax = 5000, ymin = 0, ymax = 5000,
                   crs = "EPSG:3035")
  terra::values(r) <- rnorm(25)
  names(r) <- "feat1"

  pred <- predict_to_raster(model, r, feature_names = "feat1",
                            clamp = TRUE, train_y = y)

  expect_s4_class(pred, "SpatRaster")
  expect_equal(names(pred), "prediction")

  # Clamped values should be within training range
  vals <- terra::values(pred)
  vals <- vals[!is.na(vals)]
  expect_true(all(vals >= min(y) - 1e-6))
  expect_true(all(vals <= max(y) + 1e-6))
})

test_that("predict_to_raster works with xgboost", {
  set.seed(42)
  X <- data.frame(feat1 = rnorm(30))
  y <- X$feat1 * 2 + 10

  model <- train_xgboost(X, y, seed = 42)

  r <- terra::rast(nrows = 5, ncols = 5,
                   xmin = 0, xmax = 5000, ymin = 0, ymax = 5000,
                   crs = "EPSG:3035")
  terra::values(r) <- rnorm(25)
  names(r) <- "feat1"

  pred <- predict_to_raster(model, r, feature_names = "feat1",
                            clamp = FALSE)

  expect_s4_class(pred, "SpatRaster")
  expect_true(all(!is.na(terra::values(pred))))
})

test_that("write_prediction_raster writes GeoTIFF", {
  tmp <- tempdir()
  r <- terra::rast(nrows = 3, ncols = 3,
                   xmin = 0, xmax = 3000, ymin = 0, ymax = 3000,
                   crs = "EPSG:3035")
  terra::values(r) <- 1:9
  names(r) <- "prediction"

  path <- write_prediction_raster(r, "TST", "rf",
                                  output_dir = "output", root_dir = tmp)

  expect_true(file.exists(path))
  expect_true(grepl("\\.tif$", path))

  # Read back and verify
  r2 <- terra::rast(path)
  expect_equal(terra::ncell(r2), 9)

  unlink(file.path(tmp, "output"), recursive = TRUE)
})
