source(file.path("..", "..", "R", "spatial_cv.R"))

make_square_sf <- function(n, cell = 1000, crs = "EPSG:3035") {
  geoms <- lapply(seq_len(n), function(i) {
    x0 <- (i - 1) * (cell * 2)
    y0 <- 0
    sf::st_polygon(list(rbind(
      c(x0, y0),
      c(x0 + cell, y0),
      c(x0 + cell, y0 + cell),
      c(x0, y0 + cell),
      c(x0, y0)
    )))
  })
  sf::st_sfc(geoms, crs = crs)
}

test_that("prepare_model_data models population as density and stores metadata", {
  pts <- make_square_sf(2, cell = 1000)
  panel <- sf::st_sf(
    population = c(100, 200),
    elevation_mean = c(500, 800),
    geometry = pts
  )

  ml_cfg <- list(ml = list(target_variable = "population"))
  registry <- list(list(id = "elevation_mean"))

  md <- prepare_model_data(panel, ml_cfg, registry)

  areas <- as.numeric(sf::st_area(panel))
  expect_equal(md$y, panel$population / areas)
  expect_equal(md$X$elevation_mean, c(500, 800))
  expect_equal(md$feature_names, "elevation_mean")
  expect_equal(md$complete_idx, 1:2)
  expect_equal(md$target_info$response_kind, "population_density_per_m2")
  expect_true(md$target_info$transform %in% c("identity", "log1p"))
  expect_equal(md$target_raw, c(100, 200))
})

test_that("prepare_model_data applies log transform when density is skewed", {
  pts <- make_square_sf(8, cell = 1000)
  panel <- sf::st_sf(
    population = c(1, 1, 2, 2, 3, 4, 10, 5000),
    feat = seq_len(8),
    geometry = pts
  )
  registry <- list(list(id = "feat"))
  ml_cfg <- list(ml = list(target_variable = "population"))

  md <- prepare_model_data(panel, ml_cfg, registry)

  expect_equal(md$target_info$response_kind, "population_density_per_m2")
  expect_equal(md$target_info$transform, "log1p")

  # Inverse transform should recover population counts when area is provided.
  recovered <- inverse_target_response(
    md$y,
    md$target_info,
    area_m2 = as.numeric(sf::st_area(panel))[md$complete_idx]
  )
  expect_equal(round(recovered, 8), round(md$target_raw, 8))
})

test_that("prepare_model_data handles missing values after target transform", {
  pts <- make_square_sf(3, cell = 1000)
  panel <- sf::st_sf(
    population = c(100, NA, 300),
    feat = c(10, 20, 30),
    geometry = pts
  )

  md <- prepare_model_data(panel, list(ml = list(target_variable = "population")),
                           list(list(id = "feat")))

  expect_length(md$y, 2)
  expect_equal(md$complete_idx, c(1L, 3L))
})

test_that("prepare_model_data can exclude polygon-only derived features", {
  pts <- make_square_sf(2, cell = 1000)
  panel <- sf::st_sf(
    population = c(100, 200),
    elevation_mean = c(5, 10),
    log_area = c(1, 2),
    lon = c(8, 9),
    lat = c(47, 48),
    geometry = pts
  )
  ml_cfg <- list(ml = list(target_variable = "population"))
  registry <- list(list(id = "elevation_mean"))

  md <- prepare_model_data(
    panel, ml_cfg, registry,
    include_polygon_only_features = FALSE
  )

  expect_false("log_area" %in% md$feature_names)
  expect_true(all(c("lon", "lat", "elevation_mean") %in% md$feature_names))
})

test_that("create_spatial_resamples returns manual_rset indexed to model data", {
  skip_if_not_installed("spatialsample")
  skip_if_not_installed("rsample")

  pts <- make_square_sf(10, cell = 50000)
  panel <- sf::st_sf(
    population = seq(100, 1000, by = 100),
    feat1 = rnorm(10),
    geometry = pts
  )
  ml_cfg <- list(ml = list(split = list(folds = 3), target_variable = "population"))
  md <- prepare_model_data(panel, ml_cfg, list(list(id = "feat1")))

  rs <- create_spatial_resamples(panel, md, ml_cfg, seed = 42)

  expect_true(inherits(rs, "manual_rset"))
  expect_equal(length(rs$splits), 3)

  total_n <- nrow(cbind(data.frame(.outcome = md$y), md$X))
  assessment_n <- sum(vapply(rs$splits, function(sp) nrow(rsample::assessment(sp)), integer(1)))
  expect_equal(assessment_n, total_n)
})
