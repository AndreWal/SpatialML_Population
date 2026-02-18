source(file.path("..", "..", "R", "feature_extraction.R"))

test_that("load_feature_registry returns enabled features", {
  tmp <- tempdir()
  dir.create(file.path(tmp, "config", "sources"), recursive = TRUE, showWarnings = FALSE)

  writeLines(
    '
features_registry:
  - id: "elevation_mean"
    type: "raster_zonal"
    source_config: "config/sources/elevation.yml"
    enabled: true
  - id: "disabled_feature"
    type: "raster_zonal"
    source_config: "config/sources/disabled.yml"
    enabled: false
',
    file.path(tmp, "config", "sources", "features.yml")
  )

  reg <- load_feature_registry(root_dir = tmp)
  expect_length(reg, 1)
  expect_equal(reg[[1]]$id, "elevation_mean")
})

test_that("make_mock_raster creates deterministic raster", {
  pts <- sf::st_sfc(
    sf::st_point(c(8.5, 47.3)),
    sf::st_point(c(8.6, 47.4)),
    crs = 4326
  )
  panel <- sf::st_sf(id = 1:2, geometry = pts)

  r1 <- make_mock_raster(panel, feature_id = "elev", canonical_crs = "EPSG:3035")
  r2 <- make_mock_raster(panel, feature_id = "elev", canonical_crs = "EPSG:3035")

  expect_s4_class(r1, "SpatRaster")
  expect_equal(terra::values(r1), terra::values(r2))
  expect_equal(names(r1), "elev")
})

test_that("extract_zonal_stat works with points", {
  pts <- sf::st_sfc(
    sf::st_point(c(0, 0)),
    sf::st_point(c(1000, 1000)),
    crs = "EPSG:3035"
  )
  panel <- sf::st_sf(id = 1:2, geometry = pts)

  r <- make_mock_raster(panel, feature_id = "test_feat", canonical_crs = "EPSG:3035")
  vals <- extract_zonal_stat(r, panel, stat = "mean", feature_col = "test_feat")

  expect_length(vals, 2)
  expect_true(is.numeric(vals))
})

test_that("extract_and_join_features adds feature columns", {
  pts <- sf::st_sfc(
    sf::st_point(c(0, 0)),
    sf::st_point(c(5000, 5000)),
    crs = "EPSG:3035"
  )
  panel <- sf::st_sf(
    country_code = c("TST", "TST"),
    admin_unit_harmonized = c("A", "B"),
    year = c(2000L, 2000L),
    population = c(100, 200),
    geometry = pts
  )

  registry <- list(
    list(id = "mock_feat", source_config = "nonexistent.yml", enabled = TRUE)
  )

  result <- extract_and_join_features(
    panel_sf = panel,
    feature_registry = registry,
    canonical_crs = "EPSG:3035",
    root_dir = ".",
    mock_mode = TRUE
  )

  expect_true("mock_feat" %in% names(result))
  expect_equal(nrow(result), 2)
})

test_that("extract_and_join_features returns panel unchanged with empty registry", {
  pts <- sf::st_sfc(sf::st_point(c(0, 0)), crs = "EPSG:3035")
  panel <- sf::st_sf(id = 1, geometry = pts)

  result <- extract_and_join_features(panel, feature_registry = list(),
                                       canonical_crs = "EPSG:3035")
  expect_equal(nrow(result), 1)
  expect_true("id" %in% names(result))
})
