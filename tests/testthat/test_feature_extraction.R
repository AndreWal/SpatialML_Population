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

test_that("extract_zonal_stat works with points", {
  pts <- sf::st_sfc(
    sf::st_point(c(0, 0)),
    sf::st_point(c(1000, 1000)),
    crs = "EPSG:3035"
  )
  panel <- sf::st_sf(id = 1:2, geometry = pts)

  # Create a simple raster covering the point extent
  r <- terra::rast(
    terra::ext(-5000, 10000, -5000, 10000),
    nrows = 3L, ncols = 3L,
    crs = "EPSG:3035"
  )
  terra::values(r) <- seq_len(terra::ncell(r)) * 10.0
  names(r) <- "test_feat"
  vals <- extract_zonal_stat(r, panel, stat = "mean", feature_col = "test_feat")

  expect_length(vals, 2)
  expect_true(is.numeric(vals))
})

test_that("extract_and_join_features errors on missing source_config", {
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

  expect_error(
    extract_and_join_features(
      panel_sf = panel,
      feature_registry = registry,
      canonical_crs = "EPSG:3035",
      root_dir = "."
    ),
    "not found"
  )
})

test_that("extract_and_join_features returns panel unchanged with empty registry", {
  pts <- sf::st_sfc(sf::st_point(c(0, 0)), crs = "EPSG:3035")
  panel <- sf::st_sf(id = 1, geometry = pts)

  result <- extract_and_join_features(panel, feature_registry = list(),
                                       canonical_crs = "EPSG:3035")
  expect_equal(nrow(result), 1)
  expect_true("id" %in% names(result))
})
