source(file.path("..", "..", "R", "config_loader.R"))
source(file.path("..", "..", "R", "io_readers.R"))
source(file.path("..", "..", "R", "assembly.R"))
source(file.path("..", "..", "R", "pipeline_country.R"))

testthat::test_that("read_country_inputs falls back to mock data", {
  cfg <- normalize_country_config(
    list(
      country = list(iso3 = "TST", enabled = TRUE),
      inputs = list(
        tabular = list(
          path = "data/raw/TST/missing.csv",
          format = "csv",
          columns = list(admin_id_raw = "id", admin_name = "name", year = "year", population = "pop")
        ),
        geometry = list(
          path = "data/raw/TST/missing.geojson",
          format = "geojson",
          columns = list(admin_id_raw = "id", admin_name = "name")
        )
      )
    ),
    file_country = "TST"
  )

  panel <- read_country_inputs(cfg, root_dir = tempdir(), mock_mode = TRUE)
  testthat::expect_s3_class(panel, "sf")
  testthat::expect_true(nrow(panel) > 0)
  testthat::expect_true(all(c("admin_id_raw", "year", "population") %in% names(panel)))
})

testthat::test_that("harmonize_keys applies crosswalk and aggregates many-to-one", {
  root <- file.path(tempdir(), paste0("pipe_", as.integer(stats::runif(1, 1, 1e9))))
  dir.create(file.path(root, "config", "crosswalks"), recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(
    data.frame(
      from_admin_id = c("1", "2"),
      to_admin_id = c("10", "10"),
      to_admin_name = c("Merged", "Merged")
    ),
    file.path(root, "config", "crosswalks", "TST.csv"),
    row.names = FALSE
  )

  panel <- sf::st_sf(
    country_code = c("TST", "TST"),
    admin_id_raw = c("1", "2"),
    admin_name = c("A", "B"),
    year = c(2000L, 2000L),
    population = c(100, 200),
    geometry = sf::st_sfc(sf::st_point(c(8.5, 47.3)), sf::st_point(c(8.6, 47.4)), crs = 4326)
  )

  cfg <- normalize_country_config(
    list(
      country = list(iso3 = "TST"),
      inputs = list(tabular = list(), geometry = list()),
      harmonization = list(crosswalk_file = "config/crosswalks/TST.csv"),
      assemblies = list()
    ),
    "TST"
  )

  out <- harmonize_keys(panel, cfg, root_dir = root, mock_mode = FALSE)
  testthat::expect_s3_class(out, "sf")
  testthat::expect_equal(nrow(out), 1)
  testthat::expect_equal(out$admin_unit_harmonized[[1]], "10")
  testthat::expect_equal(out$population[[1]], 300)
})

testthat::test_that("transform_to_canonical_crs transforms CRS", {
  panel <- sf::st_sf(
    country_code = "TST",
    admin_id_raw = "1",
    admin_name = "A",
    year = 2000L,
    population = 100,
    geometry = sf::st_sfc(sf::st_point(c(8.5, 47.3)), crs = 4326)
  )
  out <- transform_to_canonical_crs(panel, "EPSG:3035")
  testthat::expect_equal(sf::st_crs(out)$epsg, 3035)
})

testthat::test_that("harmonize_keys respects unmatched policy", {
  panel <- sf::st_sf(
    country_code = "TST",
    admin_id_raw = c("1", "2"),
    admin_name = c("A", "B"),
    year = c(2000L, 2000L),
    population = c(100, 200),
    geometry = sf::st_sfc(sf::st_point(c(8.5, 47.3)), sf::st_point(c(8.6, 47.4)), crs = 4326)
  )

  root <- file.path(tempdir(), paste0("pipe_", as.integer(stats::runif(1, 1, 1e9))))
  dir.create(file.path(root, "config", "crosswalks"), recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(
    data.frame(from_admin_id = "1", to_admin_id = "10", to_admin_name = "Merged"),
    file.path(root, "config", "crosswalks", "TST.csv"),
    row.names = FALSE
  )

  cfg_fail <- normalize_country_config(
    list(
      country = list(iso3 = "TST"),
      inputs = list(tabular = list(), geometry = list()),
      harmonization = list(crosswalk_file = "config/crosswalks/TST.csv", unmatched_policy = "fail"),
      assemblies = list()
    ),
    "TST"
  )
  testthat::expect_error(harmonize_keys(panel, cfg_fail, root_dir = root, mock_mode = FALSE), "unmatched")

  cfg_drop <- cfg_fail
  cfg_drop$harmonization$unmatched_policy <- "drop"
  dropped <- harmonize_keys(panel, cfg_drop, root_dir = root, mock_mode = FALSE)
  testthat::expect_equal(nrow(dropped), 1)
})

testthat::test_that("validate_country_panel_qa enforces unique keys", {
  qa_cfg <- list(qa = list(keys = list(unique_key = c("country_code", "admin_unit_harmonized", "year")), coverage = list(join_coverage_min = 0)))
  cfg <- list(country = list(iso3 = "TST"), qa_override = list(join_coverage_min = 0))
  panel <- sf::st_sf(
    country_code = c("TST", "TST"),
    admin_unit_harmonized = c("1", "1"),
    admin_name_harmonized = c("A", "A"),
    year = c(2000L, 2000L),
    population = c(100, 150),
    geometry = sf::st_sfc(sf::st_point(c(8.5, 47.3)), sf::st_point(c(8.6, 47.4)), crs = 3035)
  )
  attr(panel, "join_coverage") <- 1
  testthat::expect_error(
    validate_country_panel_qa(panel, panel, cfg, qa_cfg, "EPSG:3035"),
    "duplicate unique key"
  )
})

# ---- Geometry validation tests ----

testthat::test_that("validate_and_fix_geometry passes valid geometries through", {
  panel <- sf::st_sf(
    country_code = "TST",
    admin_unit_harmonized = "1",
    year = 2000L,
    population = 100,
    geometry = sf::st_sfc(
      sf::st_polygon(list(matrix(c(0,0, 1,0, 1,1, 0,1, 0,0), ncol = 2, byrow = TRUE))),
      crs = 3035
    )
  )
  qa <- list(qa = list(geometry = list(
    require_valid_geometry = TRUE,
    auto_fix_invalid = FALSE,
    allow_multipart = TRUE
  )))
  out <- validate_and_fix_geometry(panel, qa, "TST")
  testthat::expect_s3_class(out, "sf")
  testthat::expect_equal(nrow(out), 1)
})

testthat::test_that("validate_and_fix_geometry fixes invalid geometry with auto_fix", {
  # Bowtie polygon (self-intersecting) — invalid by OGC rules
  bowtie <- sf::st_polygon(list(matrix(
    c(0,0, 2,2, 2,0, 0,2, 0,0), ncol = 2, byrow = TRUE
  )))
  panel <- sf::st_sf(
    country_code = "TST",
    admin_unit_harmonized = "1",
    year = 2000L,
    population = 100,
    geometry = sf::st_sfc(bowtie, crs = 3035)
  )

  qa_fix <- list(qa = list(geometry = list(
    require_valid_geometry = TRUE,
    auto_fix_invalid = TRUE,
    allow_multipart = TRUE
  )))
  out <- validate_and_fix_geometry(panel, qa_fix, "TST")
  sf::sf_use_s2(FALSE)
  testthat::expect_true(all(sf::st_is_valid(out)))
  sf::sf_use_s2(TRUE)

  qa_no_fix <- list(qa = list(geometry = list(
    require_valid_geometry = TRUE,
    auto_fix_invalid = FALSE,
    allow_multipart = TRUE
  )))
  testthat::expect_error(
    validate_and_fix_geometry(panel, qa_no_fix, "TST"),
    "invalid geometries"
  )
})

testthat::test_that("validate_and_fix_geometry skips when require_valid is FALSE", {
  bowtie <- sf::st_polygon(list(matrix(
    c(0,0, 2,2, 2,0, 0,2, 0,0), ncol = 2, byrow = TRUE
  )))
  panel <- sf::st_sf(
    country_code = "TST",
    admin_unit_harmonized = "1",
    year = 2000L,
    population = 100,
    geometry = sf::st_sfc(bowtie, crs = 3035)
  )
  qa <- list(qa = list(geometry = list(require_valid_geometry = FALSE)))
  out <- validate_and_fix_geometry(panel, qa, "TST")
  testthat::expect_equal(nrow(out), 1)
})

testthat::test_that("validate_and_fix_geometry casts multipart when disallowed", {
  mp <- sf::st_multipolygon(list(
    list(matrix(c(0,0, 1,0, 1,1, 0,1, 0,0), ncol = 2, byrow = TRUE)),
    list(matrix(c(2,2, 3,2, 3,3, 2,3, 2,2), ncol = 2, byrow = TRUE))
  ))
  panel <- sf::st_sf(
    country_code = c("TST"),
    admin_unit_harmonized = c("1"),
    year = c(2000L),
    population = c(100),
    geometry = sf::st_sfc(mp, crs = 3035)
  )
  qa_no_multi <- list(qa = list(geometry = list(
    require_valid_geometry = TRUE,
    auto_fix_invalid = TRUE,
    allow_multipart = FALSE
  )))
  out <- validate_and_fix_geometry(panel, qa_no_multi, "TST")
  geom_types <- as.character(sf::st_geometry_type(out))
  testthat::expect_true(all(geom_types == "POLYGON"))
  testthat::expect_true(nrow(out) >= 2)
})
