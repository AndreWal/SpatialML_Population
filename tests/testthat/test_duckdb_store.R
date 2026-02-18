source(file.path("..", "..", "R", "duckdb_store.R"))

test_that("open/close duckdb works in memory", {
  con <- open_duckdb(":memory:")
  expect_true(DBI::dbIsValid(con))
  close_duckdb(con)
})

test_that("write_to_duckdb and read_from_duckdb roundtrip", {
  con <- open_duckdb(":memory:")
  on.exit(close_duckdb(con))

  df <- data.frame(
    country_code = c("TST", "TST"),
    year = c(2000L, 2010L),
    population = c(100, 200),
    stringsAsFactors = FALSE
  )

  write_to_duckdb(con, df, "test_panel")
  result <- read_from_duckdb(con, "test_panel")

  expect_equal(nrow(result), 2)
  expect_true(all(c("country_code", "year", "population") %in% names(result)))
})

test_that("write_to_duckdb drops geometry from sf", {
  pts <- sf::st_sfc(sf::st_point(c(0, 0)), sf::st_point(c(1, 1)), crs = 4326)
  sf_df <- sf::st_sf(id = 1:2, val = c(10, 20), geometry = pts)

  con <- open_duckdb(":memory:")
  on.exit(close_duckdb(con))

  write_to_duckdb(con, sf_df, "sf_test")
  result <- read_from_duckdb(con, "sf_test")

  expect_true("id" %in% names(result))
  expect_true("val" %in% names(result))
  expect_false("geometry" %in% names(result))
})

test_that("query_duckdb runs SQL", {
  con <- open_duckdb(":memory:")
  on.exit(close_duckdb(con))

  df <- data.frame(x = c(1, 2, 3), y = c(10, 20, 30))
  write_to_duckdb(con, df, "numbers")

  result <- query_duckdb(con, "SELECT SUM(y) AS total FROM numbers")
  expect_equal(result$total, 60)
})

test_that("export_to_parquet and import_parquet roundtrip", {
  con <- open_duckdb(":memory:")
  on.exit(close_duckdb(con))

  df <- data.frame(a = 1:5, b = letters[1:5], stringsAsFactors = FALSE)
  write_to_duckdb(con, df, "source_tbl")

  pq_path <- file.path(tempdir(), "test_export.parquet")
  export_to_parquet(con, "source_tbl", pq_path)
  expect_true(file.exists(pq_path))

  import_parquet(con, pq_path, "imported_tbl")
  result <- read_from_duckdb(con, "imported_tbl")
  expect_equal(nrow(result), 5)
  expect_equal(result$a, 1:5)

  unlink(pq_path)
})

test_that("store_panels_duckdb creates tables", {
  pts <- sf::st_sfc(
    sf::st_point(c(0, 0)),
    sf::st_point(c(1, 1)),
    crs = 4326
  )
  panels <- list(
    TST = sf::st_sf(
      country_code = c("TST", "TST"),
      year = c(2000L, 2010L),
      population = c(100, 200),
      geometry = pts
    )
  )

  tmp <- tempdir()
  db_file <- file.path("test_cache", "test.duckdb")
  tables <- store_panels_duckdb(panels, db_path = db_file, root_dir = tmp)

  expect_true("panel_tst" %in% tables)
  expect_true("panel_all" %in% tables)

  # Verify data is actually in the DB
  con <- open_duckdb(db_file, root_dir = tmp)
  result <- read_from_duckdb(con, "panel_all")
  close_duckdb(con)

  expect_equal(nrow(result), 2)

  unlink(file.path(tmp, "test_cache"), recursive = TRUE)
})
