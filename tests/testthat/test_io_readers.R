source(file.path("..", "..", "R", "io_readers.R"))

make_test_fixtures <- function() {
  root <- file.path(tempdir(), paste0("io_", as.integer(stats::runif(1, 1, 1e9))))
  dir.create(root, recursive = TRUE, showWarnings = FALSE)

  tab <- data.frame(
    admin_id_raw = c(1L, 2L),
    admin_name = c("A", "B"),
    year = c(2000L, 2010L),
    population = c(100, 200),
    stringsAsFactors = FALSE
  )

  csv_path <- file.path(root, "tab.csv")
  parquet_path <- file.path(root, "tab.parquet")
  xlsx_path <- file.path(root, "tab.xlsx")

  utils::write.csv(tab, csv_path, row.names = FALSE)
  arrow::write_parquet(tab, parquet_path)
  openxlsx::write.xlsx(tab, xlsx_path)

  geom <- sf::st_sf(
    id = c(1L, 2L),
    name = c("A", "B"),
    geometry = sf::st_sfc(
      sf::st_point(c(8.5, 47.3)),
      sf::st_point(c(8.6, 47.4)),
      crs = 4326
    )
  )

  geojson_path <- file.path(root, "geom.geojson")
  gpkg_path <- file.path(root, "geom.gpkg")
  shp_path <- file.path(root, "geom.shp")

  sf::st_write(geom, geojson_path, driver = "GeoJSON", quiet = TRUE, delete_dsn = TRUE)
  sf::st_write(geom, gpkg_path, layer = "geom", driver = "GPKG", quiet = TRUE, delete_dsn = TRUE)
  sf::st_write(geom, shp_path, driver = "ESRI Shapefile", quiet = TRUE, delete_layer = TRUE)

  list(
    root = root,
    csv = "tab.csv",
    parquet = "tab.parquet",
    xlsx = "tab.xlsx",
    geojson = "geom.geojson",
    gpkg = "geom.gpkg",
    shp = "geom.shp"
  )
}

testthat::test_that("read_tabular dispatches csv/parquet/xlsx", {
  f <- make_test_fixtures()

  csv_df <- read_tabular(
    list(path = f$csv, format = "csv"),
    root_dir = f$root,
    country = "TST",
    section = "inputs.tabular[1]"
  )
  pq_df <- read_tabular(
    list(path = f$parquet, format = "parquet"),
    root_dir = f$root,
    country = "TST",
    section = "inputs.tabular[2]"
  )
  xlsx_df <- read_tabular(
    list(path = f$xlsx, format = "xlsx"),
    root_dir = f$root,
    country = "TST",
    section = "inputs.tabular[3]"
  )

  testthat::expect_equal(nrow(csv_df), 2)
  testthat::expect_equal(nrow(pq_df), 2)
  testthat::expect_equal(nrow(xlsx_df), 2)
  testthat::expect_true(all(c("admin_id_raw", "admin_name", "year", "population") %in% names(csv_df)))
})

testthat::test_that("read_geometry dispatches shp/gpkg/geojson", {
  f <- make_test_fixtures()

  shp_sf <- read_geometry(
    list(path = f$shp, format = "shp"),
    root_dir = f$root,
    country = "TST",
    section = "inputs.geometry[1]"
  )
  gpkg_sf <- read_geometry(
    list(path = f$gpkg, format = "gpkg", layer = "geom"),
    root_dir = f$root,
    country = "TST",
    section = "inputs.geometry[2]"
  )
  geojson_sf <- read_geometry(
    list(path = f$geojson, format = "geojson"),
    root_dir = f$root,
    country = "TST",
    section = "inputs.geometry[3]"
  )

  testthat::expect_s3_class(shp_sf, "sf")
  testthat::expect_s3_class(gpkg_sf, "sf")
  testthat::expect_s3_class(geojson_sf, "sf")
  testthat::expect_equal(nrow(shp_sf), 2)
  testthat::expect_equal(nrow(gpkg_sf), 2)
  testthat::expect_equal(nrow(geojson_sf), 2)
})

testthat::test_that("reader errors include country/section/field", {
  testthat::expect_error(
    read_tabular(
      list(path = "missing.csv", format = "csv"),
      root_dir = tempdir(),
      country = "BAD",
      section = "inputs.tabular[1]"
    ),
    regexp = "country=BAD.*section=inputs.tabular\\[1\\].*field=path"
  )

  testthat::expect_error(
    read_geometry(
      list(path = "geom.unknown", format = "wkt"),
      root_dir = tempdir(),
      country = "BAD",
      section = "inputs.geometry[1]"
    ),
    regexp = "country=BAD.*section=inputs.geometry\\[1\\].*field=format"
  )
})
