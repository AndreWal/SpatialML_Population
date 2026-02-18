source(file.path("..", "..", "R", "io_readers.R"))
source(file.path("..", "..", "R", "config_loader.R"))
source(file.path("..", "..", "R", "assembly.R"))

write_geojson <- function(path, ids, names) {
  g <- sf::st_sf(
    id = ids,
    name = names,
    geometry = sf::st_sfc(
      sf::st_point(c(8.5, 47.3)),
      sf::st_point(c(8.6, 47.4)),
      crs = 4326
    )
  )
  sf::st_write(g, path, driver = "GeoJSON", quiet = TRUE, delete_dsn = TRUE)
}

make_assembly_fixture <- function() {
  root <- file.path(tempdir(), paste0("asm_", as.integer(stats::runif(1, 1, 1e9))))
  dir.create(root, recursive = TRUE, showWarnings = FALSE)

  tab_single <- data.frame(
    id = c(1, 2, 1, 2),
    name = c("A", "B", "A", "B"),
    year = c(2000, 2000, 2010, 2010),
    pop = c(100, 200, 110, 210)
  )
  utils::write.csv(tab_single, file.path(root, "tab_single.csv"), row.names = FALSE)

  tab_2000 <- data.frame(id = c(1, 2), name = c("A", "B"), year = c(2000, 2000), pop = c(100, 200))
  tab_2010 <- data.frame(id = c(1, 2), name = c("A", "B"), year = c(2010, 2010), pop = c(120, 220))
  utils::write.csv(tab_2000, file.path(root, "tab_2000.csv"), row.names = FALSE)
  utils::write.csv(tab_2010, file.path(root, "tab_2010.csv"), row.names = FALSE)

  tab_stack_a <- data.frame(id = c(1, 2), name = c("A", "B"), year = c(2000, 2000), pop = c(100, 200))
  tab_stack_b <- data.frame(id = c(1, 2), name = c("A2", "B2"), year = c(2000, 2000), pop = c(999, 999))
  utils::write.csv(tab_stack_a, file.path(root, "tab_stack_a.csv"), row.names = FALSE)
  utils::write.csv(tab_stack_b, file.path(root, "tab_stack_b.csv"), row.names = FALSE)

  write_geojson(file.path(root, "geom_static.geojson"), c(1, 2), c("A", "B"))
  write_geojson(file.path(root, "geom_2000.geojson"), c(1, 2), c("A", "B"))
  write_geojson(file.path(root, "geom_2010.geojson"), c(1, 2), c("A", "B"))
  write_geojson(file.path(root, "geom_stack_a.geojson"), c(1, 2), c("A", "B"))
  write_geojson(file.path(root, "geom_stack_b.geojson"), c(1, 2), c("A_ALT", "B_ALT"))

  cfg <- list(
    country = list(iso3 = "TST", enabled = TRUE),
    inputs = list(
      tabular = list(
        list(
          id = "tab_single",
          path = "tab_single.csv",
          format = "csv",
          columns = list(admin_id_raw = "id", admin_name = "name", year = "year", population = "pop")
        ),
        list(
          id = "tab_2000",
          path = "tab_2000.csv",
          format = "csv",
          columns = list(admin_id_raw = "id", admin_name = "name", year = "year", population = "pop")
        ),
        list(
          id = "tab_2010",
          path = "tab_2010.csv",
          format = "csv",
          columns = list(admin_id_raw = "id", admin_name = "name", year = "year", population = "pop")
        ),
        list(
          id = "tab_stack_a",
          path = "tab_stack_a.csv",
          format = "csv",
          columns = list(admin_id_raw = "id", admin_name = "name", year = "year", population = "pop")
        ),
        list(
          id = "tab_stack_b",
          path = "tab_stack_b.csv",
          format = "csv",
          columns = list(admin_id_raw = "id", admin_name = "name", year = "year", population = "pop")
        )
      ),
      geometry = list(
        list(
          id = "geom_static",
          path = "geom_static.geojson",
          format = "geojson",
          columns = list(admin_id_raw = "id", admin_name = "name")
        ),
        list(
          id = "geom_2000",
          path = "geom_2000.geojson",
          format = "geojson",
          valid_years = c(2000),
          columns = list(admin_id_raw = "id", admin_name = "name")
        ),
        list(
          id = "geom_2010",
          path = "geom_2010.geojson",
          format = "geojson",
          valid_years = c(2010),
          columns = list(admin_id_raw = "id", admin_name = "name")
        ),
        list(
          id = "geom_stack_a",
          path = "geom_stack_a.geojson",
          format = "geojson",
          valid_years = c(2000),
          columns = list(admin_id_raw = "id", admin_name = "name")
        ),
        list(
          id = "geom_stack_b",
          path = "geom_stack_b.geojson",
          format = "geojson",
          valid_years = c(2000),
          columns = list(admin_id_raw = "id", admin_name = "name")
        )
      )
    ),
    assemblies = list(
      list(
        id = "single_panel",
        tabular_recipe = list(strategy = "single", use_inputs = c("tab_single")),
        geometry_recipe = list(strategy = "single", use_inputs = c("geom_static"))
      ),
      list(
        id = "by_year_panel",
        tabular_recipe = list(
          strategy = "by_year",
          use_inputs = c("tab_2000", "tab_2010"),
          year_map = list("2000" = "tab_2000", "2010" = "tab_2010")
        ),
        geometry_recipe = list(
          strategy = "by_year",
          use_inputs = c("geom_2000", "geom_2010"),
          year_map = list("2000" = "geom_2000", "2010" = "geom_2010")
        )
      ),
      list(
        id = "stack_panel",
        tabular_recipe = list(
          strategy = "stack_then_resolve",
          use_inputs = c("tab_stack_a", "tab_stack_b")
        ),
        geometry_recipe = list(
          strategy = "stack_then_resolve",
          use_inputs = c("geom_stack_a", "geom_stack_b")
        )
      )
    )
  )
  list(root = root, cfg = cfg)
}

testthat::test_that("single assembly produces one country panel", {
  fx <- make_assembly_fixture()
  panel <- assemble_country_panel(fx$cfg, root_dir = fx$root, assembly_id = "single_panel")

  testthat::expect_s3_class(panel, "sf")
  testthat::expect_equal(nrow(panel), 4)
  testthat::expect_true(all(panel$country_code == "TST"))
  testthat::expect_true(all(c("admin_id_raw", "year", "population") %in% names(panel)))
})

testthat::test_that("by_year assembly uses year_map for tabular and geometry", {
  fx <- make_assembly_fixture()
  panel <- assemble_country_panel(fx$cfg, root_dir = fx$root, assembly_id = "by_year_panel")

  testthat::expect_s3_class(panel, "sf")
  testthat::expect_equal(sort(unique(panel$year)), c(2000, 2010))
  testthat::expect_equal(nrow(panel), 4)
})

testthat::test_that("stack_then_resolve keeps first source for duplicates", {
  fx <- make_assembly_fixture()
  panel <- assemble_country_panel(fx$cfg, root_dir = fx$root, assembly_id = "stack_panel")

  testthat::expect_s3_class(panel, "sf")
  testthat::expect_equal(nrow(panel), 2)
  testthat::expect_equal(sort(panel$population), c(100, 200))
})
