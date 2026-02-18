source(file.path("..", "..", "R", "config_loader.R"))

testthat::test_that("validator passes for a minimal valid config in mock mode", {
  root <- file.path(tempdir(), paste0("cfg_", as.integer(stats::runif(1, 1, 1e9))))
  dir.create(root, recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(root, "config", "countries"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(root, "config", "crosswalks"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(root, "config", "sources"), recursive = TRUE, showWarnings = FALSE)

  writeLines(
    c(
      "country_code,from_admin_id,from_admin_name,to_admin_id,to_admin_name",
      "TST,1,A,1,A"
    ),
    file.path(root, "config", "crosswalks", "TST.csv")
  )

  writeLines(
    c(
      "features_registry:",
      "  - id: \"elevation_mean\"",
      "    source_config: \"config/sources/elevation.yml\""
    ),
    file.path(root, "config", "sources", "features.yml")
  )

  writeLines(
    c(
      "source:",
      "  id: \"elevation\"",
      "  mode: \"local\""
    ),
    file.path(root, "config", "sources", "elevation.yml")
  )

  writeLines(
    c(
      "country:",
      "  iso3: \"TST\"",
      "  enabled: true",
      "inputs:",
      "  tabular:",
      "    - id: \"tab_main\"",
      "      path: \"data/raw/TST/panel.csv\"",
      "      format: \"csv\"",
      "      columns:",
      "        admin_id_raw: \"id\"",
      "        admin_name: \"name\"",
      "        year: \"year\"",
      "        population: \"population\"",
      "  geometry:",
      "    - id: \"geom_main\"",
      "      path: \"data/raw/TST/admin.geojson\"",
      "      format: \"geojson\"",
      "      columns:",
      "        admin_id_raw: \"id\"",
      "        admin_name: \"name\"",
      "harmonization:",
      "  crosswalk_file: \"config/crosswalks/TST.csv\"",
      "assemblies:",
      "  - id: \"panel\"",
      "    tabular_recipe:",
      "      strategy: \"single\"",
      "      use_inputs: [\"tab_main\"]",
      "    geometry_recipe:",
      "      strategy: \"single\"",
      "      use_inputs: [\"geom_main\"]"
    ),
    file.path(root, "config", "countries", "TST.yml")
  )

  testthat::expect_silent(
    validate_country_configs(
      countries_dir = file.path("config", "countries"),
      root_dir = root,
      countries = "TST",
      mock_mode = TRUE,
      features_file = file.path("config", "sources", "features.yml")
    )
  )
})

testthat::test_that("validator errors when assembly references missing input id", {
  root <- file.path(tempdir(), paste0("cfg_", as.integer(stats::runif(1, 1, 1e9))))
  dir.create(root, recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(root, "config", "countries"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(root, "config", "crosswalks"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(root, "config", "sources"), recursive = TRUE, showWarnings = FALSE)

  writeLines("features_registry: []", file.path(root, "config", "sources", "features.yml"))
  writeLines("country_code,from_admin_id,to_admin_id,to_admin_name", file.path(root, "config", "crosswalks", "BAD.csv"))

  writeLines(
    c(
      "country:",
      "  iso3: \"BAD\"",
      "inputs:",
      "  tabular:",
      "    - id: \"tab_main\"",
      "      path: \"data/raw/BAD/panel.csv\"",
      "      format: \"csv\"",
      "      columns:",
      "        admin_id_raw: \"id\"",
      "        admin_name: \"name\"",
      "        year: \"year\"",
      "        population: \"population\"",
      "  geometry:",
      "    - id: \"geom_main\"",
      "      path: \"data/raw/BAD/admin.geojson\"",
      "      format: \"geojson\"",
      "      columns:",
      "        admin_id_raw: \"id\"",
      "        admin_name: \"name\"",
      "harmonization:",
      "  crosswalk_file: \"config/crosswalks/BAD.csv\"",
      "assemblies:",
      "  - id: \"panel\"",
      "    tabular_recipe:",
      "      strategy: \"single\"",
      "      use_inputs: [\"tab_missing\"]",
      "    geometry_recipe:",
      "      strategy: \"single\"",
      "      use_inputs: [\"geom_main\"]"
    ),
    file.path(root, "config", "countries", "BAD.yml")
  )

  testthat::expect_error(
    validate_country_configs(
      countries_dir = file.path("config", "countries"),
      root_dir = root,
      countries = "BAD",
      mock_mode = TRUE,
      features_file = file.path("config", "sources", "features.yml")
    ),
    regexp = "country=BAD.*section=assemblies\\[1\\].*field=tabular_recipe.use_inputs"
  )
})
