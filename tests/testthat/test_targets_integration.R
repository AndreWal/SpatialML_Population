testthat::test_that("toy project tar_make works in mock mode", {
  testthat::skip_if_not_installed("callr")
  root <- file.path(tempdir(), paste0("tar_proj_", as.integer(stats::runif(1, 1, 1e9))))
  dir.create(root, recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(root, "R"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(root, "config", "countries"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(root, "config", "global"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(root, "config", "sources"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(root, "config", "crosswalks"), recursive = TRUE, showWarnings = FALSE)

  file.copy(file.path("..", "..", "_targets.R"), file.path(root, "_targets.R"))
  file.copy(file.path("..", "..", "R", "config_loader.R"), file.path(root, "R", "config_loader.R"))
  file.copy(file.path("..", "..", "R", "io_readers.R"), file.path(root, "R", "io_readers.R"))
  file.copy(file.path("..", "..", "R", "assembly.R"), file.path(root, "R", "assembly.R"))
  file.copy(file.path("..", "..", "R", "pipeline_country.R"), file.path(root, "R", "pipeline_country.R"))
  file.copy(file.path("..", "..", "R", "feature_extraction.R"), file.path(root, "R", "feature_extraction.R"))
  file.copy(file.path("..", "..", "R", "spatial_cv.R"), file.path(root, "R", "spatial_cv.R"))
  file.copy(file.path("..", "..", "R", "model_training.R"), file.path(root, "R", "model_training.R"))
  file.copy(file.path("..", "..", "R", "model_evaluation.R"), file.path(root, "R", "model_evaluation.R"))
  file.copy(file.path("..", "..", "R", "mlflow_utils.R"), file.path(root, "R", "mlflow_utils.R"))
  file.copy(file.path("..", "..", "R", "raster_predict.R"), file.path(root, "R", "raster_predict.R"))
  file.copy(file.path("..", "..", "R", "duckdb_store.R"), file.path(root, "R", "duckdb_store.R"))

  writeLines(
    c(
      "project:",
      "  name: \"toy\"",
      "  seed: 42",
      "countries:",
      "  enabled: [\"TST\"]",
      "outputs:",
      "  vector_format: \"gpkg\"",
      "  table_format: \"parquet\""
    ),
    file.path(root, "config", "global", "project.yml")
  )
  writeLines(
    c(
      "paths:",
      "  final_data: \"data/final\"",
      "  models: \"models\"",
      "  cache: \"cache\""
    ),
    file.path(root, "config", "global", "paths.yml")
  )
  writeLines(
    c(
      "crs:",
      "  canonical: \"EPSG:3035\""
    ),
    file.path(root, "config", "global", "crs.yml")
  )
  writeLines(
    c(
      "qa:",
      "  coverage:",
      "    join_coverage_min: 0.0",
      "  keys:",
      "    unique_key: [\"country_code\", \"admin_unit_harmonized\", \"year\"]"
    ),
    file.path(root, "config", "global", "qa.yml")
  )
  writeLines(
    c(
      "ml:",
      "  target_variable: \"population\"",
      "  split:",
      "    method: \"spatial_cv\"",
      "    folds: 2",
      "    block_size_km: 100",
      "  models:",
      "    - id: \"rf\"",
      "      engine: \"ranger\"",
      "  evaluation:",
      "    metrics: [\"rmse\", \"mae\", \"rsq\"]",
      "  raster_prediction:",
      "    resolution_m: 5000",
      "    clamp_to_training_range: true"
    ),
    file.path(root, "config", "global", "ml.yml")
  )
  writeLines(
    c(
      "features_registry:",
      "  - id: \"elevation_mean\"",
      "    source_config: \"config/sources/elevation.yml\"",
      "    enabled: true"
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
      "country_code,from_admin_id,from_admin_name,to_admin_id,to_admin_name,weight",
      "TST,1,A,1,A,1"
    ),
    file.path(root, "config", "crosswalks", "TST.csv")
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
      "  unmatched_policy: \"warn\"",
      "assemblies:",
      "  - id: \"main_panel\"",
      "    tabular_recipe:",
      "      strategy: \"single\"",
      "      use_inputs: [\"tab_main\"]",
      "    geometry_recipe:",
      "      strategy: \"single\"",
      "      use_inputs: [\"geom_main\"]"
    ),
    file.path(root, "config", "countries", "TST.yml")
  )

  res <- callr::r(
    func = function(project_root) {
      setwd(project_root)
      Sys.setenv(MOCK_MODE = "true")
      targets::tar_make(reporter = "silent")
      gpkg <- file.path(project_root, "data", "final", "TST", "TST_panel.gpkg")
      parquet <- file.path(project_root, "data", "final", "TST", "TST_panel.parquet")
      model_dir <- file.path(project_root, "models")
      has_etl <- file.exists(gpkg) && file.exists(parquet)
      has_models <- dir.exists(model_dir) && length(list.files(model_dir, pattern = "\\.rds$")) > 0
      has_etl && has_models
    },
    args = list(project_root = root)
  )

  testthat::expect_true(isTRUE(res))
})
