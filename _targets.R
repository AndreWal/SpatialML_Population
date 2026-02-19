library(targets)

source(file.path("R", "config_loader.R"))
source(file.path("R", "io_readers.R"))
source(file.path("R", "assembly.R"))
source(file.path("R", "pipeline_country.R"))
source(file.path("R", "feature_extraction.R"))
source(file.path("R", "spatial_cv.R"))
source(file.path("R", "model_training.R"))
source(file.path("R", "model_evaluation.R"))
source(file.path("R", "mlflow_utils.R"))
source(file.path("R", "raster_predict.R"))
source(file.path("R", "duckdb_store.R"))

tar_option_set(
  packages = c("yaml", "sf", "arrow", "terra", "ranger", "xgboost", "lightgbm", "tidymodels", "bonsai", "spatialsample", "doParallel", "duckdb", "DBI")
)

list(
  # ── Config validation ──────────────────────────────────────────
  tar_target(
    country_config_validation,
    validate_enabled_country_configs(
      root_dir = "."
    )
  ),
  # File-tracking targets: targets hashes the file content so any edit to a
  # YAML config automatically invalidates the corresponding parsed target.
  tar_target(project_cfg_file, file.path("config", "global", "project.yml"), format = "file"),
  tar_target(paths_cfg_file,   file.path("config", "global", "paths.yml"),   format = "file"),
  tar_target(crs_cfg_file,     file.path("config", "global", "crs.yml"),     format = "file"),
  tar_target(qa_cfg_file,      file.path("config", "global", "qa.yml"),      format = "file"),
  tar_target(ml_cfg_file,      file.path("config", "global", "ml.yml"),      format = "file"),

  tar_target(project_cfg, read_yaml_file(project_cfg_file)),
  tar_target(paths_cfg,   read_yaml_file(paths_cfg_file)),
  tar_target(crs_cfg,     read_yaml_file(crs_cfg_file)),
  tar_target(qa_cfg,      read_yaml_file(qa_cfg_file)),
  tar_target(ml_cfg,      read_yaml_file(ml_cfg_file)),

  # ── Feature registry ──────────────────────────────────────────
  tar_target(
    feature_registry,
    load_feature_registry(root_dir = ".")
  ),

  # ── Country ETL (branched) ────────────────────────────────────
  tar_target(
    enabled_countries,
    unlist(project_cfg$countries$enabled)
  ),
  tar_target(
    country_code,
    enabled_countries,
    pattern = map(enabled_countries)
  ),
  tar_target(
    country_cfg,
    load_country_config(country_code, root_dir = "."),
    pattern = map(country_code)
  ),
  tar_target(
    country_panel_read,
    read_country_inputs(country_cfg, root_dir = "."),
    pattern = map(country_cfg),
    iteration = "list"
  ),
  tar_target(
    country_panel_harmonized,
    harmonize_keys(country_panel_read, country_cfg, root_dir = "."),
    pattern = map(country_panel_read, country_cfg),
    iteration = "list"
  ),
  tar_target(
    country_panel_crs,
    transform_to_canonical_crs(country_panel_harmonized, crs_cfg$crs$canonical),
    pattern = map(country_panel_harmonized),
    iteration = "list"
  ),
  tar_target(
    country_panel_geom_valid,
    validate_and_fix_geometry(
      panel_sf = country_panel_crs,
      qa_cfg = qa_cfg,
      country_code = country_cfg$country$iso3
    ),
    pattern = map(country_panel_crs, country_cfg),
    iteration = "list"
  ),
  tar_target(
    country_panel_validated,
    validate_country_panel_qa(
      panel_sf = country_panel_geom_valid,
      panel_harmonized = country_panel_harmonized,
      country_cfg = country_cfg,
      qa_cfg = qa_cfg,
      canonical_crs = crs_cfg$crs$canonical
    ),
    pattern = map(country_panel_geom_valid, country_panel_harmonized, country_cfg),
    iteration = "list"
  ),

  # ── Elevation raster download ────────────────────────────────
  tar_target(
    elevation_raster_dl,
    download_elevation_raster(
      elevation_cfg  = load_source_config("config/sources/elevation.yml"),
      canonical_crs  = crs_cfg$crs$canonical,
      root_dir       = "."
    ),
    format = "file"
  ),

  # ── Feature extraction (branched per country) ─────────────────
  tar_target(
    country_panel_features,
    {
      force(elevation_raster_dl)  # ensure DEM is downloaded before extraction
      extract_and_join_features(
        panel_sf = country_panel_validated,
        feature_registry = feature_registry,
        canonical_crs = crs_cfg$crs$canonical,
        root_dir = "."
      )
    },
    pattern = map(country_panel_validated),
    iteration = "list"
  ),

  # ── Write country outputs ─────────────────────────────────────
  tar_target(
    country_output_files,
    write_country_outputs(
      panel_sf = country_panel_features,
      country_code = country_cfg$country$iso3,
      final_data_dir = paths_cfg$paths$final_data,
      root_dir = "."
    ),
    pattern = map(country_panel_features, country_cfg),
    format = "file"
  ),

  # ── Combine panels for ML ─────────────────────────────────────
  tar_target(
    combined_panel,
    do.call(rbind, country_panel_features)
  ),

  # ── Global panel (all countries) ─────────────────────────────
  tar_target(
    global_panel_files,
    write_global_panel(
      combined_panel  = combined_panel,
      final_data_dir  = paths_cfg$paths$final_data %||% "data/final",
      root_dir        = "."
    ),
    format = "file"
  ),

  # ── Hold-out test split (DEU out, rest for training/CV) ───────
  tar_target(
    holdout_country,
    ml_cfg$ml$holdout_test_country %||% "DEU"
  ),
  tar_target(
    holdout_panel,
    {
      cc <- as.character(combined_panel$country_code)
      combined_panel[cc == holdout_country, ]
    }
  ),
  tar_target(
    train_panel,
    {
      cc <- as.character(combined_panel$country_code)
      combined_panel[cc != holdout_country, ]
    }
  ),

  # ── DuckDB intermediate store ─────────────────────────────────
  tar_target(
    duckdb_tables,
    {
      panels <- list()
      cc <- as.character(combined_panel$country_code)
      for (code in unique(cc)) {
        panels[[code]] <- combined_panel[cc == code, ]
      }
      store_panels_duckdb(panels, db_path = "cache/panels.duckdb", root_dir = ".")
    }
  ),

  # ── Prepare model data (training countries only) ──────────────
  tar_target(
    model_data,
    prepare_model_data(
      panel_sf         = train_panel,
      ml_cfg           = ml_cfg,
      feature_registry = feature_registry
    )
  ),

  # ── Spatial CV resamples (spatialsample, training countries only) ──
  tar_target(
    spatial_folds,
    create_spatial_resamples(
      panel_sf   = train_panel,
      model_data = model_data,
      ml_cfg     = ml_cfg,
      seed       = project_cfg$project$seed
    )
  ),

  # ── Model IDs for branching ───────────────────────────────────
  tar_target(
    model_specs,
    {
      specs <- ml_cfg$ml$models
      # Filter to supported engines only
      supported <- c("ranger", "xgboost", "lightgbm")
      Filter(function(s) s$engine %in% supported, specs)
    },
    iteration = "list"  # enables pattern = map(model_specs) branching below
  ),

  # ── Train + CV per model (one branch per engine) ───────────────
  # Each branch is a fully independent targets unit: independently cached,
  # independently re-run when only that engine's config changes.
  tar_target(
    cv_results,
    run_spatial_cv(
      model_spec = model_specs,
      model_data = model_data,
      resamples  = spatial_folds,
      ml_cfg     = ml_cfg,
      seed       = project_cfg$project$seed
    ),
    pattern   = map(model_specs),
    iteration = "list"
  ),

  # ── Save CV results ───────────────────────────────────────────
  tar_target(
    cv_output_files,
    save_cv_results(
      cv_results_list = cv_results,
      output_dir      = paths_cfg$paths$models,
      root_dir        = ".",
      train_countries = unique(as.character(train_panel$country_code))
    ),
    format = "file"
  ),

  # ── Select best model ─────────────────────────────────────────
  tar_target(
    best_model,
    select_best_model(cv_results, metric = "rmse")
  ),

  # ── CV summary ────────────────────────────────────────────────
  tar_target(
    cv_summary,
    summarize_cv_results(
      cv_results,
      train_countries = unique(as.character(train_panel$country_code))
    )
  ),

  # ── Hold-out evaluation on DEU ────────────────────────────────
  tar_target(
    holdout_metrics,
    evaluate_holdout(
      best_cv_result   = best_model,
      holdout_panel_sf = holdout_panel,
      ml_cfg           = ml_cfg,
      feature_registry = feature_registry
    )
  ),
  # ── Combined model summary (CV + holdout in one CSV) ───────────────
  tar_target(
    model_summary_file,
    write_model_summary(
      cv_results_list      = cv_results,
      holdout_metrics_list = holdout_metrics,
      train_countries      = unique(as.character(train_panel$country_code)),
      output_dir           = paths_cfg$paths$models,
      root_dir             = "."
    ),
    format = "file"
  ),
  # ── MLflow logging ────────────────────────────────────────────
  tar_target(
    mlflow_logged,
    log_all_model_runs(
      cv_results_list = cv_results,
      ml_cfg          = ml_cfg,
      project_cfg     = project_cfg,
      model_dir       = file.path(".", paths_cfg$paths$models)
    )
  ),

  # ── Raster predictions: one GeoTIFF per decade → data/final/predictions/ ──
  #
  # The `year` column already contains round decade values (1850, 1860, …).
  # Each raster sets year_<decade>=1 and all other year dummies to 0, exactly
  # mirroring how the training feature matrix was constructed.
  tar_target(
    prediction_rasters,
    {
      decades   <- sort(unique(as.integer(combined_panel$year)))
      out_dir   <- paths_cfg$paths$predictions %||% "data/final/predictions"
      paths     <- character(0)
      for (decade in decades) {
        path <- run_raster_prediction(
          best_cv_result   = best_model,
          panel_sf         = combined_panel,
          feature_registry = feature_registry,
          ml_cfg           = ml_cfg,
          canonical_crs    = crs_cfg$crs$canonical,
          prediction_year  = decade,
          label            = paste0("global_", decade),
          output_dir       = out_dir,
          root_dir         = "."
        )
        paths <- c(paths, path)
      }
      paths
    },
    format = "file"
  )
)
