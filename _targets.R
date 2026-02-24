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
source(file.path("R", "grid_intersections.R"))
source(file.path("R", "dasymetric_allocation.R"))
source(file.path("R", "allocation_validation.R"))
source(file.path("R", "soilgrids.R"))
source(file.path("R", "terrain_features.R"))
source(file.path("R", "water_distance.R"))
source(file.path("R", "duckdb_store.R"))

tar_option_set(
  packages = c("yaml", "sf", "arrow", "terra", "exactextractr", "ranger", "xgboost", "lightgbm", "tidymodels", "bonsai", "spatialsample", "doParallel", "duckdb", "DBI")
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
  tar_target(allocation_cfg_file, file.path("config", "global", "allocation.yml"), format = "file"),

  tar_target(project_cfg, read_yaml_file(project_cfg_file)),
  tar_target(paths_cfg,   read_yaml_file(paths_cfg_file)),
  tar_target(crs_cfg,     read_yaml_file(crs_cfg_file)),
  tar_target(qa_cfg,      read_yaml_file(qa_cfg_file)),
  tar_target(ml_cfg,      read_yaml_file(ml_cfg_file)),
  tar_target(allocation_cfg, read_yaml_file(allocation_cfg_file)),

  # ── Feature registry ──────────────────────────────────────────
  tar_target(features_cfg_file, file.path("config", "sources", "features.yml"), format = "file"),
  tar_target(
    feature_registry,
    {
      features_cfg_file          # tracked dependency — invalidates when YAML changes
      load_feature_registry(root_dir = ".")
    }
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

  # ── SoilGrids raster download ────────────────────────────────
  tar_target(
    soilgrids_dl,
    download_soilgrids_rasters(
      soilgrids_cfg = load_source_config("config/sources/soilgrids.yml"),
      canonical_crs = crs_cfg$crs$canonical,
      root_dir      = "."
    ),
    format = "file"
  ),

  # ── Terrain rasters (slope + TRI from DEM) ───────────────────
  tar_target(
    terrain_rasters,
    compute_terrain_rasters(
      dem_path      = elevation_raster_dl,
      terrain_cfg   = load_source_config("config/sources/terrain.yml"),
      canonical_crs = crs_cfg$crs$canonical,
      root_dir      = "."
    ),
    format = "file"
  ),

  # ── Water distance rasters (coast + major rivers) ────────────
  # Uses slope raster from terrain step as friction surface for
  # least-cost path analysis (costDist).
  tar_target(
    water_distance_rasters,
    compute_water_distance_rasters(
      water_cfg     = load_source_config("config/sources/water_distance.yml"),
      dem_path      = elevation_raster_dl,
      slope_path    = terrain_rasters[1],   # first element is slope raster
      canonical_crs = crs_cfg$crs$canonical,
      root_dir      = "."
    ),
    format = "file"
  ),

  # ── Feature extraction (branched per country) ─────────────────
  tar_target(
    country_panel_features,
    {
      force(elevation_raster_dl)      # ensure DEM is downloaded before extraction
      force(soilgrids_dl)             # ensure soil rasters are downloaded
      force(terrain_rasters)          # ensure slope + TRI rasters are computed
      force(water_distance_rasters)   # ensure distance rasters are computed
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
    combined_panel_raw,
    do.call(rbind, country_panel_features)
  ),

  # ── Soil PCA: fit on combined panel and transform ────────────
  tar_target(
    soil_pca_model,
    fit_soil_pca(
      panel_sf           = combined_panel_raw,
      variance_threshold = {
        sg_cfg <- load_source_config("config/sources/soilgrids.yml")
        sg_cfg$pca$variance_threshold %||% 0.95
      }
    )
  ),
  tar_target(
    combined_panel,
    apply_soil_pca_to_panel(combined_panel_raw, soil_pca_model)
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

  # ── Score-proxy model (cell-score compatible, excludes polygon-only features) ──
  # Transitional bridge: trains a second polygon-support model using the same
  # target/response setup but without `log_area`, so raster scores used for
  # dasymetric allocation do not depend on polygon-only predictors.
  tar_target(
    score_model_data,
    prepare_model_data(
      panel_sf = train_panel,
      ml_cfg = ml_cfg,
      feature_registry = feature_registry,
      include_polygon_only_features = FALSE
    )
  ),
  tar_target(
    score_spatial_folds,
    create_spatial_resamples(
      panel_sf = train_panel,
      model_data = score_model_data,
      ml_cfg = ml_cfg,
      seed = project_cfg$project$seed
    )
  ),
  tar_target(
    score_model_spec,
    list(
      id = paste0(best_model$model_id %||% "best", "_score"),
      engine = best_model$engine %||% "ranger",
      tune = FALSE
    )
  ),
  tar_target(
    score_model_result,
    run_spatial_cv(
      model_spec = score_model_spec,
      model_data = score_model_data,
      resamples = score_spatial_folds,
      ml_cfg = ml_cfg,
      seed = project_cfg$project$seed
    )
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

  # ── Constrained dasymetric allocation (baseline: uniform area) ─────────────
  tar_target(
    allocation_mass_tolerance_rel,
    allocation_cfg$allocation$qa$mass_rel_error_tolerance %||%
      qa_cfg$qa$allocation$mass_rel_error_tolerance %||% 1e-8
  ),
  tar_target(
    allocation_panel_constrained,
    {
      panel <- combined_panel
      if ("population" %in% names(panel)) {
        keep <- is.finite(as.numeric(panel$population))
        panel <- panel[keep, , drop = FALSE]
      }
      panel
    }
  ),
  tar_target(
    polygon_grid_intersections,
    {
      alloc_grid <- create_prediction_grid(
        panel_sf = allocation_panel_constrained,
        resolution_m = ml_cfg$ml$raster_prediction$resolution_m %||% 1000,
        canonical_crs = crs_cfg$crs$canonical
      )
      build_polygon_grid_intersections(
        panel_sf = allocation_panel_constrained,
        grid_template = alloc_grid,
        canonical_crs = crs_cfg$crs$canonical,
        keep_geometry = FALSE
      )
    }
  ),
  tar_target(
    uniform_area_allocation,
    allocate_uniform_by_area(
      intersections_df = polygon_grid_intersections,
      area_denominator_col = allocation_cfg$allocation$area_denominator %||% "overlap_area_m2",
      polygon_total_col = "population"
    )
  ),
  tar_target(
    allocation_diagnostics_uniform,
    validate_mass_preservation(
      allocation_df = uniform_area_allocation,
      polygon_panel = allocation_panel_constrained,
      polygon_total_col = "population",
      tolerance_rel = allocation_mass_tolerance_rel
    )
  ),
  tar_target(
    allocation_qc_uniform,
    assert_allocation_qc(
      diagnostics_df = allocation_diagnostics_uniform,
      tolerance_rel = allocation_mass_tolerance_rel
    )
  ),
  tar_target(
    allocation_diagnostics_uniform_summary,
    {
      allocation_qc_uniform
      allocation_diagnostics_summary(
        allocation_diagnostics_uniform,
        tolerance_rel = allocation_mass_tolerance_rel
      )
    }
  ),
  tar_target(
    allocation_diagnostics_uniform_file,
    write_allocation_diagnostics(
      diagnostics_df = allocation_diagnostics_uniform,
      model_id = "uniform_area",
      year = NULL,
      output_dir = file.path(paths_cfg$paths$final_data %||% "data/final", "diagnostics"),
      root_dir = ".",
      prefer_parquet = FALSE
    ),
    format = "file"
  ),
  tar_target(
    constrained_population_rasters_uniform,
    {
      decades <- sort(unique(as.integer(uniform_area_allocation$year)))
      out_dir <- paths_cfg$paths$predictions %||% "data/final/predictions"
      alloc_grid <- create_prediction_grid(
        panel_sf = allocation_panel_constrained,
        resolution_m = ml_cfg$ml$raster_prediction$resolution_m %||% 1000,
        canonical_crs = crs_cfg$crs$canonical
      )
      paths <- character(0)
      for (decade in decades) {
        alloc_y <- uniform_area_allocation[as.integer(uniform_area_allocation$year) == decade, , drop = FALSE]
        path <- write_constrained_population_raster(
          allocation_df = alloc_y,
          grid_template = alloc_grid,
          label = paste0("global_", decade),
          model_id = "uniform_area",
          output_dir = out_dir,
          root_dir = "."
        )
        paths <- c(paths, path)
      }
      paths
    },
    format = "file"
  ),
  tar_target(
    intensity_rasters_uniform,
    {
      decades <- sort(unique(as.integer(uniform_area_allocation$year)))
      out_dir <- paths_cfg$paths$predictions %||% "data/final/predictions"
      alloc_grid <- create_prediction_grid(
        panel_sf = allocation_panel_constrained,
        resolution_m = ml_cfg$ml$raster_prediction$resolution_m %||% 1000,
        canonical_crs = crs_cfg$crs$canonical
      )
      paths <- character(0)
      for (decade in decades) {
        alloc_y <- uniform_area_allocation[as.integer(uniform_area_allocation$year) == decade, , drop = FALSE]
        intensity_r <- rasterize_cell_values(
          grid_template = alloc_grid,
          cell_values = alloc_y,
          value_col = "weight_raw",
          fun = sum,
          background = 0,
          layer_name = "intensity"
        )
        vals <- terra::values(intensity_r, mat = FALSE)
        total_score <- sum(vals, na.rm = TRUE)
        if (is.finite(total_score) && total_score > 0) {
          terra::values(intensity_r) <- vals / total_score
        }
        path <- predict_unconstrained_intensity(
          score_raster = intensity_r,
          label = paste0("global_", decade),
          model_id = "uniform_area",
          output_dir = out_dir,
          root_dir = "."
        )
        paths <- c(paths, path)
      }
      paths
    },
    format = "file"
  ),

  # ── ML-weighted constrained allocation (score proxy model) ──────────────────
  tar_target(
    calibration_totals_by_year,
    {
      cal <- ml_cfg$ml$calibration %||% list()
      alloc_cal <- allocation_cfg$allocation$calibration %||% list()
      enabled <- alloc_cal$enabled
      if (is.null(enabled)) enabled <- cal$enabled
      if (!isTRUE(enabled)) return(NULL)
      totals <- alloc_cal$totals_by_year %||% cal$totals_by_year %||% NULL
      if (is.null(totals)) return(NULL)
      out <- unlist(totals, use.names = TRUE)
      vals <- suppressWarnings(as.numeric(out))
      stats::setNames(vals, names(out))
    }
  ),
  tar_target(
    allocation_years_ml,
    sort(unique(as.integer(polygon_grid_intersections$year)))
  ),
  tar_target(
    ml_weighted_allocation_year,
    {
      decade <- as.integer(allocation_years_ml)
      out_dir <- paths_cfg$paths$predictions %||% "data/final/predictions"
      alloc_grid <- create_prediction_grid(
        panel_sf = allocation_panel_constrained,
        resolution_m = ml_cfg$ml$raster_prediction$resolution_m %||% 1000,
        canonical_crs = crs_cfg$crs$canonical
      )
      pred <- predict_raster_surface(
        best_cv_result = score_model_result,
        panel_sf = combined_panel,
        feature_registry = feature_registry,
        ml_cfg = ml_cfg,
        canonical_crs = crs_cfg$crs$canonical,
        prediction_year = decade,
        root_dir = ".",
        soil_pca_model = soil_pca_model,
        include_log_area = FALSE
      )
      score_r <- prediction_raster_to_score(pred$raster, pred$target_info)
      intensity_path <- predict_unconstrained_intensity(
        score_raster = score_r,
        label = paste0("global_", decade),
        model_id = score_model_result$model_id,
        output_dir = out_dir,
        root_dir = "."
      )

      ints_y <- polygon_grid_intersections[as.integer(polygon_grid_intersections$year) == decade, , drop = FALSE]
      ints_scored <- join_cell_scores_from_raster(ints_y, score_r, score_col = "score_raw")
      alloc_y <- allocate_constrained_from_scores(
        intersections_df = ints_scored,
        score_col = "score_raw",
        area_denominator_col = allocation_cfg$allocation$area_denominator %||% "overlap_area_m2",
        polygon_total_col = "population"
      )
      panel_y <- allocation_panel_constrained[as.integer(allocation_panel_constrained$year) == decade, , drop = FALSE]
      diagnostics <- validate_mass_preservation(
        allocation_df = alloc_y,
        polygon_panel = panel_y,
        polygon_total_col = "population",
        tolerance_rel = allocation_mass_tolerance_rel
      )

      constrained_path <- write_constrained_population_raster(
        allocation_df = alloc_y,
        grid_template = alloc_grid,
        label = paste0("global_", decade),
        model_id = score_model_result$model_id,
        output_dir = out_dir,
        root_dir = "."
      )

      calibrated_path <- character(0)
      if (!is.null(calibration_totals_by_year)) {
        tgt <- calibration_totals_by_year[as.character(decade)]
        if (length(tgt) == 1L && is.finite(tgt)) {
          cal_r <- calibrate_intensity_raster(score_r, target_total = tgt)
          calibrated_path <- write_semantic_raster(
            raster = cal_r,
            label = paste0("global_", decade),
            model_id = score_model_result$model_id,
            kind = "calibrated",
            output_dir = out_dir,
            root_dir = "."
          )
          rm(cal_r)
        }
      }

      rm(pred, score_r, ints_y, ints_scored, alloc_y, panel_y, alloc_grid)
      invisible(gc())

      list(
        year = decade,
        diagnostics = diagnostics,
        intensity_path = intensity_path,
        constrained_path = constrained_path,
        calibrated_path = calibrated_path
      )
    },
    pattern = map(allocation_years_ml),
    iteration = "list"
  ),
  tar_target(
    allocation_diagnostics_ml,
    {
      parts <- ml_weighted_allocation_year
      diags <- lapply(parts, function(x) x$diagnostics)
      if (length(diags)) do.call(rbind, diags) else data.frame()
    }
  ),
  tar_target(
    allocation_qc_ml,
    assert_allocation_qc(
      diagnostics_df = allocation_diagnostics_ml,
      tolerance_rel = allocation_mass_tolerance_rel
    )
  ),
  tar_target(
    allocation_diagnostics_ml_summary,
    {
      allocation_qc_ml
      allocation_diagnostics_summary(
        allocation_diagnostics_ml,
        tolerance_rel = allocation_mass_tolerance_rel
      )
    }
  ),
  tar_target(
    allocation_diagnostics_ml_file,
    write_allocation_diagnostics(
      diagnostics_df = allocation_diagnostics_ml,
      model_id = score_model_result$model_id,
      year = NULL,
      output_dir = file.path(paths_cfg$paths$final_data %||% "data/final", "diagnostics"),
      root_dir = ".",
      prefer_parquet = FALSE
    ),
    format = "file"
  ),
  tar_target(
    intensity_rasters_ml,
    {
      allocation_qc_ml
      unlist(lapply(ml_weighted_allocation_year, function(x) x$intensity_path), use.names = FALSE)
    },
    format = "file"
  ),
  tar_target(
    constrained_population_rasters_ml,
    {
      allocation_qc_ml
      unlist(lapply(ml_weighted_allocation_year, function(x) x$constrained_path), use.names = FALSE)
    },
    format = "file"
  ),
  tar_target(
    calibrated_population_rasters_ml,
    unlist(lapply(ml_weighted_allocation_year, function(x) x$calibrated_path), use.names = FALSE),
    format = "file"
  )
)
