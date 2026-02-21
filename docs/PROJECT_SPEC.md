# Project Spec: Multi-country Geospatial ETL + Spatial ML (R)

## Objective
Build a reproducible, multi-country geospatial dataset and train spatially aware ML models to produce gridded prediction rasters.

## Scope
- Inputs are country-level tabular and geometry sources configured in `config/countries/*.yml`.
- Harmonization maps raw admin IDs to canonical units via optional country crosswalks.
- Feature engineering currently extracts raster zonal statistics from enabled sources in `config/sources/features.yml`.
- ML trains and compares tree-based models with spatial block CV.
- Outputs include per-country and global panels (`.gpkg` + `.parquet`), model artifacts (`.csv`, `.rds`), DuckDB intermediate tables, and prediction rasters (`.tif`).

## Canonical orchestration
The pipeline is orchestrated by `targets` in `_targets.R` and sources functions from:
- `R/config_loader.R`
- `R/io_readers.R`
- `R/assembly.R`
- `R/pipeline_country.R`
- `R/feature_extraction.R`
- `R/spatial_cv.R`
- `R/model_training.R`
- `R/model_evaluation.R`
- `R/mlflow_utils.R`
- `R/raster_predict.R`
- `R/soilgrids.R`
- `R/duckdb_store.R`

## Runtime behavior
- Enabled countries are read from `config/global/project.yml` at `countries.enabled`.
- Current default enabled list is `DEU`, `NLD`.
- Missing configured raw inputs fail the run (`read_country_inputs()` has no fallback/mock mode).
- Canonical CRS is `config/global/crs.yml::crs.canonical` (default `EPSG:3035`).

## Country ETL contract
For each enabled country branch:
1. Validate and load country config.
2. Assemble tabular and geometry inputs (`single`, `by_year`, or `stack_then_resolve`).
3. Harmonize keys via crosswalk (or identity mapping when no crosswalk is configured).
4. Transform to canonical CRS.
5. Validate/fix geometry (`st_make_valid` when configured).
6. Run QA checks (unique key, CRS, join coverage, non-empty geometry handling).
7. Extract enabled raster features and append derived geometric features:
   - `log_area` (`log1p(st_area)` in canonical CRS)
   - `lon`, `lat` (centroid coordinates in WGS84)
   - SoilGrids zonal means (61 raw layers; PCA-reduced after panel combination)
8. Write country outputs:
   - `data/final/<ISO3>/<ISO3>_panel.gpkg`
   - `data/final/<ISO3>/<ISO3>_panel.parquet`

## Cross-country ML contract
After country ETL:
1. Combine all country panels to one global sf panel.
2. Fit PCA on raw soil feature columns (`soil_*`), transform panel to replace `soil_*` with `soil_pc*` components (variance threshold configurable, default 95%).
3. Persist global outputs:
   - `data/final/global_panel.gpkg`
   - `data/final/global_panel.parquet`
3. Split into training vs holdout country (`config/global/ml.yml::ml.holdout_test_country`, default `DEU`).
4. Store tabular panels in DuckDB (`cache/panels.duckdb`):
   - `panel_<iso3>` tables
   - `panel_all`
5. Prepare model matrix from enabled feature columns plus derived columns and year dummies.
6. Create spatial block CV folds (`spatialsample::spatial_block_cv`).
7. Train/tune configured models (supported engines: `ranger`, `xgboost`, `lightgbm`).
8. Save CV summaries, fold metrics, and final model objects.
9. Select best model by CV RMSE.
10. Evaluate best model on the holdout country.
11. Write combined model summary CSV.
12. Optionally log runs to MLflow.
13. Produce one global prediction raster per decade/year present in the combined panel.

## Config-driven model setup
From `config/global/ml.yml` defaults:
- Target variable: `population`
- CV folds: `5`
- Holdout test country: `DEU`
- Models configured: `rf` (`ranger`), `xgb` (`xgboost`), `lgbm` (`lightgbm`)
- Raster prediction resolution: `1000 m`
- Clamp predictions to training response range: enabled

## QA rules
Global QA config in `config/global/qa.yml` plus optional country overrides:
- Unique key columns (default): `country_code`, `admin_unit_harmonized`, `year`
- Geometry validity requirement and auto-fix behavior
- Empty geometries are dropped with warning
- Final CRS must equal canonical CRS
- Join coverage minimum threshold (`qa.coverage.join_coverage_min`, country override supported)

## Output contract (high level)
- Country panels: `data/final/<ISO3>/<ISO3>_panel.gpkg`, `.parquet`
- Global panel: `data/final/global_panel.gpkg`, `.parquet`
- Models/metrics: `models/cv_summary.csv`, `models/<model_id>_folds.csv`, `models/<model_id>_final.rds`, `models/model_summary.csv`
- Prediction rasters: `data/final/predictions/global_<year>_prediction_<best_model_id>.tif`
- DuckDB cache: `cache/panels.duckdb`

## Reproducibility
- Dependency lockfile: `renv.lock`
- Pipeline orchestration and caching: `targets`
- Containerized environment available via `Dockerfile` and `docker-compose.yml`
- Deterministic seed from `config/global/project.yml::project.seed`

## Definition of Done
- `R -q -e "renv::restore(prompt=FALSE)"`
- `R -q -e "targets::tar_make()"`
- If tests exist: `R -q -e "testthat::test_dir('tests/testthat')"`
