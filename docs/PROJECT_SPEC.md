# Project Spec: Multi-country Geospatial ETL + Spatial ML (R)

## Objective
Build a reproducible, multi-country geospatial dataset and train spatially aware ML models to produce raster prediction maps.

## Scope
- Inputs: country tabular + country geometry configured in `config/countries/*.yml`.
- Feature engineering: raster-based zonal features extracted per admin polygon.
- ML: spatial block cross-validation with ranger and xgboost.
- Outputs: harmonized country panels (`.gpkg` + `.parquet`), trained models (`.rds`), prediction rasters (`.tif`).
- Tracking: optional MLflow experiment logging.
- Storage: DuckDB intermediate store for tabular analytics.
- Execution: `targets` branch-per-country pipeline.

## Runtime behavior
- Enabled countries come from `config/global/project.yml::countries.enabled`.
- Missing raw files fail the run — no mock fallback exists.

## Country pipeline contract
For each country, pipeline runs these pure stages:
1. Read/assemble configured tabular + geometry inputs
2. Harmonize keys via crosswalk (`config/crosswalks/<ISO3>.csv`)
3. Transform to canonical CRS (`config/global/crs.yml::crs.canonical`)
4. Validate and fix geometry (`validate_and_fix_geometry`)
5. Validate QA rules
6. Extract raster features (zonal statistics per polygon)
7. Write `.gpkg` + `.parquet`

## ML pipeline (cross-country)
After per-country ETL completes:
1. Combine all country panels with features
2. Create spatial block CV folds (`config/global/ml.yml`)
3. Train models (ranger, xgboost) with spatial CV
4. Evaluate and select best model (RMSE, MAE, R²)
5. Log runs to MLflow (optional)
6. Predict onto raster grid per country (`.tif`)

## QA rules (must pass)
- Unique key: (`country_code`, `admin_unit_harmonized`, `year`)
- No empty geometry
- Valid geometry (auto-fixed via `st_make_valid` if configured)
- Final CRS equals canonical CRS
- Join coverage >= threshold from `config/global/qa.yml` or country override

## Reproducibility
- `renv.lock` for package versions
- Docker (`Dockerfile` + `docker-compose.yml`) for full environment
- `targets` for orchestration
- No manual production steps
- Explicit seed via `config/global/project.yml`
- GitHub Actions CI (`.github/workflows/ci.yml`)

## Definition of Done
- `R -q -e "renv::restore(prompt=FALSE)"` succeeds
- `R -q -e "targets::tar_make()"` succeeds
- `R -q -e "testthat::test_dir('tests/testthat')"` succeeds
