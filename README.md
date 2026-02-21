# 🌍 Spatial Predictions

A reproducible geospatial ETL and spatial machine learning workflow for historical sub-national population estimation in Europe (1850-2020).

## 🧪 Project

This repository supports comparative population reconstruction across countries with heterogeneous historical records. National tabular and boundary datasets are harmonized into a common panel, enriched with geospatial predictors, and used to train spatially aware predictive models.

Primary goal:
- estimate population patterns for administrative units and periods where direct observations are sparse, inconsistent, or missing.

## ⚙️ Method overview

### 1) Country-level harmonization
For each enabled country, the pipeline:
1. Loads configured tabular and geometry inputs.
2. Harmonizes changing administrative units via optional crosswalks.
3. Reprojects to a shared canonical CRS (`EPSG:3035` by default).
4. Applies QA checks (key uniqueness, geometry validity, CRS consistency, join coverage).
5. Extracts configured raster covariates and adds derived geometric covariates.
6. Writes country panel outputs.

### 2) Cross-country modeling
After country panels are assembled, the workflow:
1. Combines country panels into a global panel.
2. Constructs SoilGrids PCA features (`soil_pc1` to `soil_pcN`).
3. Creates spatial block cross-validation folds.
4. Trains and tunes `ranger`, `xgboost`, and `lightgbm` models.
5. Selects the best model by cross-validated RMSE.
6. Evaluates final performance on a holdout country (`DEU` by default).
7. Generates year/decade-specific prediction rasters.

## 🌐 Included countries (current config)

The table below reflects `countries.enabled` in `config/global/project.yml` and the corresponding country config files.

| ISO3 | Country | Time frame in panel | Administrative unit |
|---|---|---|---|
| `DEU` | Germany | `1890`, `1900`, `1910` | Harmonized electoral districts (`target_unit_id: ADM_HARM_DEU_V1`; raw ID: `Wahlkreis_Nummer`) |
| `NLD` | Netherlands | `1850`, `1860`, `1870`, `1880`, `1890`, `1900`, `1910`, `1920`, `1930`, `1940`, `1950`, `1960`, `1970`, `1990`, `2000`, `2010`, `2020` | Harmonized municipalities (`target_unit_id: ADM_HARM_NLD_V1`; raw ID: `GMDNR`) |

## 🧭 Predictor set

Configured raster predictors:
- `elevation_mean`
- `slope_mean`
- `tri_mean`
- `dist_coast_km`
- `dist_river_km`

Additional modeling covariates:
- `soil_pc1` to `soil_pcN` (SoilGrids PCA)
- `log_area`
- `lon`, `lat`
- `year_<value>` dummy variables

Source definitions, provenance, and licensing are documented in `docs/FEATURE_SOURCES.md`.

## 📦 Outputs

Core output artifacts:
- `data/final/<ISO3>/<ISO3>_panel.gpkg`
- `data/final/<ISO3>/<ISO3>_panel.parquet`
- `data/final/global_panel.gpkg`
- `data/final/global_panel.parquet`
- `data/final/predictions/global_<year>_prediction_<model_id>.tif`
- `models/model_summary.csv`
- `models/cv_summary.csv`
- `models/<model_id>_folds.csv`
- `models/<model_id>_final.rds`

Column-level schema is specified in `docs/DATA_SCHEMA.md`.

## 🚀 Running the pipeline

### 🐳 Docker

```bash
docker compose run --rm pipeline
```

### 💻 Local

```bash
R -q -e "renv::restore(prompt=FALSE)"
R -q -e "targets::tar_make()"
```

### 🧪 Tests

```bash
R -q -e "testthat::test_dir('tests/testthat')"
```

## 🗂️ Configuration structure

- `config/global/project.yml`: enabled countries, project seed
- `config/global/crs.yml`: canonical CRS
- `config/global/ml.yml`: model setup, CV, holdout country, raster prediction settings
- `config/global/qa.yml`: QA thresholds and behavior
- `config/global/paths.yml`: output directory settings
- `config/countries/<ISO3>.yml`: country-specific input mappings and assembly logic
- `config/crosswalks/<ISO3>.csv`: harmonization crosswalk tables
- `config/sources/features.yml`: active feature registry
- `config/sources/*.yml`: source-specific acquisition and processing settings

## ➕ Adding a country case

1. Add raw files under `data/raw/<ISO3>/`.
2. Add `config/countries/<ISO3>.yml`.
3. Add `config/crosswalks/<ISO3>.csv` if harmonization is required.
4. Add `<ISO3>` under `countries.enabled` in `config/global/project.yml`.
5. Re-run `targets::tar_make()`.

## 🔁 Reproducibility

- Dependency versions are locked in `renv.lock`.
- Workflow orchestration and caching are managed by `targets`.
- Containerized execution is defined by `Dockerfile` and `docker-compose.yml`.
- Pipeline behavior is config-driven and country-extensible.

## 📁 Repository layout

```text
_targets.R              Pipeline definition
R/                      Pipeline functions
config/                 Global, country, and source configuration
data/raw/               Input data (not committed)
data/final/             Final outputs
models/                 Trained models and evaluation artifacts
docs/                   Project spec, schema, and feature source documentation
tests/testthat/         Test suite
```

## 📚 Canonical documentation

Use these files as source of truth:
1. `docs/PROJECT_SPEC.md`
2. `docs/DATA_SCHEMA.md`
3. `docs/FEATURE_SOURCES.md`
