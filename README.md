# 🌍 Spatial Predictions

A reproducible pipeline for estimating historical sub-national population across European countries (1850–2020). It assembles harmonized admin-level panels, extracts raster features, trains gradient boosting and random forest models with spatial block cross-validation, and writes decade-by-decade prediction rasters.

---

## 🎯 What this repo does

**🔍 Problem.** Population data for historical periods is scattered across heterogeneous national archives with unstable admin boundaries. This pipeline harmonizes those records into a single panel keyed by `(country_code, admin_unit_harmonized, year)`, then uses spatially aware ML to estimate population where historical observations are absent.

**⚙️ ETL.** For each enabled country the pipeline:
1. Reads configured tabular and geometry inputs (country-specific CSV/GeoJSON/Parquet formats).
2. Resolves admin boundary changes through crosswalk tables (`config/crosswalks/<ISO3>.csv`).
3. Reprojects everything to the canonical equal-area CRS (EPSG:3035).
4. Validates geometry and QA rules (unique keys, join coverage, CRS).
5. Extracts configured raster features per admin polygon (elevation, terrain, water-distance, and SoilGrids-derived features), plus derived geometric features.
6. Writes harmonized outputs to `data/final/<ISO3>/`.

**🤖 Machine learning.** Once all country panels are assembled:
- Spatial block CV is created with `spatialsample::spatial_block_cv` (5-fold by default) on the training countries.
- Three model engines are trained and evaluated: `ranger`, `xgboost`, `lightgbm`.
- The best model by CV RMSE is selected and evaluated on a spatial holdout set (DEU).
- Metrics are saved to `models/model_summary.csv` with an `eval_set` column that distinguishes `spatial_cv` from `test_holdout` rows.

**🗺️ Raster predictions.** The best model is applied to a regular prediction grid (default 1 km resolution in EPSG:3035) for every decade in the panel, producing 20 GeoTIFFs at `data/final/predictions/global_<year>_prediction_<model>.tif`.

---

## 📊 Feature set

| Feature | Source | Notes |
|---|---|---|
| `elevation_mean` | ⛰️ DEM raster, zonal mean per polygon | configured in `config/sources/elevation.yml` |
| `slope_mean`, `tri_mean` | 🏔️ Terrain derivatives from DEM | configured in `config/sources/terrain.yml` |
| `dist_coast_km`, `dist_river_km` | 🌊 Terrain-weighted least-cost distance to water | configured in `config/sources/water_distance.yml` |
| `soil_pc1` … `soil_pcN` | 🌱 PCA components from SoilGrids zonal means | configured in `config/sources/soilgrids.yml` |
| `log_area` | 📐 `sf::st_area()` → log1p | computed in EPSG:3035 (equal-area) |
| `lon`, `lat` | 📍 WGS84 centroid coordinates | reprojected to EPSG:4326 for extraction |
| `year_1850` … `year_2020` | 📅 Binary one-hot decade dummies | one column per decade present in the panel |

---

## 📦 Outputs

| Path | Description |
|---|---|
| `data/final/<ISO3>/<ISO3>_panel.gpkg` | Harmonized country panel (vector) |
| `data/final/<ISO3>/<ISO3>_panel.parquet` | Same panel in columnar format |
| `data/final/global_panel.gpkg` / `.parquet` | All countries combined |
| `data/final/predictions/global_<year>_prediction_<model>.tif` | Decade population raster |
| `models/model_summary.csv` | CV + holdout metrics in one table |
| `models/cv_summary.csv` | CV summary metrics per model |
| `models/<model_id>_folds.csv` | Per-fold CV diagnostics |

---

## 🚀 Quickstart

### 🐳 Docker (recommended — full environment)

```bash
docker compose run --rm pipeline
```

Raw data must be mounted before the pipeline can run:
```yaml
# docker-compose.yml already mounts ./data/raw → /project/data/raw (read-only)
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

---

## ⚙️ Configuration

| File / directory | Purpose |
|---|---|
| `config/global/project.yml` | Enabled countries, project seed |
| `config/global/crs.yml` | Canonical CRS (EPSG:3035) |
| `config/global/ml.yml` | Model specs, CV folds, holdout country, raster resolution |
| `config/global/qa.yml` | QA thresholds (join coverage, geometry rules) |
| `config/global/paths.yml` | Output directory layout |
| `config/countries/<ISO3>.yml` | Per-country input paths, column mappings, assembly recipe |
| `config/crosswalks/<ISO3>.csv` | Admin boundary crosswalk (from/to unit, weight) |
| `config/sources/features.yml` | Feature registry (id, source config, enabled flag) |
| `config/sources/<feature>.yml` | Raster source config (raw/processed paths, zonal stat) |

---

## ➕ Adding a country

1. Place tabular and geometry files in `data/raw/<ISO3>/`.
2. Create `config/countries/<ISO3>.yml` following the existing DEU/NLD templates.
3. Optionally add `config/crosswalks/<ISO3>.csv` if admin boundaries changed over time.
4. Add `<ISO3>` to `config/global/project.yml::countries.enabled`.
5. Run `targets::tar_make()` — only the new-country targets are (re-)built.

---

## 🔁 Reproducibility

- **Packages**: `renv.lock` pins all R package versions.
- **Environment**: `Dockerfile` + `docker-compose.yml` (`rocker/geospatial:4.5.2`).
- **Pipeline**: `targets` with dynamic branching per country; stale targets are detected and rebuilt automatically.
- **Seed**: set in `config/global/project.yml::project.seed` and forwarded to all random operations.
- **CI**: GitHub Actions (`.github/workflows/ci.yml`) runs the test suite on every push.

---

## 📁 Repository layout

```
_targets.R              # Pipeline definition (all targets)
R/                      # Pure functions: IO, ETL, features, models, evaluation
config/                 # All configuration (no hardcoded paths in code)
data/raw/               # Source data — not committed, mounted at runtime
data/final/             # Pipeline outputs (panels, rasters)
models/                 # Trained models (.rds) and metrics (.csv)
docs/                   # PROJECT_SPEC.md, DATA_SCHEMA.md, FEATURE_SOURCES.md
tests/testthat/         # Unit + integration tests
```

See `docs/PROJECT_SPEC.md` for the full pipeline contract and `docs/DATA_SCHEMA.md` for the output column schema.
