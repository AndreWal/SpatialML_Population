# Project Spec: Multi-country Geospatial ETL + Spatial ML + Population Disaggregation (R)

## Objective
Build a reproducible, multi-country geospatial dataset and train spatially aware ML models to produce gridded population surfaces.

The pipeline supports two output modes:
1. **Constrained disaggregation** (preferred where polygon totals exist): ML produces a cell-level weighting/intensity surface, and known polygon population totals are redistributed to cells so polygon sums are preserved.
2. **Unconstrained intensity prediction** (fallback where no polygon totals exist): ML produces a relative intensity surface that is not interpreted as population counts unless calibrated to an external total.

## Scope
- Inputs are country-level tabular and geometry sources configured in `config/countries/*.yml`.
- Harmonization maps raw admin IDs to canonical units via optional country crosswalks.
- Feature engineering extracts raster zonal statistics from enabled sources in `config/sources/features.yml`.
- ML trains and compares tree-based models with spatial CV for polygon-level benchmarking and/or score modeling.
- Outputs include per-country and global panels (`.gpkg` + `.parquet`), model artifacts (`.csv`, `.rds`), DuckDB intermediate tables, and raster products (`.tif`).
- Population rasters are explicitly labeled as **constrained counts** or **relative intensity**.

## Canonical orchestration
The pipeline is orchestrated by `targets` in `_targets.R` and sources functions from (current + planned modules):
- `R/config_loader.R`
- `R/io_readers.R`
- `R/assembly.R`
- `R/pipeline_country.R`
- `R/feature_extraction.R`
- `R/soilgrids.R`
- `R/duckdb_store.R`
- `R/spatial_cv.R`
- `R/model_training.R`
- `R/model_evaluation.R`
- `R/mlflow_utils.R`
- `R/raster_predict.R` (legacy direct raster prediction and/or score raster generation)
- `R/grid_intersections.R` (polygon-grid overlap generation)
- `R/dasymetric_allocation.R` (constrained redistribution and semantic raster writers)
- `R/allocation_validation.R` (mass-preservation checks and diagnostics)

## Runtime behavior
- Enabled countries are read from `config/global/project.yml` at `countries.enabled`.
- Current default enabled list is `DEU`, `NLD`.
- Missing configured raw inputs fail the run (`read_country_inputs()` has no fallback/mock mode).
- Canonical CRS is `config/global/crs.yml::crs.canonical` (current default `EPSG:3035`).
- Geographic CRS for lon/lat exports is `config/global/crs.yml::crs.geographic` (current default `EPSG:4326`).
- Canonical path roots are defined in `config/global/paths.yml` (`data/final`, `models`, `cache`, etc.).

## Country ETL contract
For each enabled country branch:
1. Validate and load country config.
2. Assemble tabular and geometry inputs (`single`, `by_year`, or `stack_then_resolve`).
3. Harmonize keys via crosswalk (or identity mapping when no crosswalk is configured).
4. Transform to canonical CRS.
5. Validate/fix geometry (`st_make_valid` when configured).
6. Run QA checks (unique key, CRS, join coverage, non-empty geometry handling).
7. Extract enabled raster features and append derived geometric features:
   - `log_area` (`log1p(st_area)` in canonical CRS) **for polygon-level modeling only**
   - `lon`, `lat` (centroid coordinates in WGS84)
   - SoilGrids zonal means (61 raw layers; PCA-reduced after panel combination)
8. Write country outputs:
   - `data/final/<ISO3>/<ISO3>_panel.gpkg`
   - `data/final/<ISO3>/<ISO3>_panel.parquet`

## Cross-country panel contract
After country ETL:
1. Combine all country panels to one global `sf` panel.
2. Fit PCA on raw soil feature columns (`soil_*`), transform panel to replace `soil_*` with `soil_pc*` components (variance threshold configurable, default 95%).
3. Persist global outputs:
   - `data/final/global_panel.gpkg`
   - `data/final/global_panel.parquet`
4. Store tabular panels in DuckDB (`cache/panels.duckdb`):
   - `panel_<iso3>` tables
   - `panel_all`

## ML and disaggregation contract

### A. Polygon-level ML benchmark (current/legacy-compatible path)
This path is useful for model comparison and holdout-country evaluation at polygon support.

1. Split into training vs holdout country (`config/global/ml.yml::ml.holdout_test_country`, current default `DEU`).
2. Prepare polygon model matrix from enabled feature columns plus derived columns and year dummies.
3. Create spatial CV folds using configured split method (`ml.split.method`, current config: `spatial_cv_cast`).
4. Train/tune configured models (`ranger`, `xgboost`, `lightgbm`) on a model response derived from the target:
   - target variable is currently `population` (`config/global/ml.yml::ml.target_variable`)
   - internal response may be transformed to density and optionally `log1p` for benchmarking
5. Save CV summaries, fold metrics, and final model objects.
6. Evaluate best model on the holdout country.
7. Write combined model summary CSV.
8. Optionally log runs to MLflow.

### B. Constrained population disaggregation (target production path)
This is the primary path for generating population count rasters where polygon totals are available.

Current implementation status:
- Baseline constrained allocation is implemented using exact overlap-area weights (`uniform_area`).
- ML-weighted constrained allocation is implemented via a score-proxy model branch that excludes polygon-only predictors (notably `log_area`) from raster score generation.
- Mass-preservation diagnostics are emitted and enforced in the pipeline.
- Optional calibrated count outputs are supported when calibration totals are provided in config.

1. Build (or reuse) a prediction grid in canonical CRS at configured resolution (`ml.raster_prediction.resolution_m`, current default `1000 m`).
2. Compute polygon-grid intersections (exact overlap area in canonical CRS).
3. Prepare cell-level covariates for intersecting cells.
4. Generate a **nonnegative cell-level score / intensity** surface:
   - baseline (uniform area weighting), and/or
   - ML-weighted score model using cell covariates
5. Convert scores to allocation weights within each polygon-year:
   - `weight_raw = f(score, covariates) * area_denominator`
   - normalize so weights sum to 1 within each polygon-year
6. Allocate polygon totals to cells:
   - `pop_allocated_cell = population_polygon * weight_norm`
7. Validate mass preservation:
   - sum of allocated cells must equal source polygon population within tolerance
8. Write constrained raster outputs and diagnostics.

### C. Unconstrained prediction for no-polygon areas
When no polygon totals exist for an area/year:
1. Predict cell-level relative intensity from cell covariates.
2. Write output as a **relative intensity raster** (not population count) unless calibrated to external totals.
3. If calibration totals are available (e.g., region/country level), scale the intensity surface to match those totals and label the output as calibrated.

Current optional calibration config hook:
- Preferred: `config/global/allocation.yml::allocation.calibration.enabled`
- Preferred: `config/global/allocation.yml::allocation.calibration.totals_by_year` (named mapping of `year -> total`)
- Backward-compatible fallback: `config/global/ml.yml::ml.calibration.*`

## Config-driven model setup (current committed defaults)
From `config/global/ml.yml`:
- Target variable: `population`
- Feature set label: `default_v1`
- Holdout test country: `DEU`
- Split method: `spatial_cv_cast`
- CV folds: `5`
- Repeats: `1`
- Spatial block size: `100 km`
- Models configured: `rf` (`ranger`), `xgb` (`xgboost`), `lgbm` (`lightgbm`)
- Evaluation metrics: `rmse`, `mae`, `rsq`
- Save fold predictions: enabled
- Raster prediction resolution: `1000 m`
- Clamp predictions to training response range: enabled

## QA rules
Global QA config in `config/global/qa.yml` plus optional country overrides:
- Unique key columns (default): `country_code`, `admin_unit_harmonized`, `year`
- Geometry validity requirement and auto-fix behavior
- Empty geometries are dropped with warning
- Final CRS must equal canonical CRS
- Join coverage minimum threshold: `qa.coverage.join_coverage_min` (current default `0.98`)
- Feature non-missing minimum: `qa.coverage.feature_non_missing_min` (current default `0.95`)
- Allowed years list in `qa.temporal.allowed_years`
- Population minimum bound: `qa.ranges.population_min` (current default `1`)

Additional QA for constrained disaggregation (required when enabled):
- Polygon-year mass-preservation check (`allocated_sum - observed_population`)
- Relative mass error tolerance (documented in config/spec)
- Zero/NA support fallback handling logged
- Coverage of intersected cells per polygon-year logged

## Output contract (high level)

### Core panel outputs
- Country panels: `data/final/<ISO3>/<ISO3>_panel.gpkg`, `.parquet`
- Global panel: `data/final/global_panel.gpkg`, `.parquet`

### Model artifacts
- `models/cv_summary.csv`
- `models/<model_id>_folds.csv`
- `models/<model_id>_final.rds`
- `models/model_summary.csv`
- `models/variable_importance/<model_id>_var_importance.png`

### Raster outputs (explicit semantics)
- **Relative intensity / score raster (unconstrained or intermediate)**  
  `data/final/predictions/global_<year>_intensity_<model_id>.tif`
- **Constrained population count raster (mass-preserving where polygon totals exist)**  
  `data/final/predictions/global_<year>_population_count_constrained_<model_id>.tif`
- **Optional calibrated population count raster (external-total calibrated)**  
  `data/final/predictions/global_<year>_population_count_calibrated_<model_id>.tif`
- **Optional diagnostics / flags raster**  
  `data/final/predictions/global_<year>_quality_flag_<model_id>.tif`

### Tabular allocation diagnostics (required when constrained mode enabled; implemented)
- `data/final/diagnostics/allocation_diagnostics_<year>_<model_id>.csv` (pipeline default; `.parquet` optional)
- `data/final/diagnostics/allocation_diagnostics_all_<model_id>.csv` (pipeline default; `.parquet` optional)

### DuckDB cache
- `cache/panels.duckdb`

## Reproducibility
- Dependency lockfile: `renv.lock`
- Pipeline orchestration and caching: `targets`
- Containerized environment available via `Dockerfile` and `docker-compose.yml`
- Deterministic seed from `config/global/project.yml::project.seed` (current value `20260216`)
- Project timezone from `config/global/project.yml::project.timezone` (current value `Europe/Zurich`)

## Definition of Done
- `R -q -e "renv::restore(prompt=FALSE)"`
- `R -q -e "targets::tar_make()"`
- If tests exist: `R -q -e "testthat::test_dir('tests/testthat')"`
- If constrained allocation is produced:
  - mass-preservation checks pass within tolerance
  - constrained vs unconstrained outputs are clearly labeled
  - `docs/DATA_SCHEMA.md` and `docs/FEATURE_SOURCES.md` reflect any new tables/features
