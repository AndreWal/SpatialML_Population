# Data Schema

## Canonical panel entity: `country_panel` (sf; per-country output)

Produced per country after assembly, harmonization, CRS transform, QA validation, and feature extraction.

| column | type | required | notes |
|---|---|---|---|
| `country_code` | character | yes | ISO3 code from country config |
| `admin_unit_harmonized` | character | yes | canonical admin unit ID |
| `admin_name_harmonized` | character | yes | canonical admin unit name |
| `year` | integer | yes | panel year/decade |
| `population` | numeric | yes | weighted/summed during harmonization (polygon total count) |
| `elevation_mean` | numeric | yes (current config) | enabled raster feature from `config/sources/features.yml` |
| `slope_mean` | numeric | yes (current config) | mean slope in degrees per admin unit; derived from elevation DEM via `terra::terrain()` |
| `tri_mean` | numeric | yes (current config) | mean Terrain Ruggedness Index per admin unit; derived from elevation DEM via `terra::terrain()` |
| `dist_coast_km` | numeric | yes (current config) | mean cost-distance to nearest coastline (km, slope-penalised via least-cost path); Natural Earth 10m coastline |
| `dist_river_km` | numeric | yes (current config) | mean cost-distance to nearest major river (km, slope-penalised via least-cost path); HydroRIVERS v1.0, Strahler ≥ 4 |
| `soil_*` (raw SoilGrids zonal features) | numeric | yes (when SoilGrids enabled) | raw SoilGrids zonal means extracted per country before cross-country PCA; replaced later in the global/model panel by `soil_pc*` |
| `log_area` | numeric | yes | `log1p` polygon area in m^2 (canonical CRS); **polygon-level modeling only** |
| `lon` | numeric | yes | centroid longitude in `EPSG:4326` |
| `lat` | numeric | yes | centroid latitude in `EPSG:4326` |
| `geometry` | geometry | yes | geometry in canonical CRS (default `EPSG:3035`) |

Notes:
- Additional feature columns appear when enabled in `config/sources/features.yml`.
- Current default feature registry enables `elevation_mean`, `slope_mean`, `tri_mean`, `dist_coast_km`, and `dist_river_km`.
- Terrain features (`slope_mean`, `tri_mean`) are derived from the elevation DEM — no separate download.
- Water distance features (`dist_coast_km`, `dist_river_km`) use least-cost path analysis (`terra::costDist()`) with slope as friction surface.
- SoilGrids soil properties (61 raw layers across 11 properties × depths) are extracted per country, then PCA-reduced to `soil_pc1`…`soil_pcN` on the combined panel. The number of components `N` is set by the cumulative-variance threshold in `config/sources/soilgrids.yml` (default `0.95`).
- Raw `soil_*` columns are dropped after cross-country PCA; `soil_pc*` columns persist in the global panel and model matrix.
- `population` remains the source polygon count and must not be replaced by model output in constrained workflows.

Primary key:
- `country_code + admin_unit_harmonized + year`

## Global panel outputs

Combined panel is written to:
- `data/final/global_panel.gpkg`
- `data/final/global_panel.parquet`

Schema matches the country panel at the common columns, but the global panel replaces raw `soil_*` columns with `soil_pc*` after cross-country PCA (without geometry in parquet).

---

## Model matrix schemas (internal)

### `polygon_model_matrix` (legacy / benchmarking path)

`prepare_model_data()` creates the polygon-level training matrix from panel columns for model benchmarking and holdout evaluation.

Features:
- Base features: enabled registry features (currently `elevation_mean`, `slope_mean`, `tri_mean`, `dist_coast_km`, `dist_river_km`)
- Soil PCA features (auto-detected): `soil_pc1`, `soil_pc2`, …, `soil_pcN`
- Derived scalar features (auto-appended when available): `log_area`, `lon`, `lat`
- Time encoding: one-hot year dummy columns derived from observed years, e.g. `year_1850`, `year_1860`, ..., `year_2020`

Target (current config):
- `population` (default from `config/global/ml.yml::ml.target_variable`)

Legacy internal response preprocessing (benchmarking path):
- May convert to density in persons per m² using polygon area (`population / area_m2`)
- May apply `log1p(density)` when density is positively skewed
- May inverse-transform for holdout reporting

Important:
- This path is **not** sufficient by itself for mass-preserving cell-level population counts.

Complete-case filtering is applied to target + all selected feature columns.

### `cell_score_model_matrix` (primary disaggregation path; planned/target)

Training matrix for cell-level score / intensity prediction used in constrained allocation.

Typical row entity:
- one grid cell (or polygon-cell overlap record) for a given year

Columns (indicative):
- `country_code`
- `admin_unit_harmonized` (when training in constrained areas)
- `year`
- `cell_id`
- `cell_area_m2`
- `habitable_cell_area_m2` (optional)
- cell-level covariates (elevation, slope, TRI, distance-to-water, soil PCs, lon, lat, year dummies)
- optional masks/flags (land, water, no-data)

Response semantics:
- nonnegative **score / intensity proxy** (not absolute population count)
- final cell counts are obtained only after within-polygon normalization + allocation

Important support rule:
- `log_area` (polygon area) must not be used as a cell-level predictor.

---

## Polygon-grid intersection and allocation tables (internal / output)

### `polygon_grid_intersections` (tabular; internal)

One row per polygon-year × intersecting grid cell.

| column | type | required | notes |
|---|---|---|---|
| `country_code` | character | yes | ISO3 |
| `admin_unit_harmonized` | character | yes | canonical polygon ID |
| `year` | integer | yes | year/decade |
| `cell_id` | character | yes | unique grid cell identifier |
| `overlap_area_m2` | numeric | yes | exact polygon-cell overlap area in canonical CRS |
| `cell_area_m2` | numeric | yes | full cell area in canonical CRS |
| `area_share_polygon` | numeric | recommended | `overlap_area_m2 / polygon_area_m2` |
| `area_share_cell` | numeric | recommended | `overlap_area_m2 / cell_area_m2` |
| `habitable_overlap_area_m2` | numeric | optional | overlap area after land/habitable masking |
| `cell_lon` | numeric | recommended | cell centroid lon in `EPSG:4326` |
| `cell_lat` | numeric | recommended | cell centroid lat in `EPSG:4326` |

Primary key (recommended):
- `country_code + admin_unit_harmonized + year + cell_id`

### `cell_allocation` (tabular; internal or final diagnostics output)

One row per polygon-year × intersecting grid cell after score generation and normalization.

| column | type | required | notes |
|---|---|---|---|
| `country_code` | character | yes | ISO3 |
| `admin_unit_harmonized` | character | yes | canonical polygon ID |
| `year` | integer | yes | year/decade |
| `cell_id` | character | yes | grid cell ID |
| `overlap_area_m2` | numeric | yes | from intersections table |
| `score_raw` | numeric | yes | nonnegative ML/baseline score before area scaling |
| `area_denominator_m2` | numeric | yes | overlap or habitable overlap area used for allocation |
| `weight_raw` | numeric | yes | e.g. `score_raw * area_denominator_m2` |
| `weight_norm` | numeric | yes | normalized within polygon-year; sums to 1 |
| `population_polygon` | numeric | yes | source polygon count |
| `pop_allocated` | numeric | yes | allocated cell count contribution |
| `allocation_mode` | character | yes | e.g. `uniform_area`, `ml_weighted`, `fallback_uniform_area` |
| `fallback_flag` | logical | recommended | indicates fallback logic used |
| `quality_flag` | character | optional | e.g. `ok`, `zero_weight`, `no_covariates`, `empty_support` |

Primary key (recommended):
- `country_code + admin_unit_harmonized + year + cell_id`

### `allocation_diagnostics` (tabular; required when constrained mode enabled)

One row per polygon-year after constrained allocation QA.

| column | type | required | notes |
|---|---|---|---|
| `country_code` | character | yes | ISO3 |
| `admin_unit_harmonized` | character | yes | canonical polygon ID |
| `year` | integer | yes | year/decade |
| `observed_population` | numeric | yes | source polygon total |
| `allocated_population_sum` | numeric | yes | sum of `pop_allocated` over cells |
| `mass_error` | numeric | yes | `allocated_population_sum - observed_population` |
| `mass_rel_error` | numeric | yes | relative error (define denominator in implementation docs) |
| `n_cells_intersecting` | integer | yes | count of intersecting cells |
| `n_cells_positive_weight` | integer | recommended | count with `weight_norm > 0` |
| `zero_weight_fallback_used` | logical | recommended | fallback occurred |
| `qa_status` | character | yes | `pass`, `warn`, `fail` |

Primary key:
- `country_code + admin_unit_harmonized + year`

---

## ML output files

### `models/model_summary.csv` (primary comparison table)
One row per model/evaluation split.

| column | type | notes |
|---|---|---|
| `model_id` | character | configured model ID (e.g. `rf`, `xgb`, `lgbm`) |
| `engine` | character | model engine (`ranger`, `xgboost`, `lightgbm`) |
| `eval_set` | character | `spatial_cv`, `test_holdout`, or future score-model eval set labels |
| `response_kind` | character | modeled response representation (e.g. polygon benchmark response or score proxy) |
| `response_transform` | character | response transform used (`identity`, `log1p`, etc.) |
| `countries` | character | training-country set (for CV) or holdout country ISO3 |
| `n_folds` | integer | number of CV folds (`NA` for holdout row) |
| `n_obs` | integer | evaluated observations |
| `rmse` | numeric | RMSE |
| `mae` | numeric | MAE |
| `rsq` | numeric | R^2 |

### `models/cv_summary.csv`
One row per configured model with CV-only summary metrics.

| column | type | notes |
|---|---|---|
| `model_id` | character | |
| `engine` | character | |
| `eval_set` | character | usually `spatial_cv` |
| `response_kind` | character | modeled response representation |
| `response_transform` | character | transform on modeled response |
| `n_cv_folds` | integer | number of CV folds |
| `n_cv_obs` | integer | observations used for CV |
| `train_countries` | character | `+`-joined ISO3 set |
| `cv_rmse` | numeric | mean RMSE across folds |
| `cv_mae` | numeric | mean MAE across folds |
| `cv_rsq` | numeric | mean R^2 across folds |

### `models/<model_id>_folds.csv`
Per-fold CV diagnostics.

| column | type | notes |
|---|---|---|
| `model_id` | character | |
| `split_type` | character | fold strategy identifier |
| `fold` | character | fold identifier |
| `n_train` | integer | training rows in fold |
| `n_test` | integer | assessment rows in fold |
| `fold_rmse` | numeric | fold RMSE |
| `fold_mae` | numeric | fold MAE |
| `fold_rsq` | numeric | fold R^2 |

### Final model objects
- `models/<model_id>_final.rds`
- Contains fitted `parsnip`/`workflow` model trained on all non-holdout rows.

### Variable-importance plots
- `models/variable_importance/<model_id>_var_importance.png`
- Horizontal bar plot of top feature importances extracted from the final fitted model (engine-specific importance metric).

---

## Prediction raster outputs

### Explicit raster outputs (production)

#### Relative intensity raster (unconstrained or intermediate)
Path pattern:
- `data/final/predictions/global_<year>_intensity_<model_id>.tif`

Semantics:
- Cell values represent relative intensity / weighting scores (or monotone transform thereof).
- Not directly interpretable as population counts unless calibrated.

#### Constrained population count raster
Path pattern:
- `data/final/predictions/global_<year>_population_count_constrained_<model_id>.tif`

Semantics:
- Cell values are allocated population counts.
- Aggregation to source polygon-year totals is exact within numerical tolerance.
- Output CRS is canonical CRS.

#### Optional calibrated population count raster
Path pattern:
- `data/final/predictions/global_<year>_population_count_calibrated_<model_id>.tif`

Semantics:
- Cell values are population counts after calibration to external totals where polygon totals are unavailable.
- Calibration level and method must be documented in metadata/config.

#### Optional quality/flags raster
Path pattern:
- `data/final/predictions/global_<year>_quality_flag_<model_id>.tif`

Semantics:
- Encodes data support / fallback / QA states for cells.

Notes on raster feature stacks:
- Cell-level raster prediction stacks may include enabled raster features, lon/lat, and year dummy layers.
- `log_area` is not a valid cell-level predictor because it is a polygon-level support feature.

---

## DuckDB intermediate store

Database path:
- `cache/panels.duckdb`

Current tables:
- `panel_<iso3>` (lowercase country code)
- `panel_all`

Planned / recommended tables for disaggregation:
- `grid_cells` (cell index and geometry metadata; tabular attributes only in DuckDB)
- `polygon_grid_intersections`
- `cell_allocation`
- `allocation_diagnostics`

All DuckDB tables are tabular (no geometry column).

---

## Input contracts

### Tabular inputs (`inputs.tabular[]`)
Required per input config:
- `id`, `path`, `format`, `columns`

Required column mappings:
- `admin_id_raw`, `admin_name`, `year`, `population`

Supported formats:
- `csv`, `parquet`, `xlsx`

### Geometry inputs (`inputs.geometry[]`)
Required per input config:
- `id`, `path`, `format`, `columns`

Required column mappings:
- `admin_id_raw`, `admin_name`

Optional year handling:
- direct `year` column in geometry file, or
- a single value in `valid_years` / `years` / `year_filter`

Supported formats:
- `shp`/`shapefile`, `gpkg`, `geojson`

### Optional disaggregation support inputs (planned)
Examples:
- land mask / water mask
- habitable-area mask
- external control totals for calibration
- grid definition / template raster

These should be documented in config and schema once implemented.

---

## Assembly strategies
Applies to tabular and geometry recipes:
- `single`: exactly one selected input
- `by_year`: per-year mapping using `year_map`
- `stack_then_resolve`: stack selected inputs and keep first per key by input priority

## Crosswalk contract
Expected file path pattern:
- `config/crosswalks/<ISO3>.csv`

Required columns:
- `from_admin_id`, `to_admin_id`, `to_admin_name`

Optional columns:
- `weight` (default `1`)
- `valid_from_year`, `valid_to_year`

Unmatched behavior:
- Controlled by `harmonization.unmatched_policy`: `fail`, `warn`, or `drop`

## QA checks
Core QA:
- Unique key constraint
- Non-empty geometry (empty rows dropped with warning)
- Geometry validity (with optional auto-fix)
- Final CRS equals canonical CRS
- Join coverage above configured threshold
- Feature non-missing coverage above configured threshold

Additional allocation QA (when constrained mode is used):
- Polygon-year mass-preservation within tolerance
- Logged fallback handling for zero/NA support
- Diagnostics table emitted for reproducibility
