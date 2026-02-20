# Data Schema

## Canonical panel entity: `country_panel` (sf)

Produced per country after assembly, harmonization, CRS transform, QA validation, and feature extraction.

| column | type | required | notes |
|---|---|---|---|
| `country_code` | character | yes | ISO3 code from country config |
| `admin_unit_harmonized` | character | yes | canonical admin unit ID |
| `admin_name_harmonized` | character | yes | canonical admin unit name |
| `year` | integer | yes | panel year/decade |
| `population` | numeric | yes | weighted/summed during harmonization |
| `elevation_mean` | numeric | yes (current config) | enabled raster feature from `config/sources/features.yml` |
| `log_area` | numeric | yes | `log1p` polygon area in m^2 (canonical CRS) |
| `lon` | numeric | yes | centroid longitude in EPSG:4326 |
| `lat` | numeric | yes | centroid latitude in EPSG:4326 |
| `geometry` | geometry | yes | geometry in canonical CRS (default EPSG:3035) |

Notes:
- Additional feature columns appear when enabled in `config/sources/features.yml`.
- Current default feature registry enables `elevation_mean` only.

Primary key:
- `country_code + admin_unit_harmonized + year`

## Global panel outputs

Combined panel is written to:
- `data/final/global_panel.gpkg`
- `data/final/global_panel.parquet`

Schema matches country panel columns (without geometry in parquet).

## Model matrix schema (internal)

`prepare_model_data()` creates the training matrix from panel columns:
- Base features: enabled registry features (currently `elevation_mean`)
- Derived scalar features (auto-appended when available): `log_area`, `lon`, `lat`
- Time encoding: one-hot year dummy columns derived from observed years, e.g. `year_1850`, `year_1860`, ..., `year_2020`

Target:
- `population` (default from `config/global/ml.yml::ml.target_variable`)

Complete-case filtering is applied to target + all selected feature columns.

## ML output files

### `models/model_summary.csv` (primary comparison table)
One row per model/evaluation split.

| column | type | notes |
|---|---|---|
| `model_id` | character | configured model ID (e.g. `rf`, `xgb`, `lgbm`) |
| `engine` | character | model engine (`ranger`, `xgboost`, `lightgbm`) |
| `eval_set` | character | `spatial_cv` or `test_holdout` |
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
| `eval_set` | character | always `spatial_cv` |
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
| `split_type` | character | always `spatial_cv_validation` |
| `fold` | character | fold identifier from rsample/spatialsample |
| `n_train` | integer | training rows in fold |
| `n_test` | integer | assessment rows in fold |
| `fold_rmse` | numeric | fold RMSE |
| `fold_mae` | numeric | fold MAE |
| `fold_rsq` | numeric | fold R^2 |

### Final model objects
- `models/<model_id>_final.rds`
- Contains fitted `parsnip`/`workflow` model trained on all non-holdout rows.

## Prediction raster outputs

Path pattern:
- `data/final/predictions/global_<year>_prediction_<model_id>.tif`

Semantics:
- One raster per unique year/decade in `combined_panel$year`.
- Raster feature stack includes enabled raster features plus constant layers for `log_area`, `lon`, `lat`, and year dummy layers matching the trained model feature names.
- Output CRS is canonical CRS.

## DuckDB intermediate store

Database path:
- `cache/panels.duckdb`

Tables:
- `panel_<iso3>` (lowercase country code)
- `panel_all`

All DuckDB tables are tabular (no geometry column).

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
- Unique key constraint
- Non-empty geometry (empty rows dropped with warning)
- Geometry validity (with optional auto-fix)
- Final CRS equals canonical CRS
- Join coverage above configured threshold
