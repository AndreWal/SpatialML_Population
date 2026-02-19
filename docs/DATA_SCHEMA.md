# Data Schema

## Canonical final entity: `country_panel` (sf)

Produced per country after harmonization + CRS transform + feature extraction.

| column | type | required | notes |
|---|---|---|---|
| `country_code` | character | yes | ISO3 |
| `admin_unit_harmonized` | character | yes | harmonized unit id |
| `admin_name_harmonized` | character | yes | harmonized unit name |
| `year` | integer | yes | panel year; used as categorical ML feature |
| `population` | numeric | yes | weighted/summed by crosswalk mapping |
| `elevation_mean` | numeric | no | mean elevation (m), from raster zonal extraction |
| `log_area` | numeric | yes | log1p of polygon area in m² (canonical CRS); derived via `sf::st_area()` |
| `lon` | numeric | yes | WGS84 longitude of polygon centroid (degrees) |
| `lat` | numeric | yes | WGS84 latitude of polygon centroid (degrees) |
| `<feature_id>` | numeric | no | additional features per `config/sources/features.yml` |
| `geometry` | geometry | yes | canonical CRS |

Primary key:
- `country_code + admin_unit_harmonized + year`

## ML outputs

### `models/model_summary.csv` — **primary model comparison file**
Long-format table: one row per model per evaluation split. The `eval_set` column
makes the data provenance unambiguous.

| column | type | notes |
|---|---|---|
| `model_id` | character | e.g. "rf", "xgb", "cat" |
| `engine` | character | e.g. "ranger", "xgboost", "catboost" |
| `eval_set` | character | `"spatial_cv"` = held-out CV folds on training countries; `"test_holdout"` = never-seen holdout country |
| `countries` | character | countries contributing observations to this evaluation (e.g. "NLD" for CV, "DEU" for test) |
| `n_folds` | integer | number of spatial CV folds; `NA` for test_holdout |
| `n_obs` | integer | total observations evaluated |
| `rmse` | numeric | RMSE for this eval_set |
| `mae` | numeric | MAE for this eval_set |
| `rsq` | numeric | R² for this eval_set |

### `models/cv_summary.csv` — spatial CV metrics only
| column | type | notes |
|---|---|---|
| `model_id` | character | |
| `engine` | character | |
| `eval_set` | character | always `"spatial_cv"` |
| `n_cv_folds` | integer | number of CV folds |
| `n_cv_obs` | integer | training observations used |
| `train_countries` | character | `+`-joined ISO3 codes |
| `cv_rmse` | numeric | mean RMSE across held-out folds |
| `cv_mae` | numeric | mean MAE across held-out folds |
| `cv_rsq` | numeric | mean R² across held-out folds |

### `models/<model_id>_folds.csv` — per-fold detail
| column | type | notes |
|---|---|---|
| `model_id` | character | |
| `split_type` | character | always `"spatial_cv_validation"` |
| `fold` | integer | fold number |
| `n_train` | integer | training set size for this fold |
| `n_test` | integer | validation set size for this fold |
| `fold_rmse` | numeric | RMSE on the held-out fold |
| `fold_mae` | numeric | MAE on the held-out fold |
| `fold_rsq` | numeric | R² on the held-out fold |

### Final models (`models/<model_id>_final.rds`)
Serialized R model objects trained on **all non-holdout data**.

### Prediction rasters (`data/final/predictions/global_<decade>_prediction_<model_id>.tif`)
One GeoTIFF per decade (e.g. `global_1850_prediction_rf.tif` … `global_2020_prediction_rf.tif`).
Decade = `floor(year / 10) * 10`. Each raster covers all enabled countries at the
configured resolution (default 1000 m) in canonical CRS. The constant `year`,
`lat`, `lon`, and `log_area` feature layers are baked into every raster before
prediction.

## DuckDB intermediate store (`cache/panels.duckdb`)
Tables: `panel_<iso3>` (per country), `panel_all` (combined).
Contains tabular columns only (no geometry).

## Input contracts

### Tabular input (`inputs.tabular[]`)
Required fields in each config entry:
- `id`, `path`, `format`, `columns`

Required column mappings:
- `admin_id_raw`, `admin_name`, `year`, `population`

Supported formats:
- `csv`, `parquet`, `xlsx`

### Geometry input (`inputs.geometry[]`)
Required fields in each config entry:
- `id`, `path`, `format`, `columns`

Required column mappings:
- `admin_id_raw`, `admin_name`

Supported formats:
- `shp`/`shapefile`, `gpkg`, `geojson`

## Assembly strategies
Applies to `tabular_recipe.strategy` and `geometry_recipe.strategy`:
- `single`: use exactly one input
- `by_year`: use `year_map` to pick input per year
- `stack_then_resolve`: stack inputs and resolve duplicates by first input priority

## Crosswalk contract
Expected columns in `config/crosswalks/<ISO3>.csv`:
- `from_admin_id`, `to_admin_id`, `to_admin_name`

Optional columns:
- `weight` (default `1`)
- `valid_from_year`, `valid_to_year`

Unmatched behavior controlled by `harmonization.unmatched_policy`:
- `fail`, `warn`, `drop`

## QA checks
- Unique key constraint
- Non-empty geometry
- CRS equals canonical CRS
- Join coverage >= configured threshold
