# Data Schema

## Canonical final entity: `country_panel` (sf)

Produced per country after harmonization + CRS transform + feature extraction.

| column | type | required | notes |
|---|---|---|---|
| `country_code` | character | yes | ISO3 |
| `admin_unit_harmonized` | character | yes | harmonized unit id |
| `admin_name_harmonized` | character | yes | harmonized unit name |
| `year` | integer | yes | panel year |
| `population` | numeric | yes | weighted/summed by crosswalk mapping |
| `elevation_mean` | numeric | no | mean elevation (m), from raster zonal extraction |
| `<feature_id>` | numeric | no | additional features per `config/sources/features.yml` |
| `geometry` | geometry | yes | canonical CRS |

Primary key:
- `country_code + admin_unit_harmonized + year`

## ML outputs

### CV summary (`models/cv_summary.csv`)
| column | type | notes |
|---|---|---|
| `model_id` | character | e.g. "rf", "xgb" |
| `engine` | character | e.g. "ranger", "xgboost" |
| `rmse` | numeric | root mean squared error |
| `mae` | numeric | mean absolute error |
| `rsq` | numeric | R-squared |

### Per-model fold details (`models/<model_id>_folds.csv`)
| column | type | notes |
|---|---|---|
| `model_id` | character | model identifier |
| `fold` | integer | fold number |
| `n_train` | integer | training set size |
| `n_test` | integer | test set size |
| `rmse` | numeric | fold RMSE |
| `mae` | numeric | fold MAE |
| `rsq` | numeric | fold R² |

### Final models (`models/<model_id>_final.rds`)
Serialized R model objects trained on all data.

### Prediction rasters (`data/final/<ISO3>/<ISO3>_prediction_<model_id>.tif`)
GeoTIFF raster at configured resolution (default 1000m) in canonical CRS.

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
