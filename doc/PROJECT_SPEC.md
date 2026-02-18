# Project Spec: Multi-country Geospatial ETL + Spatial ML (R)

## Objective
Build a reproducible, multi-country geospatial dataset and train spatially aware ML models to produce raster prediction maps.

## Scope
- Inputs:
  - Country-specific tabular demographic data (decadal observations).
  - Country-specific administrative polygons (usually shapefiles).
- Outputs:
  - Harmonized vector dataset (admin units + population size for multiple years + engineered features).
  - Model-ready table and trained model artifacts.
  - Predicted raster maps.

## Data Contracts

### Required tabular columns after subsetting
- `country_code`
- `admin_unit_raw`
- `year`
- `population`

### Harmonized admin key
- `admin_unit_harmonized`
- Crosswalk source: `config/admin_crosswalks/<country>.csv`
- If many-to-one merge occurs, aggregate numeric values by harmonized unit and year.

### Spatial CRS standard
- Canonical CRS: `EPSG:<SET_THIS_ONCE>`
- All vector layers must be transformed to canonical CRS before joins.

## Pipeline Stages (targets)
1. Ingest country tabular + polygons
2. Subset tabular to required columns
3. Harmonize admin units using country crosswalk
4. Aggregate tabular where harmonization merges units
5. Read and transform polygons to canonical CRS
6. Join attributes to geometry
7. Build engineered features (local files or remote sources)
8. Validate contracts and QA checks
9. Write final artifacts
10. Train spatial ML and generate raster predictions

## Output Formats
- Vector outputs: GeoPackage (`.gpkg`)
- Raster outputs: GeoTIFF (`.tif`)
- Large non-spatial feature tables: Parquet (`.parquet`)
- Optional analytics DB: DuckDB file (`.duckdb`)

## Reproducibility Contract
- Environment: Docker image
- R packages: `renv.lock`
- Workflow orchestration: `targets`
- No manual steps in production pipeline
- All random processes use explicit seeds

## Validation Rules (must pass)
- No duplicate (`country_code`, `admin_unit_harmonized`, `year`)
- No missing geometry in final vector output
- CRS equals canonical CRS for all vector outputs
- Join coverage >= threshold (set per country in config)
- Features documented in `docs/FEATURE_SOURCES.md`

## ML Rules
- Use spatially aware resampling / CV and spatially aware feature selection
- Track metrics by country and globally
- Save model metadata, features used, and training timestamp by using mlflow

## Definition of Done
- `targets::tar_make()` runs end-to-end on a clean machine/container
- Outputs recreated bitwise or within defined tolerance
- README quickstart works without manual intervention
