# Feature Sources

## Purpose
Canonical registry of engineered predictors used by the pipeline.

When feature behavior changes, update:
1. `docs/FEATURE_SOURCES.md`
2. `config/sources/features.yml`
3. The referenced source config file(s) in `config/sources/*.yml`

## Global feature rules
1. `feature_id` values must be unique.
2. Each enabled feature must reference a valid `source_config` file.
3. Extraction method and transformation must be explicit.
4. Missing-value policy must be explicit.
5. Source licensing/provenance must be documented.

## Active feature registry (current code + config)

This table mirrors `config/sources/features.yml` as currently configured.

| feature_id | enabled | type | source_id | source_config | extraction | transform | missing_policy |
|---|---|---|---|---|---|---|---|
| `elevation_mean` | true | `raster_zonal` | `elevation` | `config/sources/elevation.yml` | zonal mean (`processing.zonal_stat`) | none | keep NA |

Notes:
- Current production runs only include `elevation_mean` from the registry.
- Derived geometric predictors `log_area`, `lon`, and `lat` are added in code (`add_geometric_features()`) and are not configured in `features.yml`.
- Year is encoded during model prep as dummy features `year_<value>`; these are generated from panel data, not from source configs.

## Source record: `elevation_mean`

- Description: Mean elevation within each harmonized admin unit geometry.
- Model role: predictor.
- Source dataset: WorldClim v2.1 elevation raster.
- Provider: UC Davis (as declared in source config).
- Acquisition mode: download (zip archive), then unzip and process.
- Source config file: `config/sources/elevation.yml`.
- Source URL: `https://geodata.ucdavis.edu/climate/worldclim/2_1/base/wc2.1_30s_elev.zip`
- Source format: `zip` (contains `.tif`).
- Source version: `2.1`.
- License: `CC BY 4.0`.
- Raw storage path: `data/raw/global/elevation/copernicus_dem_glo30/` (config path; loader can also read from processed path).
- Processed raster path: `data/intermediate/features/elevation_10m.tif`.
- Spatial processing:
  - Input CRS expected in source config: `EPSG:4326`
  - Cropped to configured study bbox: `[-10, 40, 20, 60]` (WGS84)
  - Reprojected to canonical CRS (`config/global/crs.yml`, default `EPSG:3035`)
  - Extraction method: polygon zonal mean (`terra::extract(..., fun = mean, exact = TRUE)`)
- Temporal characteristics: static (applies to all years).
- Missing-data handling: `NA` values are retained.
- Quality checks:
  - Source config must exist and be readable.
  - Raster must be discoverable from processed path or raw path.
  - Raster is reprojected if CRS differs from canonical CRS.

## Planned but currently disabled features

Present as commented examples in `config/sources/features.yml` (not active in pipeline unless enabled):
- `slope_mean` via `config/sources/elevation.yml`
- `climate_temp_mean` via `config/sources/climate.yml`
- `nightlights_mean` via `config/sources/nightlights.yml`
- `travel_time_city_mean` via `config/sources/accessibility.yml`

These do not currently contribute columns to model training/output schemas.

## Change log
- `2026-02-20`: Replaced template content with code-accurate registry and source details; aligned enabled features to current `features.yml`.
