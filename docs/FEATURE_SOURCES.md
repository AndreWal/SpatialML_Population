# Feature Sources

## Purpose
This file is the canonical registry of all engineered features used in the pipeline.

It documents:
- what each feature means
- where data comes from
- how it is acquired (local vs download)
- licensing and provenance
- spatial/temporal processing
- missing-data handling

If any feature is added/removed/changed, update:
1. this file
2. `config/sources/features.yml`
3. the feature-specific config in `config/sources/*.yml`

---

## 1) Global feature engineering rules

1. Every feature must have a unique `feature_id`.
2. Every feature must map to exactly one source config file.
3. External downloads must include version + checksum + license.
4. Spatial extraction method must be explicit (e.g., zonal mean).
5. Temporal alignment rule must be explicit (e.g., nearest year).
6. Missing-value policy must be explicit.
7. Any transformation/scaling must be documented.

---

## 2) Feature registry (authoritative index)

| feature_id             | source_id      | source_config                         | spatial_input | temporal_ref | extraction      | transform | missing_policy | enabled |
|------------------------|----------------|---------------------------------------|---------------|--------------|-----------------|-----------|----------------|---------|
| elevation_mean         | elevation      | config/sources/elevation.yml          | raster        | static       | zonal_mean      | none      | keep_na        | true    |
| slope_mean             | elevation      | config/sources/elevation.yml          | raster        | static       | zonal_mean      | none      | keep_na        | true    |
| climate_temp_mean      | climate        | config/sources/climate.yml            | raster        | annual       | zonal_mean      | none      | impute_country | true    |
| nightlights_mean       | nightlights    | config/sources/nightlights.yml        | raster        | annual       | zonal_mean      | log1p     | keep_na        | true    |
| travel_time_city_mean  | accessibility  | config/sources/accessibility.yml      | raster        | static       | zonal_mean      | none      | keep_na        | true    |

Notes:
- This table must mirror `config/sources/features.yml`.
- `enabled=false` features are ignored in production runs.

---

## 3) Per-feature source records

Use this template for each feature/source block.

## Feature: `<feature_id>`

- **Description:**  
- **Unit:**  
- **Model role:** predictor / target-adjacent / diagnostic
- **Source dataset name:**  
- **Provider:**  
- **Acquisition mode:** `download` or `local`
- **Source config file:** `config/sources/<name>.yml`
- **Version/date:**  
- **License:**  
- **Citation requirement:** yes/no (if yes, include citation text below)
- **Checksum (if downloaded):** SHA256
- **Raw file path:**  
- **Processed file path:**  
- **Spatial characteristics:** resolution, CRS, extent
- **Temporal characteristics:** reference year/period, update cadence
- **Extraction rule:** zonal statistic / distance / overlay logic
- **Transformations:** e.g., `log1p`, standardization, clipping
- **Missing-data rule:** drop / keep / impute (with method)
- **Quality checks:** valid range, outlier bounds, coverage minimum
- **Known caveats:**  

---

## 4) Filled examples

## Feature: `elevation_mean`
- **Description:** Mean elevation within harmonized admin polygon.
- **Unit:** meters above sea level.
- **Source dataset name:** Global DEM (document exact product in source config).
- **Provider:** documented in `config/sources/elevation.yml`.
- **Acquisition mode:** download/local (as configured).
- **Source config file:** `config/sources/elevation.yml`
- **Temporal characteristics:** static.
- **Extraction rule:** zonal mean over admin polygons in canonical CRS.
- **Transformations:** none.
- **Missing-data rule:** keep NA (no synthetic fill by default).
- **Quality checks:** values in plausible global range for DEM product.

---

## 5) Temporal alignment policy
When panel year and feature year differ:
1. exact year match if available
2. nearest prior year within tolerance
3. nearest year (absolute distance) if configured
4. otherwise NA

All alignment behavior must be encoded in source config and reproducible.

---

## 6) Licensing and redistribution policy
For each external dataset, include:
- license type
- attribution requirement
- redistribution restrictions
- whether derived outputs can be shared publicly

If redistribution is restricted:
- do not commit raw source files
- document download instructions and checksum only

---

## 7) Change log
- `YYYY-MM-DD`: Added/removed/updated feature(s), reason, author.
