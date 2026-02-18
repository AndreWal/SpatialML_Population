# Data Schema

## Purpose
This file defines the canonical data contracts for all pipeline stages:
- required columns
- types
- primary keys
- join logic
- CRS/geometry constraints
- validation rules

If any schema changes, update:
1. this file
2. `config/schemas/*.yml`
3. relevant country config (`config/countries/*.yml`) if country-specific

---

## 1) Global conventions

### 1.1 Naming
- Column names: `snake_case`
- Country code: ISO3 uppercase (`DEU`, `FRA`, ...)
- Year column: integer Gregorian year (`1990`, `2000`, ...)

### 1.2 Canonical keys
- Spatial-temporal row key (panel):  
  `country_code + admin_unit_harmonized + year`
- Spatial key (geometry table):  
  `country_code + admin_unit_harmonized`

### 1.3 Canonical CRS
- All final vector outputs must be in CRS from `config/global/crs.yml::crs.canonical`.
- No mixed-CRS objects in final outputs.

---

## 2) Entities and contracts

## 2.1 `tabular_subset` (country-level, pre-harmonization)
Minimum required columns after initial subset from raw demographic files.

| column            | type      | required | constraints |
|------------------|-----------|----------|-------------|
| country_code      | character | yes      | ISO3 uppercase |
| admin_id_raw      | integer | yes      | non-empty |
| admin_name_raw    | character | yes      | non-empty |
| year              | integer   | yes      | allowed years only |
| population        | numeric   | yes      | > 0 |

Notes:
- Keep any extra raw columns out of this canonical object.
- `admin_id_raw` is the preferred key; `admin_name_raw` is fallback only.

---

## 2.2 `admin_crosswalk`
Harmonization mapping from raw admin units to harmonized units.

| column            | type      | required | constraints |
|------------------|-----------|----------|-------------|
| country_code      | character | yes      | ISO3 |
| from_admin_id     | integer | yes      | maps raw admin id |
| from_admin_name   | character | no       | raw admin label |
| to_admin_id       | integer | yes      | harmonized unit id |
| to_admin_name     | character | yes      | harmonized unit label |
| valid_from_year   | integer   | no       | optional temporal mapping |
| valid_to_year     | integer   | no       | optional temporal mapping |
| weight            | numeric   | no       | default 1.0; must be > 0 |
| notes             | character | no       | free text |

Rules:
- Many-to-one merges: multiple `from_*` rows to one `to_*`.
- One-to-many splits allowed only with explicit `weight`.
- For split mappings, weights for each (`from_admin_id`, `year`) group must sum to 1 within tolerance.

---

## 2.3 `tabular_harmonized`
Demographic panel after crosswalk and aggregation.

| column                  | type      | required | constraints |
|------------------------|-----------|----------|-------------|
| country_code            | character | yes      | ISO3 |
| admin_unit_harmonized   | character | yes      | derived from `to_admin_id` |
| admin_name_harmonized   | character | yes      | derived from `to_admin_name` |
| year                    | integer   | yes      | allowed years |
| population              | numeric   | yes      | > 0 |

Primary key:
- `country_code + admin_unit_harmonized + year` must be unique.

Aggregation rule:
- For merges, population is summed unless overridden in config.

---

## 2.4 `polygons_harmonized` (sf object)
Administrative polygons aligned to harmonized units and canonical CRS.

| column                  | type      | required | constraints |
|------------------------|-----------|----------|-------------|
| country_code            | character | yes      | ISO3 |
| admin_unit_harmonized   | character | yes      | unique within country |
| admin_name_harmonized   | character | yes      | non-empty |
| geometry                | geometry  | yes      | valid geometry |

Rules:
- CRS must equal canonical CRS.
- Geometries should be valid (`st_is_valid == TRUE`) after repair step.
- Multipolygons allowed.

Primary key:
- `country_code + admin_unit_harmonized` unique.

---

## 2.5 `admin_panel_sf` (final vector panel, sf)
Joined table + geometry used for feature engineering and ML prep.

| column                  | type      | required | constraints |
|------------------------|-----------|----------|-------------|
| country_code            | character | yes      | ISO3 |
| admin_unit_harmonized   | character | yes      | non-empty |
| year                    | integer   | yes      | allowed years |
| population              | numeric   | yes      | > 0 |
| <feature columns...>    | numeric   | depends  | per feature definition |
| geometry                | geometry  | yes      | valid + canonical CRS |

Primary key:
- `country_code + admin_unit_harmonized + year` unique.

Join coverage:
- Must satisfy threshold in `config/global/qa.yml` or country override.

---

## 2.6 `model_table` (non-spatial modeling table)
Derived tabular data for model fitting; geometry may be dropped but IDs retained.

| column                  | type      | required | constraints |
|------------------------|-----------|----------|-------------|
| country_code            | character | yes      | ISO3 |
| admin_unit_harmonized   | character | yes      | non-empty |
| year                    | integer   | yes      | allowed years |
| target_variable         | numeric   | yes      | defined in `config/global/ml.yml` |
| <feature columns...>    | numeric   | yes/no   | according to feature registry |

Primary key:
- `country_code + admin_unit_harmonized + year` unique.

---

## 2.7 `raster_prediction` (GeoTIFF outputs)
Predicted raster map metadata requirements.

Required metadata fields:
- model_id
- target_variable
- prediction_year (if applicable)
- resolution_m
- crs
- source_pipeline_run_id (or timestamp/version)

Rules:
- Raster CRS must be documented and consistent with prediction grid settings.
- Nodata handling must be explicit.

---

## 3) Allowed years and temporal policy
- Allowed years are defined in config (`qa.temporal.allowed_years` and/or country settings).
- Years outside allowed set fail validation unless explicitly whitelisted.

---

## 4) Validation checklist (must pass)

1. Required columns present for each entity.
2. Column types conform.
3. Primary keys unique.
4. No missing geometry in final spatial outputs.
5. CRS equals canonical CRS for final vectors.
6. Join coverage >= configured threshold.
7. Population values within configured bounds.
8. Feature missingness within configured thresholds.

---

## 5) Versioning
Schema version: `v1.0.0`

When incrementing:
- Patch: non-breaking clarifications
- Minor: additive columns/optional entities
- Major: breaking key/type/contract changes
