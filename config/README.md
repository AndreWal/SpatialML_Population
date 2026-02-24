# Config conventions

## Precedence (lowest -> highest)
1. `config/global/*.yml`
2. `config/countries/<ISO3>.yml`
3. `config/local.override.yml` (not committed)

## Current global config files (core)
- `config/global/project.yml`
- `config/global/paths.yml`
- `config/global/crs.yml`
- `config/global/qa.yml`
- `config/global/ml.yml`
- `config/global/allocation.yml`

## Current defaults (from committed config)
- Canonical CRS: `EPSG:3035`
- Geographic CRS: `EPSG:4326`
- Enabled countries: `DEU`, `NLD` (others may be configured but disabled)
- Output formats: vectors `.gpkg`, rasters `.tif`, tables `.parquet`
- ML holdout country: `DEU`
- ML split method: `spatial_cv_cast`
- ML raster prediction resolution: `1000 m`
- Allocation QA mass-relative-error tolerance: `1e-8` (`config/global/qa.yml::qa.allocation.mass_rel_error_tolerance`)
- Allocation area denominator: `overlap_area_m2` (`config/global/allocation.yml::allocation.area_denominator`)
- Optional calibration outputs can be enabled via `config/global/allocation.yml::allocation.calibration.enabled` with `allocation.calibration.totals_by_year` (legacy fallback: `ml.calibration.*`)

## Naming
- Country files use ISO3 uppercase: `DEU.yml`
- Keys are snake_case
- Paths are repo-relative
- CRS values are EPSG strings, e.g. `EPSG:3035`

## Add a new country
1. Create `config/countries/<ISO3>.yml`
2. Add `config/crosswalks/<ISO3>.csv` (if harmonization is needed)
3. Verify source columns and join keys
4. Confirm year coverage is compatible with global QA (`qa.temporal.allowed_years`) or document overrides
5. Run pipeline checks

## Add a new feature source
1. Create `config/sources/<feature>.yml`
2. Reference it in `config/sources/features.yml`
3. Add license/version/checksum metadata
4. Document the feature in `docs/FEATURE_SOURCES.md`
5. Confirm support semantics:
   - polygon zonal feature (for panel models), and/or
   - cell-level covariate (for disaggregation weighting)

## Recommended future config split for disaggregation (planned)
These are not required yet, but are recommended as the pipeline moves to constrained allocation:
- `config/global/disaggregation.yml`
- `config/global/allocation.yml`

Suggested keys (examples):
- `mode: constrained | unconstrained | hybrid`
- `area_denominator: overlap_area_m2 | habitable_overlap_area_m2`
- `mass_preservation_tolerance`
- `fallback_zero_weight: uniform_area`
- `calibration.enabled`
- `calibration.level: country | region`

## Documentation sync rule
Any change to:
- output filenames,
- schema columns,
- feature semantics,
- or constrained/unconstrained prediction behavior

must update:
1. `docs/PROJECT_SPEC.md`
2. `docs/DATA_SCHEMA.md`
3. `docs/FEATURE_SOURCES.md`
