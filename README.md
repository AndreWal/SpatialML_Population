# Spatial Predictions

Reproducible multi-country geospatial ETL + spatial ML scaffold in R (`targets` + `renv`).

## Quickstart

```bash
R -q -e "renv::restore(prompt=FALSE)"
R -q -e "targets::tar_make()"
```

Default runs in mock mode (`MOCK_MODE=true`) so missing `data/raw/*` files do not block CI.

To force real raw inputs:

```bash
MOCK_MODE=false R -q -e "targets::tar_make()"
```

## Pipeline stages (current)

1. Load enabled countries from `config/global/project.yml`
2. Branch by country
3. Read/assemble tabular + geometry inputs (or mock fallback)
4. Harmonize admin keys via crosswalk
5. Transform to canonical CRS (`config/global/crs.yml`)
6. Run QA checks (unique key, geometry present, CRS, join coverage)
7. Write outputs:
   - `data/final/<ISO3>/<ISO3>_panel.gpkg`
   - `data/final/<ISO3>/<ISO3>_panel.parquet`

## Tests

```bash
R -q -e "testthat::test_dir('tests/testthat')"
```
