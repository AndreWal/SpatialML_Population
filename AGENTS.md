# AGENTS.md

## Purpose
This repo builds a reproducible multi-country geospatial dataset and spatial ML pipeline in R.

## Canonical instructions
Always read these first and treat them as source of truth:
1. `docs/PROJECT_SPEC.md`
2. `docs/DATA_SCHEMA.md`
3. `docs/FEATURE_SOURCES.md`

If this file conflicts with those docs, update this file to align with them.

## Working mode
1. Explore current code and config before editing.
2. Propose a short plan.
3. Implement minimal, testable changes.
4. Run validation commands.
5. Report exactly what changed and what still needs work.

## Required commands before declaring success
- `R -q -e "renv::restore(prompt=FALSE)"`
- `R -q -e "targets::tar_make()"`

If tests exist:
- `R -q -e "testthat::test_dir('tests/testthat')"`

## R coding conventions
- Prefer `sf`, `terra`, `targets`, `duckdb`, `arrow`.
- Keep functions pure and file IO explicit.
- Never hardcode machine-specific paths.
- Never bypass CRS harmonization.
- Never write final vector outputs as shapefiles; use `.gpkg`.

## Data safety
- Do not alter files in `data/raw/` in-place.
- Write derived data only to designated output folders.
- Preserve source provenance metadata.

## PR-quality checklist
- [ ] Pipeline still runs with `targets`
- [ ] Reproducibility assumptions unchanged or documented
- [ ] New columns/outputs reflected in `docs/DATA_SCHEMA.md`
- [ ] Feature sources documented in `docs/FEATURE_SOURCES.md`
- [ ] No secret keys or local paths committed
