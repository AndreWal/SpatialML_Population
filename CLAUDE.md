# CLAUDE.md

Use this project memory for all tasks in this repository.

## Import canonical project docs
See @docs/PROJECT_SPEC.md
See @docs/DATA_SCHEMA.md
See @docs/FEATURE_SOURCES.md

## Execution workflow
1. Inspect relevant files first.
2. Provide a concise implementation plan.
3. Implement in small commits/steps.
4. Run reproducibility checks:
   - `R -q -e "renv::restore(prompt=FALSE)"`
   - `R -q -e "targets::tar_make()"`
5. Summarize:
   - files changed
   - checks run
   - any remaining risks

## Non-negotiable constraints
- Use canonical CRS from project spec.
- Keep pipeline reproducible and OS-independent.
- Avoid hidden/manual preprocessing.
- Prefer GeoPackage for vector outputs and GeoTIFF for rasters.
- Keep docs in sync when schema or feature sources change.
- Preserve support consistency: polygon-level features may differ from cell-level disaggregation features.
- In constrained disaggregation mode, grid-cell allocations must aggregate back to source polygon totals (within documented tolerance).

## Where detailed rules live
See `.claude/rules/*.md` for:
- R code style
- targets pipeline rules
- geospatial standards

## Notes on population mapping workflow
- Treat ML raster predictions as a weighting/intensity surface by default.
- Only interpret raster values as population counts after a documented allocation/calibration step.
- If no polygon totals exist for an area/year, outputs should be labeled as relative intensity unless calibrated to external totals.