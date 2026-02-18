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

## Where detailed rules live
See `.claude/rules/*.md` for:
- R code style
- targets pipeline rules
- geospatial standards
