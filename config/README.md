# Config conventions

## Precedence (lowest -> highest)
1. `config/global/*.yml`
2. `config/countries/<ISO3>.yml`
3. `config/local.override.yml` (not committed)

## Naming
- Country files use ISO3 uppercase: `DEU.yml`
- Keys are snake_case
- Paths are repo-relative
- CRS values are EPSG strings, e.g. `EPSG:3035`

## Add a new country
1. Create `config/countries/<ISO3>.yml`
2. Add `config/crosswalks/<ISO3>.csv`
3. Verify source columns and join keys
4. Run pipeline checks

## Add a new feature source
1. Create `config/sources/<feature>.yml`
2. Reference it in `config/sources/features.yml`
3. Add license/version/checksum metadata
