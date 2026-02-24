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
6. **Support must be explicit**:
   - polygon-level zonal feature (for polygon panel / benchmark ML), and/or
   - cell-level covariate (for disaggregation score/intensity modeling)
7. Polygon-only derived features must not be silently reused as cell-level predictors.

## Feature support semantics (important)
This project uses two modeling supports:
- **Polygon support**: one row per admin polygon-year (`country_panel`)
- **Cell support**: one row per grid cell (or polygon-cell intersection row) for disaggregation

Implications:
- Zonal-mean features in the panel (`*_mean`) are polygon-support summaries.
- Cell-level score/intensity models should use the underlying raster covariates (or cell summaries), not polygon zonal means.
- `log_area` is valid as a polygon-support feature but should **not** be used as a cell-level predictor.
- `lon` / `lat` can be used at both supports (polygon centroids vs cell centroids), but their meaning must be documented.

## Time-invariant covariate limitation (current project state)
The currently enabled covariates are predominantly time-invariant (topography, soil, distance-to-water, geometric location). This is acceptable for historical disaggregation but has consequences:
- Spatial patterning learned by the model will tend to be relatively stable over time.
- Temporal variation will come primarily from:
  - polygon totals (`population`),
  - year dummy/intercept effects,
  - and any future time-varying covariates if added.
- Resulting decadal surfaces remain useful as historically informed allocation products, but they should not be overinterpreted as capturing dynamic infrastructure/building changes unless such covariates are introduced.

## Active feature registry (current code + config)

This table summarizes the effective predictor inventory used by the pipeline. It includes:
- direct registry entries from `config/sources/features.yml`,
- PCA-derived outputs (`soil_pc*`), and
- derived geometry features added in code (`lon`, `lat`, `log_area`).

| feature_id | enabled | type | source_id | source_config | extraction | transform | missing_policy | support |
|---|---|---|---|---|---|---|---|---|
| `elevation_mean` | true | `raster_zonal` | `elevation` | `config/sources/elevation.yml` | zonal mean (`processing.zonal_stat`) | none | keep NA | polygon |
| `slope_mean` | true | `raster_zonal` | `terrain` | `config/sources/terrain.yml` | zonal mean of slope raster (degrees) | none | keep NA | polygon |
| `tri_mean` | true | `raster_zonal` | `terrain` | `config/sources/terrain.yml` | zonal mean of TRI raster | none | keep NA | polygon |
| `dist_coast_km` | true | `raster_zonal` | `water_distance` | `config/sources/water_distance.yml` | zonal mean of cost-distance to coast (km) | none | keep NA | polygon |
| `dist_river_km` | true | `raster_zonal` | `water_distance` | `config/sources/water_distance.yml` | zonal mean of cost-distance to major rivers (km) | none | keep NA | polygon |
| `soil_pc1`…`soil_pcN` | true | `soil_pca` | `soilgrids` | `config/sources/soilgrids.yml` | zonal mean of 61 SoilGrids layers → PCA | center + scale + PCA rotation | impute NA with column median (PCA fit) or center (apply) | polygon |
| `lon`, `lat` | n/a (derived in code) | `derived_geometry` | n/a | n/a | centroid coordinates | none | n/a | polygon + cell (if derived from cell centroids) |
| `log_area` | n/a (derived in code) | `derived_geometry` | n/a | n/a | `log1p(area_m2)` | none | n/a | polygon only |

Notes:
- Current production runs include `elevation_mean`, `slope_mean`, `tri_mean`, `dist_coast_km`, `dist_river_km` from the registry plus soil PCA components.
- Slope and TRI are derived from the same WorldClim DEM used for `elevation_mean` via `terra::terrain()`.
- Distance-to-water features use least-cost path analysis (`terra::costDist()`) with slope as friction surface, not Euclidean distance.
- SoilGrids extraction downloads 61 rasters (11 properties × 6 depths, with `ocs` at 0-30 cm only), computes zonal means per admin polygon, then runs PCA. The number of retained components is governed by `config/sources/soilgrids.yml::pca.variance_threshold` (default `0.95`).
- Derived geometric predictors `log_area`, `lon`, and `lat` are added in code (`add_geometric_features()`), not in `features.yml`.
- Year is encoded during model prep as dummy features `year_<value>`; these are generated from panel data, not from source configs.

## Guidance for cell-level disaggregation feature stacks (planned/target)
For constrained or unconstrained cell-level score/intensity modeling:
- Use cell-level covariates derived from the same underlying sources where possible:
  - elevation
  - slope
  - TRI
  - distance-to-coast
  - distance-to-river
  - soil PCA rasters (or cell-level transformed soil features)
  - lon / lat
  - year dummies
- Do **not** include polygon-support summaries as cell predictors:
  - `elevation_mean`, `slope_mean`, `tri_mean`, etc. are polygon means, not cell covariates
  - `log_area` is polygon support and should not be reused at cell support

Current implementation note:
- The ML-weighted constrained-allocation branch uses a separate score-proxy model that excludes polygon-only derived predictors (especially `log_area`) from the raster score feature stack.

Compatibility note:
- Raster score prediction for the ML-weighted constrained-allocation branch excludes polygon-only derived predictors (especially `log_area`) from the cell-level feature stack.

---

## Source record: `elevation_mean`

- Description: Mean elevation within each harmonized admin unit geometry.
- Model role: predictor (polygon-support feature).
- Source dataset: WorldClim v2.1 elevation raster.
- Provider: UC Davis (as declared in source config).
- Acquisition mode: download (zip archive), then unzip and process.
- Source config file: `config/sources/elevation.yml`.
- Source URL: `https://geodata.ucdavis.edu/climate/worldclim/2_1/base/wc2.1_30s_elev.zip`
- Source format: `zip` (contains `.tif`).
- Source version: `2.1`.
- License: `CC BY 4.0`.
- Raw storage path: `data/raw/global/elevation/copernicus_dem_glo30/` (per current config; name reflects local directory choice, not necessarily source branding).
- Processed raster path: `data/intermediate/features/wc2.1_30s_elev.tif` (from `elevation.yml`).
- Spatial processing:
  - Input CRS expected in source config: `EPSG:4326`
  - Cropped to configured study bbox: `[-10, 40, 20, 60]` (WGS84)
  - Reprojected to canonical CRS (`config/global/crs.yml`, default `EPSG:3035`)
  - Extraction method: polygon zonal mean (`terra::extract(..., fun = mean, exact = TRUE)`)
- Cell-level usage (planned):
  - Same underlying raster may be used directly as a cell covariate in score/intensity models.
- Temporal characteristics: static (applies to all years).
- Missing-data handling: `NA` values are retained.
- Quality checks:
  - Source config must exist and be readable.
  - Raster must be discoverable from processed path or raw path.
  - Raster is reprojected if CRS differs from canonical CRS.

## Source record: `soil_pc*` (SoilGrids 250m v2.0)

- Description: PCA components derived from 61 physical and chemical soil property layers, extracted as zonal means per admin geometry.
- Model role: predictor (one column per retained PC; polygon-support in current implementation).
- Source dataset: SoilGrids 250m v2.0.
- Provider: ISRIC — World Soil Information.
- Reference: Poggio, L., de Sousa, L. M., Batjes, N. H., Heuvelink, G. B. M., Kempen, B., Ribeiro, E., and Rossiter, D. (2021), SOIL 7, 217–240.
- Acquisition mode: remote read via GDAL `/vsicurl/` from ISRIC WebDAV VRT files; cropped to study bbox, reprojected, and cached locally as GeoTIFF.
- Source config file: `config/sources/soilgrids.yml`.
- Source URL: `https://files.isric.org/soilgrids/latest/data/`
- Source format: VRT (Cloud Optimized GeoTIFF tiles).
- Source version: `2.0`.
- License: `CC BY 4.0`.
- Processed raster directory: `data/intermediate/features/soilgrids/` (one `.tif` per property × depth).
- Properties (11 total):
  - Physical (5): `bdod`, `cfvo`, `clay`, `sand`, `silt`
  - Chemical (6): `cec`, `nitrogen`, `phh2o`, `soc`, `ocd`, `ocs`
- Depths:
  - Standard: 0-5 cm, 5-15 cm, 15-30 cm, 30-60 cm, 60-100 cm, 100-200 cm
  - `ocs` only: 0-30 cm
  - Total layers: 61
- Spatial processing:
  - Native CRS: Interrupted Goode Homolosine (ISRIC)
  - Cropped to study bbox `[-10, 40, 20, 60]` (WGS84), projected as needed for cropping
  - Reprojected to canonical CRS (default `EPSG:3035`)
  - Extraction method: polygon zonal mean via `terra::extract(..., fun = mean, exact = TRUE)`
- PCA transform (current implementation):
  - Fit on the combined panel (all countries)
  - Centered and scaled (`prcomp(..., center = TRUE, scale. = TRUE)`)
  - Retain components until cumulative variance ≥ threshold (`soilgrids.yml::pca.variance_threshold`, default `0.95`)
  - Output columns: `soil_pc1`, `soil_pc2`, …, `soil_pcN`
  - Raw `soil_*` columns are dropped after PCA
  - The fitted PCA rotation is stored and reused for prediction workflows
- Cell-level usage (planned):
  - Prefer a consistent PCA transform applied to cell-level soil covariates before score/intensity prediction.
- Temporal characteristics: static (applies to all years).
- Missing-data handling: NAs imputed with column median during PCA fit, with PCA center during PCA application.
- Quality checks:
  - All required rasters must be present after download/caching
  - At least 2 non-constant soil columns required for PCA
  - Variance explained is logged

## Source record: `slope_mean` (terrain slope)

- Description: Mean slope (degrees) within each harmonized admin unit geometry.
- Model role: predictor (polygon-support feature).
- Source dataset: Derived from WorldClim v2.1 elevation raster via `terra::terrain(v = "slope", unit = "degrees")`.
- Provider: Computed in-pipeline from elevation DEM.
- Source config file: `config/sources/terrain.yml`.
- License: `CC BY 4.0` (inherited from WorldClim DEM).
- Processed raster path: `data/intermediate/features/slope_30s.tif`.
- Spatial processing:
  - Computed in canonical CRS (default `EPSG:3035`).
  - Extraction method: polygon zonal mean (`terra::extract(..., fun = mean, exact = TRUE)`).
- Cell-level usage (planned):
  - Use the slope raster directly as a cell covariate where score/intensity models operate on cells.
- Temporal characteristics: static (applies to all years).
- Missing-data handling: `NA` values are retained.

## Source record: `tri_mean` (Terrain Ruggedness Index)

- Description: Mean Terrain Ruggedness Index (Riley et al. 1999) within each harmonized admin unit geometry.
- Model role: predictor (polygon-support feature).
- Source dataset: Derived from WorldClim v2.1 elevation raster via `terra::terrain(v = "TRI")`.
- Provider: Computed in-pipeline from elevation DEM.
- Source config file: `config/sources/terrain.yml`.
- License: `CC BY 4.0` (inherited from WorldClim DEM).
- Processed raster path: `data/intermediate/features/tri_30s.tif`.
- Spatial processing:
  - Computed in canonical CRS (default `EPSG:3035`).
  - Extraction method: polygon zonal mean (`terra::extract(..., fun = mean, exact = TRUE)`).
- Cell-level usage (planned):
  - Use the TRI raster directly as a cell covariate where score/intensity models operate on cells.
- Temporal characteristics: static (applies to all years).
- Missing-data handling: `NA` values are retained.

## Source record: `dist_coast_km` (distance to coast)

- Description: Mean terrain-weighted least-cost distance (km) to the nearest coastline, within each admin unit geometry.
- Model role: predictor (polygon-support feature).
- Source dataset: Natural Earth 10m coastline (public domain) for coast geometry; WorldClim DEM slope as friction surface.
- Provider: Natural Earth / derived in pipeline.
- Acquisition mode: download (zip archive).
- Source config file: `config/sources/water_distance.yml`.
- Source URL: `https://naciscdn.org/naturalearth/10m/physical/ne_10m_coastline.zip`
- License: public domain (Natural Earth).
- Raw storage path: `data/intermediate/downloads/coastline/`.
- Processed raster path: `data/intermediate/features/dist_coast_km.tif`.
- Distance method: Least-cost path analysis using `terra::costDist()`.
  - Friction surface: `cell_res_m × (1 + slope_penalty × slope_deg / 90)`
  - Default `slope_penalty`: `5` (configurable in `water_distance.yml`)
  - Flat terrain approximates Euclidean distance; steep terrain inflates effective distance
- Spatial processing:
  - Coastline cropped to study bbox `[-10, 40, 20, 60]` (WGS84), reprojected to canonical CRS
  - Extraction method: polygon zonal mean (`terra::extract(..., fun = mean, exact = TRUE)`)
- Cell-level usage (planned):
  - Use the cost-distance raster directly as a cell covariate
- Temporal characteristics: static (applies to all years).
- Missing-data handling: `NA` values are retained.

## Source record: `dist_river_km` (distance to major rivers)

- Description: Mean terrain-weighted least-cost distance (km) to the nearest major river, within each admin unit geometry.
- Model role: predictor (polygon-support feature).
- Source dataset: HydroRIVERS v1.0 (HydroSHEDS / WWF) for river geometry; WorldClim DEM slope as friction surface.
- Provider: WWF / derived in pipeline.
- Reference: Lehner, B., Grill, G. (2013). Hydrological Processes, 27(15): 2171–2186.
- Acquisition mode: download (zip archive) of European extract.
- Source config file: `config/sources/water_distance.yml`.
- Source URL: `https://data.hydrosheds.org/file/HydroRIVERS/HydroRIVERS_v10_eu_shp.zip`
- License: HydroSHEDS license (free for non-commercial use).
- Raw storage path: `data/intermediate/downloads/hydrorivers/`.
- Processed raster path: `data/intermediate/features/dist_river_km.tif`.
- River filtering: Strahler stream order ≥ 4 (configurable via `processing.strahler_min_order`)
- Distance method: Least-cost path analysis using `terra::costDist()`.
  - Friction surface: `cell_res_m × (1 + slope_penalty × slope_deg / 90)`
  - Default `slope_penalty`: `5` (configurable in `water_distance.yml`)
  - Flat terrain approximates Euclidean distance; steep terrain inflates effective distance
- Spatial processing:
  - Rivers filtered by Strahler order and bbox, reprojected to canonical CRS
  - Extraction method: polygon zonal mean (`terra::extract(..., fun = mean, exact = TRUE)`)
- Cell-level usage (planned):
  - Use the cost-distance raster directly as a cell covariate
- Temporal characteristics: static (applies to all years).
- Missing-data handling: `NA` values are retained.

## Change log
- `2026-02-23`: Clarified support semantics (polygon vs cell), documented polygon-only use of `log_area`, and added guidance for cell-level disaggregation feature stacks.
- `2026-02-23`: Added note on time-invariant covariate limitations for historical disaggregation interpretation.
- `2026-02-20`: Added terrain features (`slope_mean`, `tri_mean`) derived from DEM and water distance features (`dist_coast_km`, `dist_river_km`) using least-cost path analysis with slope friction; added source records and updated active registry table.
- `2026-02-20`: Added SoilGrids 250m v2.0 soil PCA features (61 raw layers → PCA components); added source record and updated active registry table.
- `2026-02-20`: Replaced template content with code-accurate registry and source details; aligned enabled features to current `features.yml`.
