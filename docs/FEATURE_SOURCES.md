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
| `slope_mean` | true | `raster_zonal` | `terrain` | `config/sources/terrain.yml` | zonal mean of slope raster (degrees) | none | keep NA |
| `tri_mean` | true | `raster_zonal` | `terrain` | `config/sources/terrain.yml` | zonal mean of TRI raster | none | keep NA |
| `dist_coast_km` | true | `raster_zonal` | `water_distance` | `config/sources/water_distance.yml` | zonal mean of cost-distance to coast (km) | none | keep NA |
| `dist_river_km` | true | `raster_zonal` | `water_distance` | `config/sources/water_distance.yml` | zonal mean of cost-distance to major rivers (km) | none | keep NA |
| `soil_pc1`…`soil_pcN` | true | `soil_pca` | `soilgrids` | `config/sources/soilgrids.yml` | zonal mean of 61 SoilGrids layers → PCA | center + scale + PCA rotation | impute NA with column median (PCA fit) or center (apply) |

Notes:
- Current production runs include `elevation_mean`, `slope_mean`, `tri_mean`, `dist_coast_km`, `dist_river_km` from the registry plus soil PCA components.
- Slope and TRI are derived from the same WorldClim DEM used for `elevation_mean` via `terra::terrain()`.
- Distance-to-water features use least-cost path analysis (`terra::costDist()`) with slope as friction surface, not Euclidean distance.
- SoilGrids extraction downloads 61 rasters (11 properties × 6 depths, with `ocs` at 0-30 cm only), computes zonal means per admin polygon, then runs PCA. The number of retained components is governed by `config/sources/soilgrids.yml::pca.variance_threshold` (default 0.95).
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

## Source record: `soil_pc*` (SoilGrids 250m v2.0)

- Description: PCA components derived from 61 physical and chemical soil property layers, extracted as zonal means per admin geometry.
- Model role: predictor (one column per retained PC).
- Source dataset: SoilGrids 250m v2.0.
- Provider: ISRIC — World Soil Information.
- Reference: Poggio, L., de Sousa, L. M., Batjes, N. H., Heuvelink, G. B. M., Kempen, B., Ribeiro, E., and Rossiter, D.: SoilGrids 2.0: producing soil information for the globe with quantified spatial uncertainty, SOIL, 7, 217–240, 2021. DOI: 10.5194/soil-7-217-2021
- Acquisition mode: remote read via GDAL `/vsicurl/` from ISRIC WebDAV VRT files; cropped to study bbox, reprojected, and cached locally as GeoTIFF.
- Source config file: `config/sources/soilgrids.yml`.
- Source URL: `https://files.isric.org/soilgrids/latest/data/`
- Source format: VRT (Cloud Optimized GeoTIFF tiles).
- Source version: `2.0`.
- License: `CC BY 4.0`.
- Processed raster directory: `data/intermediate/features/soilgrids/` (one `.tif` per property × depth).
- Properties (11 total):
  - Physical (5): `bdod` (bulk density, cg/cm³), `cfvo` (coarse fragments, cm³/dm³), `clay` (g/kg), `sand` (g/kg), `silt` (g/kg).
  - Chemical (6): `cec` (cation exchange capacity, mmol(c)/kg), `nitrogen` (total N, cg/kg), `phh2o` (pH × 10), `soc` (organic carbon, dg/kg), `ocd` (organic carbon density, hg/m³), `ocs` (organic carbon stocks, t/ha — 0-30 cm only).
- Depths (6 standard + 1 exception):
  - Standard: 0-5 cm, 5-15 cm, 15-30 cm, 30-60 cm, 60-100 cm, 100-200 cm.
  - `ocs` only: 0-30 cm.
  - Total layers: 10 × 6 + 1 = 61.
- Spatial processing:
  - Native CRS: Interrupted Goode Homolosine (ISRIC).
  - Cropped to study bbox: `[-10, 40, 20, 60]` (WGS84), projected to Homolosine for cropping.
  - Reprojected to canonical CRS (default `EPSG:3035`).
  - Extraction method: polygon zonal mean via `terra::extract(..., fun = mean, exact = TRUE)`.
- PCA transform:
  - Fit on the combined panel (all countries).
  - Centered and scaled (`prcomp(..., center = TRUE, scale. = TRUE)`).
  - Number of retained components: cumulative variance ≥ threshold (default 95%, configurable in `soilgrids.yml::pca.variance_threshold`).
  - Output columns: `soil_pc1`, `soil_pc2`, …, `soil_pcN`.
  - Raw `soil_*` columns are dropped after PCA.
  - The fitted PCA rotation is stored as a targets object (`soil_pca_model`) and reused for raster prediction.
- Temporal characteristics: static (applies to all years).
- Missing-data handling: NAs imputed with column median during PCA fit, with PCA center during PCA application.
- Quality checks:
  - All 61 rasters must be present after download step.
  - At least 2 non-constant soil columns required for PCA.
  - Variance explained is logged.

## Source record: `slope_mean` (terrain slope)

- Description: Mean slope (degrees) within each harmonized admin unit geometry.
- Model role: predictor.
- Source dataset: Derived from WorldClim v2.1 elevation raster via `terra::terrain(v = "slope", unit = "degrees")`.
- Provider: Computed in-pipeline from elevation DEM.
- Source config file: `config/sources/terrain.yml`.
- License: `CC BY 4.0` (inherited from WorldClim DEM).
- Processed raster path: `data/intermediate/features/slope_30s.tif`.
- Spatial processing:
  - Computed in canonical CRS (default `EPSG:3035`).
  - Extraction method: polygon zonal mean (`terra::extract(..., fun = mean, exact = TRUE)`).
- Temporal characteristics: static (applies to all years).
- Missing-data handling: `NA` values are retained.

## Source record: `tri_mean` (Terrain Ruggedness Index)

- Description: Mean Terrain Ruggedness Index (Riley et al. 1999) within each harmonized admin unit geometry.
- Model role: predictor.
- Source dataset: Derived from WorldClim v2.1 elevation raster via `terra::terrain(v = "TRI")`.
- Provider: Computed in-pipeline from elevation DEM.
- Source config file: `config/sources/terrain.yml`.
- License: `CC BY 4.0` (inherited from WorldClim DEM).
- Processed raster path: `data/intermediate/features/tri_30s.tif`.
- Spatial processing:
  - Computed in canonical CRS (default `EPSG:3035`).
  - Extraction method: polygon zonal mean (`terra::extract(..., fun = mean, exact = TRUE)`).
- Temporal characteristics: static (applies to all years).
- Missing-data handling: `NA` values are retained.

## Source record: `dist_coast_km` (distance to coast)

- Description: Mean terrain-weighted least-cost distance (km) to the nearest coastline, within each admin unit geometry.
- Model role: predictor.
- Source dataset: Natural Earth 10m coastline (public domain) for coast geometry; WorldClim DEM slope as friction surface.
- Provider: Natural Earth / derived in pipeline.
- Acquisition mode: download (zip archive).
- Source config file: `config/sources/water_distance.yml`.
- Source URL: `https://naciscdn.org/naturalearth/10m/physical/ne_10m_coastline.zip`
- License: public domain (Natural Earth).
- Raw storage path: `data/intermediate/downloads/coastline/`.
- Processed raster path: `data/intermediate/features/dist_coast_km.tif`.
- Distance method: Least-cost path analysis using `terra::costDist()`.
  - Friction surface: `cell_res_m × (1 + slope_penalty × slope_deg / 90)`.
  - Default `slope_penalty`: 5 (configurable in `water_distance.yml`).
  - Flat terrain ≈ Euclidean distance; steep terrain inflates effective distance.
- Spatial processing:
  - Coastline cropped to study bbox `[-10, 40, 20, 60]` (WGS84), reprojected to canonical CRS.
  - Extraction method: polygon zonal mean (`terra::extract(..., fun = mean, exact = TRUE)`).
- Temporal characteristics: static (applies to all years).
- Missing-data handling: `NA` values are retained.

## Source record: `dist_river_km` (distance to major rivers)

- Description: Mean terrain-weighted least-cost distance (km) to the nearest major river, within each admin unit geometry.
- Model role: predictor.
- Source dataset: HydroRIVERS v1.0 (HydroSHEDS / WWF) for river geometry; WorldClim DEM slope as friction surface.
- Provider: WWF / ISRIC / derived in pipeline.
- Reference: Lehner, B., Grill G. (2013): Global river hydrography and network routing. Hydrological Processes, 27(15): 2171–2186. DOI: 10.1002/hyp.9740
- Acquisition mode: download (zip archive) of European extract.
- Source config file: `config/sources/water_distance.yml`.
- Source URL: `https://data.hydrosheds.org/file/HydroRIVERS/HydroRIVERS_v10_eu_shp.zip`
- License: HydroSHEDS license (free for non-commercial use).
- Raw storage path: `data/intermediate/downloads/hydrorivers/`.
- Processed raster path: `data/intermediate/features/dist_river_km.tif`.
- River filtering: Strahler stream order ≥ 4 (configurable via `processing.strahler_min_order`).
- Distance method: Least-cost path analysis using `terra::costDist()`.
  - Friction surface: `cell_res_m × (1 + slope_penalty × slope_deg / 90)`.
  - Default `slope_penalty`: 5 (configurable in `water_distance.yml`).
  - Flat terrain ≈ Euclidean distance; steep terrain inflates effective distance.
- Spatial processing:
  - Rivers filtered by Strahler order and bbox, reprojected to canonical CRS.
  - Extraction method: polygon zonal mean (`terra::extract(..., fun = mean, exact = TRUE)`).
- Temporal characteristics: static (applies to all years).
- Missing-data handling: `NA` values are retained.

## Planned but currently disabled features

Present as commented examples in `config/sources/features.yml` (not active in pipeline unless enabled):
- `climate_temp_mean` via `config/sources/climate.yml`
- `nightlights_mean` via `config/sources/nightlights.yml`
- `travel_time_city_mean` via `config/sources/accessibility.yml`

These do not currently contribute columns to model training/output schemas.

## Change log
- `2026-02-20`: Added terrain features (`slope_mean`, `tri_mean`) derived from DEM and water distance features (`dist_coast_km`, `dist_river_km`) using least-cost path analysis with slope friction; added source records and updated active registry table.
- `2026-02-20`: Added SoilGrids 250m v2.0 soil PCA features (61 raw layers → PCA components); added source record and updated active registry table.
- `2026-02-20`: Replaced template content with code-accurate registry and source details; aligned enabled features to current `features.yml`.
