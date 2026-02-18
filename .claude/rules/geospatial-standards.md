# geospatial standards
- Transform all vectors to canonical CRS before joins.
- Validate geometry (`st_is_valid`) and fix if required.
- Ensure one geometry per harmonized admin unit-year record after join.
- Write final vectors as `.gpkg`, rasters as `.tif`.
