library(tidyverse)
library(openxlsx)
library(sf)
library(XML)
library(gdalUtilities)
library(terra)
library(exactextractr)
library(elevatr)

### CH shapefile

swmap = read_sf(paste0(getwd(), "/project/Data/raw/Switzerland/shapefiles_bfs/ag-b-00.03-889-gg01g1/g1g01-shp_080214/G1G01.shp"))

st_crs(swmap) <- 21781

swmap_wgs84 <- st_transform(swmap, 4326)

# Check if transformations did not produce distortions
par(mfrow = c(1, 2))
plot(st_geometry(swmap), main = "Original (LV03, EPSG:21781)")
plot(st_geometry(swmap_wgs84), main = "Nach WGS84 (EPSG:4326)")
par(mfrow = c(1, 1))

### Soil data

# prep to pull the data

bbsw <- st_bbox(swmap_wgs84)

variables = c("bdod", "cfvo", "clay", "sand", "silt", "cec","nitrogen", "soc", "phh2o")
depth = c("5-15cm","15-30cm","30-60cm","60-100cm","100-200cm")

roi = sprintf("subset=X(%.6f,%.6f)&subset=Y(%.6f,%.6f)", bbsw["xmin"], bbsw["xmax"], bbsw["ymin"], bbsw["ymax"])
subsettingcrs = "SUBSETTINGCRS=http://www.opengis.net/def/crs/EPSG/0/4326"
outputcrs = "OUTPUTCRS=http://www.opengis.net/def/crs/EPSG/0/4326"

wcs_service = "SERVICE=WCS"
wcs_version = "VERSION=2.0.1"
wcs_request = "request=GetCoverage"
format = "format=GEOTIFF_INT16"
quantile  = "Q0.5"


for (i in 1:length(variables)){
  var = variables[i]
  for (j in 1:length(depth)){
    dep = depth[j]
    
    var_layer = paste(var, dep, quantile, sep = "_")
    
    wcs_path = paste0("https://maps.isric.org/mapserv?map=/map/", var, ".map")
    
    url_request = paste(
      wcs_path,
      paste0("coverageid=", var_layer),
      wcs_service, wcs_version, wcs_request,
      format, roi, subsettingcrs, outputcrs,
      sep = "&"
    )
    
    tiff = rast(url_request)
    
    depad = gsub("-", "", dep)
    
    swmap_wgs84[[paste0(var,depad)]] <- exact_extract(tiff[[1]], swmap_wgs84, 'mean')
    
  }
}

### Elevation

dem_aws = get_elev_raster(
  locations = swmap_wgs84,
  z         = 10,
  src       = "aws",        
  clip      = "locations"   
)

dem_aws = rast(dem_aws) 

swmap_wgs84$elev = exact_extract(dem_aws[[1]], swmap_wgs84, 'mean')

tri_riley = terrain(dem_aws, v = "TRIriley", neighbors = 8)

swmap_wgs84$ruggedness <- exact_extract(tri_riley, swmap_wgs84, "mean")

swmap_wgs84$area = as.numeric(st_area(swmap_wgs84)/1000)

### Response variable

dat = read.xlsx("/home/rstudio/project/Data/raw/Switzerland/Data_Muni_1999.xlsx")

dat = dat |> filter(year > 1899) |> mutate(firsh = first_sector/(first_sector + second_sector + third_sector)) |>
  select(mun_id, year, firsh, population) #|> pivot_wider(names_from = year, values_from = firsh, names_prefix = "firsh")

swdat = dat |> full_join(swmap_wgs84, by = c("mun_id" = "GMDE")) %>% drop_na() 

### Create cubic trend 

swdat = swdat |> mutate(trend = (year - 1890)/10, trendsq = trend*trend, trendcub = trendsq*trend, popdens = population/area, lnpopdens = log(popdens)) |> 
  select(-BEZIRK,-KT,-NAME,-area,-population)

### Save data

saveRDS(swdat, file = paste0(getwd(),"/project/Data/processed/swdat.rds"))
