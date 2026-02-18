library(tidyverse)
library(openxlsx)
library(sf)
library(XML)
library(gdalUtilities)
library(terra)
library(exactextractr)
library(elevatr)

### GER shapefile

germap = read_sf(paste0(getwd(), "/project/Data/raw/Germany/Shapefiles/harvard-german1895electoraldistricts-geojson.json"))

germap = germap |> filter(DISTRICT_N > 0)

### Soil data

# prep to pull the data

bbsw <- st_bbox(germap)

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
    
    germap[[paste0(var,depad)]] <- exact_extract(tiff[[1]], germap, 'mean')
    
  }
}

### Elevation

dem_aws <- get_elev_raster(
  locations = germap,
  z         = 10,
  src       = "aws",        
  clip      = "locations"   
)

dem_aws <- rast(dem_aws) 

germap$elev <- exact_extract(dem_aws[[1]], germap, 'mean')

tri_riley <- terrain(dem_aws, v = "TRIriley", neighbors = 8)

germap$ruggedness <- exact_extract(tri_riley, germap, "mean")

germap = germap |> mutate(DISTRICT_N = ifelse(DISTRICT_N == 3136, 31, DISTRICT_N))

germap$area = as.numeric(st_area(germap)/1000)

### Response variable

dat = read.xlsx("/home/rstudio/project/Data/raw/Germany/Data_1890_1918.xlsx")

dat <- dat |>  rename(year = Jahr, DISTRICT_N = Wahlkreis_Nummer, firsh = Erster_Sektor) |>  mutate(firsh = firsh / 100) |> group_by(DISTRICT_N) |>
  arrange(year, .by_group = TRUE) |>  mutate(population = approx(x = year, y = pop, xout = year)$y) |>  ungroup() |>  filter(year %in% c(1895, 1907)) |>
  select(year, DISTRICT_N, population, firsh)

gerdat = dat |> full_join(germap, by = "DISTRICT_N") |> drop_na() 

### Create cubic trend 

gerdat = gerdat |> mutate(trend = (year - 1890)/10, trendsq = trend*trend, trendcub = trendsq*trend, popdens = population/area, lnpopdens = log(popdens)) |> 
  select(-OBJECTID,-SHAPE_LENG,-SHAPE_AREA,-SHAPE_LEN,-population,-id, -DISTRICT)

### Save data

saveRDS(gerdat, file = paste0(getwd(),"/project/Data/processed/gerdat.rds"))

