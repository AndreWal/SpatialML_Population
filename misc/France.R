library(tidyverse)
library(sf)
library(haven)

# 1872

pop = read_dta("/home/blablub/Desktop/Dropbox/Projects/Machine_Learning/Own_Projects/Spatial_Predictions/data/raw/FRA/COG_COMMUNES_1872.dta")

pop = pop |> mutate(deppct = as.numeric(deppct)) |> group_by(deppct) |> summarize(pop = sum(pop))

map = st_read("/home/blablub/Desktop/Dropbox/Projects/Machine_Learning/Own_Projects/Spatial_Predictions/data/raw/FRA/Shapefile/CANTONS_1872.shp")

map = map |> full_join(pop, by = c("deppct" = "deppct"))


plot(map["pop"])

hist(log(map$pop))

# 1881

pop = read_dta("/home/blablub/Desktop/Dropbox/Projects/Machine_Learning/Own_Projects/Spatial_Predictions/data/raw/FRA/COG_COMMUNES_1881.dta")

pop = pop |> mutate(deppct = as.numeric(deppct)) |> group_by(deppct) |> summarize(pop = sum(pop))

map = st_read("/home/blablub/Desktop/Dropbox/Projects/Machine_Learning/Own_Projects/Spatial_Predictions/data/raw/FRA/Shapefile/CANTONS_1881.shp")

map = map |> full_join(pop, by = c("deppct" = "deppct"))


plot(map["pop"])

hist(log(map$pop))

# 1891

pop = read_dta("/home/blablub/Desktop/Dropbox/Projects/Machine_Learning/Own_Projects/Spatial_Predictions/data/raw/FRA/COG_COMMUNES_1891.dta")

pop = pop |> mutate(deppct = as.numeric(deppct)) |> group_by(deppct) |> summarize(pop = sum(pop))

map = st_read("/home/blablub/Desktop/Dropbox/Projects/Machine_Learning/Own_Projects/Spatial_Predictions/data/raw/FRA/Shapefile/CANTONS_1891.shp")

map = map |> full_join(pop, by = c("deppct" = "deppct"))


plot(map["pop"])

hist(log(map$pop))


# 1901

pop = read_dta("/home/blablub/Desktop/Dropbox/Projects/Machine_Learning/Own_Projects/Spatial_Predictions/data/raw/FRA/COG_COMMUNES_1901.dta")

pop = pop |> mutate(deppct = as.numeric(deppct)) |> group_by(deppct) |> summarize(pop = sum(pop))

map = st_read("/home/blablub/Desktop/Dropbox/Projects/Machine_Learning/Own_Projects/Spatial_Predictions/data/raw/FRA/Shapefile/CANTONS_1901.shp")

map = map |> full_join(pop, by = c("deppct" = "deppct"))


plot(map["pop"])

hist(log(map$pop))

# 1911

pop = read_dta("/home/blablub/Desktop/Dropbox/Projects/Machine_Learning/Own_Projects/Spatial_Predictions/data/raw/FRA/COG_COMMUNES_1911.dta")

pop = pop |> mutate(deppct = as.numeric(deppct)) |> group_by(deppct) |> summarize(pop = sum(pop))

map = st_read("/home/blablub/Desktop/Dropbox/Projects/Machine_Learning/Own_Projects/Spatial_Predictions/data/raw/FRA/Shapefile/CANTONS_1911.shp")

map = map |> full_join(pop, by = c("deppct" = "deppct"))


plot(map["pop"])

hist(log(map$pop))

# 1911

pop = read_dta("/home/blablub/Desktop/Dropbox/Projects/Machine_Learning/Own_Projects/Spatial_Predictions/data/raw/FRA/COG_COMMUNES_1936.dta")

pop = pop |> mutate(deppct = as.numeric(deppct)) |> group_by(deppct) |> summarize(pop = sum(pop))

map = st_read("/home/blablub/Desktop/Dropbox/Projects/Machine_Learning/Own_Projects/Spatial_Predictions/data/raw/FRA/Shapefile/CANTONS_1911.shp")

map = map |> full_join(pop, by = c("deppct" = "deppct"))


plot(map["pop"])

hist(log(map$pop))