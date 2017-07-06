# make spatial

# setup

library(tidyverse)
library(rgdal)
library(rgeos)


# read in cane block layer - add all years to list
tully_spatial <-  list(tully_spatial_2016 = readOGR("data/raw-data/spatial/F16_region.shp"))

# project to WGS84
tully_spatial_WGS84 <- spTransform(tully_spatial$tully_spatial_2016, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs "))


# build unique id for each @data object for each year (each year has its own spatial object)
# first create block_id by concatenating farm and sub block
# then grab code from build_unique_id

# spatial index off the polygons - objects that have a heirarchy

#https://philmikejones.wordpress.com/2015/09/03/dissolve-polygons-in-r/