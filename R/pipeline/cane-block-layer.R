
# xml parser for Java - 

# testing cbl

rm(list = ls())
# options(repos = c(CRAN = "https://cran.revolutionanalytics.com"))
if (!require("pacman")) install.packages("pacman")
pacman::p_load(shapefiles, maptools, rgeos, ggmap, rgdal, leaflet)

library(shapefiles)
library(tidyverse)
library(maptools)
library(rgeos)
library(ggmap)
library(RColorBrewer)
library(rgdal)
library(leaflet)


rgdal_test <-  readOGR("data/cbl/F16_region.shp")

rgdal_test@proj4string

# using maptools
tully_cbl <- readShapePoly("data/raw-data/spatial/F16_region.shp")

# using rgdal - grabs projection with read in
tully_cbl <-  readOGR("data/raw-data/spatial/F16_region.shp")

plot(tully_cbl)

tully_cbl@data


colors <- brewer.pal(9, "BuGn")

# plot Tully area
tully <- get_map(c(long = 145.927686 , lat = -17.939756),
                    color = "color",
                    source = "google",
                    maptype = "satellite",
                    zoom = 11)

tully_map <- ggmap(tully)

# now fortify tully_cbl -  cnvert shape file to dataframe

tully_cbl_fortified <- fortify(tully_cbl)


# and plot

tully_map +
  geom_polygon(aes(x = long,
                   y = lat,
                   group = group),
               data = tully_cbl_fortified) +
  labs(x = "Longitude",
       y = "Latitude")

# hmmm not lining up try geom_polygon example from help

tully_co_ordinate <- tully_cbl_fortified %>% 
  rename(id_orig = id) %>% 
  mutate(id = as.integer(id_orig) + 1) %>% 
  select(-id_orig)

tully_cbl_data <- tully_cbl@data %>% bind_cols(data_frame(id = seq(1,8128,1)))

?seq
# read in updated shape file
tully_cbl_data_updated <- read_csv("data/tully_cane_block_layer_16_unique_id.csv") %>% 
  rename(id = OBJECTID)

tully_tch_clusters_2016 <- read_csv("data/pipeline/clustered_block_1994_2016.csv")

tully_cbl_clusters <- left_join(tully_cbl_data_updated %>% mutate(unique_id = as.integer(unique_id)), tully_tch_clusters_2016, by = "unique_id")

tully_cbl_clusters %>% 
  distinct(unique_id)

tully_cbl_polygons <- merge(tully_co_ordinate, tully_cbl_clusters, by = "id") %>% 
  mutate(CaneYield = replace(CaneYield, which(CaneYield > 250), NA),
         clustered_predicted_tch = as.factor(clustered_predicted_tch))

?replace

tully_cbl_polygons %>% 
  filter(CaneYield >250) %>% 
  filter(!is.na(CaneYield))

#now plot

tully_tch_cluster_map_16 <- tully_cbl_polygons %>% 
ggplot(aes(x = long, y = lat)) +
  geom_polygon(aes(fill = clustered_predicted_tch, group = id)) +
  scale_fill_discrete(guide = guide_legend(title = "Tch Cluster")) +
  theme_void()
 

# lets line deal with the map projection to line up with google maps


??rgdal

# unsuccessful
spTransform(tully_cbl, CRS("+proj=longlat +ellps=clrk66"))

# get centroids
coordinates(tully_cbl)

coordinates(spTransform(tully_cbl, CRS("+proj=longlat +datum=WGS84")))



# try again - CRS is missing from arc gis file, assign the following from https://gis.stackexchange.com/questions/93133/how-to-get-specific-coordinate-system-to-get-arcgis-point-shapefile-in-r

# then convert to lat long and see if it lines up with google map

# get correct adg pojection proj4string from http://www.spatialreference.org/ref/epsg/20355/proj4/

tully_cbl@proj4string <- CRS("+proj=utm +zone=55 +south +ellps=aust_SA +units=m +no_defs")

# reproject agd84 gta94 wgs84 atd84

# transfrom
tully_cbl_lat_long <- spTransform(tully_cbl, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs "))

# check proj4string
tully_cbl_lat_long@proj4string

proj4string

# re grab tully map
tully <- get_map( c(long = 145.927686 , lat = -17.939756),
                 color = "color",
                 source = "google",
                 maptype = "hybrid",
                 zoom = 11)


# have a look at map
ggmap(tully) 

# setup up transformed shape file to overlay

# fortify (create ggmappable opbect)
tully_cbl_lat_long_fortified <- fortify(tully_cbl_lat_long)

# adjust id to match productivty data
tully_lat_long_co_ordinate <- tully_cbl_lat_long_fortified %>% 
  rename(id_orig = id) %>% 
  mutate(id = as.integer(id_orig) + 1) %>% 
  select(-id_orig)

# merge with data
tully_cbl_lat_long_polygons <- merge(tully_lat_long_co_ordinate, tully_cbl_clusters, by = "id") %>% 
  mutate(CaneYield = replace(CaneYield, which(CaneYield > 250), NA),
         clustered_predicted_tch = as.factor(clustered_predicted_tch))

# overlay on google map -success - this is the baseline process for all cane block layer years.

tully_tch_cluster_map_16 <- ggmap(tully) + geom_polygon(data =  tully_cbl_lat_long_polygons, 
                            aes(x = long, y = lat, fill = clustered_predicted_tch, group = id
                               ),  alpha = 0.5) +
  scale_fill_brewer(type = "div", palette = 2, direction = 1, guide = guide_legend(title = "Tch Cluster")) +
  theme_void()



# order clusters by tch scale

tully_cbl_lat_long_polygons_tch_ordered <- tully_cbl_lat_long_polygons %>% 
  mutate(clustered_predicted_tch_ordered = as.factor(if_else(as.integer(clustered_predicted_tch) == 5, 1, 
                                           if_else(as.integer(clustered_predicted_tch) == 7, 2,
                                                   if_else(as.integer(clustered_predicted_tch) == 6, 3, 1,
                                                           if_else(as.integer(clustered_predicted_tch) == 3, 4,
                                                                   if_else(as.integer(clustered_predicted_tch) == 8, 5,
                                                                           if_else(as.integer(clustered_predicted_tch) == 1, 6,
                                                                                   if_else(as.integer(clustered_predicted_tch) == 4, 7,
                                                                                           if_else(as.integer(clustered_predicted_tch) == 2, 8,0))))))))))

tully_cbl_lat_long_polygons %>% 
  distinct(clustered_predicted_tch)

tully_cbl_lat_long_polygons_tch_ordered %>% 
  distinct(clustered_predicted_tch)

tully_tch_cluster_map_16_ordered <- ggmap(tully) + geom_polygon(data =  tully_cbl_lat_long_polygons_tch_ordered, 
                            aes(x = long, y = lat, fill = clustered_predicted_tch, group = id
                            ),  alpha = 0.5) +
  scale_fill_brewer(type = "seq", palette = 8, direction = 1, guide = guide_legend(title = "Tch Cluster")) +
  theme_void()
 


# use leaflet

library(leaflet)

m <- leaflet(tully_cbl_lat_long) %>%
  addPolygons(fillColor = topo.colors(20, alpha = NULL), stroke = FALSE) %>% 
  addTiles()
m  # Print the map

?addPolygons
?leaflet
?get_map

# lets add some data to this leaflet map

tully_cbl_lat_long@data$CaneYield 

binpal <- colorNumeric(topo.colors(20,alpha = NULL), tully_cbl_lat_long@data$CaneYield)

m <- leaflet(tully_cbl_lat_long) %>%
  addPolygons(color = ~binpal(CaneYield), stroke = FALSE, smoothFactor = 0.2, fillOpacity = 0.5) %>% 
  addTiles() %>%
  addLegend("bottomright", pal = binpal, values = ~CaneYield,
            title = "Cane Yield 2016",
            labFormat = labelFormat(suffix = "t/ha"),
            opacity = 1
  )
m  # Print the map

tully_soils <- readOGR("data/Soil maps/Soils_wet_tropical_coast_study_Cardwell_Tully_Innisfail_area_CTI.shp")

tully_soils@proj4string

tully_soils_projected <-  spTransform(tully_soils, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs "))

tully_soils@data

binpal <- colorBin(topo.colors(20,alpha = NULL), tully_soils_projected@data$DOMINANT_2,  8, pretty = FALSE)

m <- leaflet(tully_soils_projected) %>%
  addPolygons( stroke = FALSE) %>% 
  addTiles()
  
m  # Prin

# create fake cane yield for Dion

# # using shapefiles pkg  - crap
# 
# ?convert.to.simple
# 
# # read in shape file
# tully_cbl <-  read.shapefile("data/cbl/F16_region")
# 
# # read in updated shape file
# tully_cbl_data <- read_csv("data/tully_cane_block_layer_16_unique_id.csv")
# 
# str(tully_cbl)
# 
# # replace original dbf with modified dbf from Rod and with my unique id
# tully_cbl$dbf$dbf <- tully_cbl_data
# 
# tully_cbl$dbf$dbf 
# 
# 
# tully_cbl_simple  <- convert.to.simple(tully_cbl$shp)
# 
# 
# tully_cbl_simple_unique_id <-  change.id(tully_cbl_simple, tully_cbl$dbf$dbf$unique_id)
# 
# # lets check map projection
# 
# ?proj4string

