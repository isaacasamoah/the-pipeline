# make spatial

# setup

library(tidyverse)
library(stringr)
library(rgdal)
library(rgeos)

# read in cane block layer - add all years to list
tully_spatial <-  list(tully_spatial_2016 = readOGR("data/raw-data/spatial/F16_region.shp",stringsAsFactors = F))

# project to WGS84
tully_spatial_WGS84 <- spTransform(tully_spatial$tully_spatial_2016, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs "))

# build unique id

# inspect sub block
tully_spatial_WGS84@data$SubBlock

# inspect block_id length
unique(str_length(tully_spatial_WGS84@data$SubBlock))

# rules
  # if str_length = 2 add leading and trailing 0
  # if str_length = 3 and last character is numeric add trailing 0
  # if str_length = 3 and last character is alpha add leading 0

# stadardise sub block

# extract last character to help with standardisation decisions and logic
last_character <- sort(unique(substr(tully_spatial_WGS84@data$SubBlock, 
              str_length(tully_spatial_WGS84@data$SubBlock),
              str_length(tully_spatial_WGS84@data$SubBlock))))

last_character_numbers_coerced <- as.numeric(sort(unique(substr(tully_spatial_WGS84@data$SubBlock, 
                              str_length(tully_spatial_WGS84@data$SubBlock),
                              str_length(tully_spatial_WGS84@data$SubBlock)))))

last_character_numbers <- last_character_numbers_coerced[!is.na(last_character_numbers_coerced)]

last_character_letters <- last_character[is.na(last_character_numbers_coerced)]



tully_spatial_WGS84@data <- tully_spatial_WGS84@data %>% 
  mutate(standard_sub_block = if_else(str_length(SubBlock) == 2, str_c("0", SubBlock, "0"), 
                                      if_else(str_length(SubBlock) == 3 &
                                                substr(SubBlock,str_length(SubBlock),str_length(SubBlock)) %in% 
                                                last_character_numbers, str_c(SubBlock, "0"),
                                              if_else(str_length(SubBlock) == 3 &
                                                        substr(SubBlock,str_length(SubBlock),str_length(SubBlock)) %in% 
                                                        last_character_letters, str_c("0",SubBlock), SubBlock))))

# convert trailing character to number to match productivity format
substr(tully_spatial_WGS84@data$standard_sub_block,
       str_length(tully_spatial_WGS84@data$standard_sub_block), 
       str_length(tully_spatial_WGS84@data$standard_sub_block)) <- 

  tully_spatial_WGS84@data %>% 
  mutate(standard_sub_block = if_else(str_sub(standard_sub_block,str_length(standard_sub_block),str_length(standard_sub_block)) == "A","1",
                                               if_else(str_sub(standard_sub_block,str_length(standard_sub_block),str_length(standard_sub_block)) == "B","2",
                                                       if_else(str_sub(standard_sub_block,str_length(standard_sub_block),str_length(standard_sub_block)) == "C","3",
                                                               if_else(str_sub(standard_sub_block,str_length(standard_sub_block),str_length(standard_sub_block)) == "D","4",
                                                                       if_else(str_sub(standard_sub_block,str_length(standard_sub_block),str_length(standard_sub_block)) == "E","5",
                                                                               if_else(str_sub(standard_sub_block,str_length(standard_sub_block),str_length(standard_sub_block)) == "F","6",
                                                                                       if_else(str_sub(standard_sub_block,str_length(standard_sub_block),str_length(standard_sub_block)) == "G","7",str_sub(standard_sub_block,str_length(standard_sub_block),str_length(standard_sub_block)))
                                                                               ))))))) %>% 
  select(standard_sub_block) %>% 
  unlist()
         
  
# check sub block length
unique(str_length(tully_spatial_WGS84@data$standard_sub_block))

# inspect Farm
tully_spatial_WGS84@data$Farm

# inspect Farm length
unique(str_length(tully_spatial_WGS84@data$Farm))

# why are there 4 digit farm codes - missing "F"
tully_spatial_WGS84@data$Farm[str_length(tully_spatial_WGS84@data$Farm) == 4]

# standardise Farm
tully_spatial_WGS84@data <- tully_spatial_WGS84@data %>% 
  mutate(standard_farm =  if_else(str_length(Farm) == 5,
                                 substr(Farm,2, str_length(Farm)), Farm))

# create unique block_id
tully_spatial_WGS84@data %>% 
  mutate(block_id = str_c(standard_farm, standard_sub_block))


