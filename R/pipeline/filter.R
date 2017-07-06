# filter

rm(list = ls())

library(tidyverse)


# load transformed data

block_level <- read_csv("data/pipeline/block_level.csv")
farm_level <- read_csv("data/pipeline/farm_level.csv")
abn_level <- read_csv("data/pipeline/abn_level.csv")

# filter using outlier detection rules


rules <- "2016032"

if(rules == "2016032") {
  
  # find farms that have existed for 10 years or more
  farms_min_10_years <-  block_level %>% 
    group_by(farm_code) %>% 
    summarise(years = max(season) - min(season) + 1) %>% 
    filter(years >= 10)
  
  block_level_2016032 <-  block_level %>% 
    
    mutate(tch = if_else(harvested_area > 0,harv_tonnes_block_calc/harvested_area,0),
           ccs = harv_tonnes_block_ccs/harv_tonnes_block_calc,
           tsh = ccs*tch/100) %>% 
    
    filter(tch <= 250, 
           tsh <= 30, 
           ccs <= 20,
           season != 2011,
           as.character(farm_code) %in% as.character(farms_min_10_years$farm_code))
  
  anti_join(block_level_2016032, farms_min_10_years, by = "farm_code")
  
  farm_level_2016032 <- farm_level %>% 
    
    mutate(tch = if_else(harvested_area > 0,harv_tonnes_farm/harvested_area,0),
           ccs = harv_tonnes_farm_ccs/harv_tonnes_farm,
           tsh = ccs*tch/100) %>% 
    
    filter(tch <= 250, 
           tsh <= 30, 
           ccs <= 20,
           season != 2011,
           as.character(farm_code) %in% as.character(farms_min_10_years$farm_code))
  
  abn_level_2016_032 <- abn_level %>% 
    
    mutate(tch = if_else(harvested_area > 0,harv_tonnes_abn/harvested_area,0),
           ccs = harv_tonnes_abn_ccs/harv_tonnes_abn,
           tsh = ccs*tch/100) %>% 
    
    filter(tch <= 250, 
           tsh <= 30, 
           ccs <= 20,
           season != 2011)
}

# write to csv
block_level_2016032 %>% 
  write_csv("data/pipeline/block_level_2016032.csv")

farm_level_2016032 %>% 
  write_csv("data/pipeline/farm_level_2016032.csv")

abn_level_2016_032 %>% 
  write_csv("data/pipeline/abn_level_2016_032.csv")

# end

