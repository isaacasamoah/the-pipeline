# load and append

# setup
rm(list = ls())

library(tidyverse)
library(readxl)
library(stringr)

# load data  for pipeline

# grower information including abn 

path <- "data/raw-data/grower_information.xls"
grower_information <- lapply(excel_sheets(path), read_excel, path = path)

# union all into one dataset

grower_information_combined <- data_frame()

for(i in 1:length(grower_information)) {
  
  grower_information_combined <- bind_rows(grower_information_combined, grower_information[[i]])
  
  i <- i +1
  
}

# for each farm_code select the latest abn (max season), so for every occurrence of the farm it will be mapped to the most current ABN

farm_code_season_reference <-  grower_information_combined %>% 
  ungroup %>% 
  group_by(farm_code) %>% 
  summarise(earliest_record = min(season), season = max(season))

# select latest record for each farm_code from combined table
grower_information_latest_record_unorderd <-  inner_join(grower_information_combined,
                                                         farm_code_season_reference
                                                         , by = c("farm_code", "season"))  %>% 
  select(-acnt, farm_code, grower_name, abn, season) %>% 
  rename(latest_record = season) %>%
  arrange(latest_record, farm_code)

grower_information_latest_record <- grower_information_latest_record_unorderd[,c(2, 3, 4, 5, 1)]

grower_information_latest_record %>% 
  write_csv("data/pipeline/grower_name_lookup_table.csv")

# explort abn to farm_code reference table for mapping of clusters
grower_information_latest_record %>% 
  select(abn, farm_code) %>% 
  arrange(abn, farm_code) %>% 
  write_csv("data/pipeline/abn_lookup_table.csv")
# end

# historic mill data
date_range <- "1994-2015"

# rake
rake_historic <- read_csv(paste("data/raw-data/tully-sugar-rake-",date_range,".csv", sep = "")) %>% 
  rename(crop_class = class_code) %>% 
  
  # create rake_id - unpack rake_season_id, remove season
  mutate(season_rake_id = as.character(code),
         rake_id = substr(as.character(season_rake_id),5,10)) %>% 
  
  select(- code)


#block
block_historic <- read_csv(paste("data/raw-data/tully-sugar-block-",date_range,".csv", sep = "")) %>% 
 
  # unpack unqiue id `CONCAT('B',code)` to block_id
  mutate(block_id = substr(`CONCAT('B',code)`, 7, 13)) %>% 
  
  # create block_crop_id as unique id for historic dataset and 
  # block_crop_cycle_id for stable yield estimates in historic dataset          
  mutate(block_crop_id = paste(block_id, variety, season, crop_class, sep = ""),
         block_crop_cycle_id =  paste(block_id, variety, sep = "")
         ) %>% 
  
  # drop oringal unique id `CONCAT('B',code)`)
  select(- `CONCAT('B',code)`) %>% 
  
  # order by crop_id
  arrange(block_crop_id)

# block to rake reference table
block_to_rake_historic <- read_csv(paste("data/raw-data/tully-sugar-block-to-rake-",date_range,".csv", sep = "")) %>% 
 
  # unpack `CONCAT('B',code)` to block_code
  mutate(block_id = substr(`CONCAT('B',block_code)`, 7, 13),
         
  # create rake_id - unpack rake_season_id, remove season        
  rake_id = substr(as.character(sample_code),5,10),
  
  # create block_rake_id - intersect of rake and block, relevant because  rakes are often split across multiple blocks
  block_rake_id = paste(block_id, rake_id, sep = ""),
  
  # create unique id for block_to_rake dataset, season_block_rake_id
  season_block_rake_id = paste(season,block_id, rake_id, sep = ""),
  
  # create season_rake_id to join to rake table
  season_rake_id = paste(season, rake_id, sep = "")
  
  ) %>%
  
  # drop redundant ids `CONCAT('B',code)`)
  select(season_block_rake_id, season_rake_id,  block_rake_id, rake_id, block_id, season, week, harv_tonnes,
         - `CONCAT('B',block_code)`, - sample_code, - code) %>%
  
  # order by season_block_rake_id
  arrange(season_block_rake_id)

# current mill data
current_year <- "2016"

# rake
rake_current <- read_csv(paste("data/raw-data/tully-sugar-rake-",current_year,".csv", sep = "")) %>% 
  rename(crop_class = class_code) %>% 
  
  # create season, season_rake_id
  mutate(season = as.integer(current_year),
         rake_id = as.character(code),
         season_rake_id = paste(season, rake_id, sep = "")) %>% 
  
  select(- code)


# block
block_current <- read_csv(paste("data/raw-data/tully-sugar-block-",current_year,".csv", sep = "")) %>% 
  
  mutate(season = as.integer(current_year),
         block_id = as.character(code),
         block_crop_id = paste(block_id, variety, season, crop_class, sep = ""),
         block_crop_cycle_id =  paste(block_id, variety, sep = "")
  ) %>% 

  arrange(block_crop_id)


# block to rake reference table
block_to_rake_current <- read_csv(paste("data/raw-data/tully-sugar-block-to-rake-",current_year,".csv", sep = "")) %>% 
  
  mutate(season = as.integer(current_year),
         block_id = as.character(block_code),
         rake_id = as.character(sample_code),
         block_rake_id = paste(block_id, rake_id, sep = ""),
         season_block_rake_id = paste(season,block_id, rake_id, sep = ""),
         season_rake_id = paste(season, rake_id, sep = "")
         ) %>%
  
  select(season_block_rake_id, season_rake_id, block_rake_id, rake_id, block_id, season, harv_tonnes) %>%
  
  arrange(season_block_rake_id)


# find common columns for bind_rows
common_columns_rake <- colnames(rake_historic)[colnames(rake_historic) %in% intersect(colnames(rake_historic), colnames(rake_current))]

common_columns_block <- colnames(block_historic)[colnames(block_historic) %in% intersect(colnames(block_historic), colnames(block_current))]

common_columns_block_to_rake <- colnames(block_to_rake_historic)[colnames(block_to_rake_historic) %in% intersect(colnames(block_to_rake_historic), colnames(block_to_rake_current))]


# append common columns 

rake_appended <- bind_rows(rake_historic[common_columns_rake], rake_current[common_columns_rake])

block_appended <- bind_rows(block_historic[common_columns_block], block_current[common_columns_block])

block_to_rake_appended <- bind_rows(block_to_rake_historic[common_columns_block_to_rake], block_to_rake_current[common_columns_block_to_rake])

# add padding 0 to block_id to make consistent with cane block layer unique id.


block_appended$block_id <-  str_c(substr(block_appended$block_id,1,4),"0",substr(block_appended$block_id,5,7))
block_to_rake_appended$block_id <- str_c(substr(block_to_rake_appended$block_id,1,4),"0",substr(block_to_rake_appended$block_id,5,7))


# write to csv

rake_appended %>% 
  write_csv("data/pipeline/rake_appended.csv")

block_appended %>% 
  write_csv("data/pipeline/block_appended.csv")

block_to_rake_appended %>% 
  write_csv("data/pipeline/block_to_rake_appended.csv")

# end

