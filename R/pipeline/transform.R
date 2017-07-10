# transform

# setup
rm(list = ls())

library(tidyverse)

# load validated data

rake <- read_csv("data/pipeline/rake_validated.csv")
block <- read_csv("data/pipeline/block_validated.csv")
block_to_rake <- read_csv("data/pipeline/block_to_rake_validated.csv")
grower_name_lookup_table <- read_csv("data/pipeline/grower_name_lookup_table.csv")

# Build to block level

# Join rake and block to rake datasets to map rakes and mill information to blocks and harvest area
block_to_rake_joined <- left_join(rake  %>%
                                    select(-harv_tonnes),
                                    block_to_rake %>% 
                                    select(season,rake_id, block_id, harv_tonnes),
                                            by = c("season","rake_id"))  %>% 
  
  rename(harv_tonnes_block_rake = harv_tonnes) %>% 
  
  mutate(harv_tonnes_block_rake_ccs = harv_tonnes_block_rake*ccs,
         harv_tonnes_block_rake_fibre = harv_tonnes_block_rake*fibre)

# summarise joined rakes and blocks to block level
block_from_rake <- block_to_rake_joined %>% 
  group_by(block_id, season) %>% 
  summarise(harv_tonnes_block = sum(harv_tonnes_block_rake, na.rm =T), 
            harv_tonnes_block_ccs = sum(harv_tonnes_block_rake_ccs, na.rm =T),
            harv_tonnes_block_fibre =sum(harv_tonnes_block_rake_fibre, na.rm =T))

# join block to rake summarised to block to block dataset
block_level <- left_join(block, block_from_rake, by = c("season","block_id")) %>% 
  rename(harv_tonnes_block_orig = harv_tonnes, harv_tonnes_block_calc = harv_tonnes_block)


# Build to Farm level
farm_level <- 
  block_level %>% 
  filter(!is.na(farm_code)) %>% 
  ungroup %>% 
  group_by(season, farm_code) %>% 
  summarise(harvested_area = sum(harvested_area, na.rm =T),
            harv_tonnes_farm = sum(harv_tonnes_block_orig, na.rm =T),
            harv_tonnes_farm_ccs = sum(harv_tonnes_block_ccs, na.rm =T),
            harv_tonnes_farm_fibre = sum(harv_tonnes_block_fibre, na.rm =T))
 

# Build to ABN level

block_level_with_abn <- left_join(block_level, 
                                  grower_name_lookup_table, 
                                  by = "farm_code")

abn_level <- block_level_with_abn %>% 
  filter(!is.na(abn)) %>% 
  group_by(season, abn) %>% 
  summarise(harvested_area = sum(harvested_area, na.rm =T),
            harv_tonnes_abn = sum(harv_tonnes_block_calc, na.rm =T),
            harv_tonnes_abn_ccs = sum(harv_tonnes_block_ccs, na.rm =T),
            harv_tonnes_abn_fibre = sum(harv_tonnes_block_fibre, na.rm =T)) %>% 
  arrange(season, abn) 

# transform checks - good

block_level %>% ungroup %>%  summarise(sum(harv_tonnes_block_calc, na.rm = T)) == 
  farm_level %>% ungroup %>% summarise(sum(harv_tonnes_farm, na.rm = T)) 

farm_level %>% ungroup %>% summarise(sum(harv_tonnes_farm, na.rm = T)) ==
abn_level %>% ungroup %>% summarise(sum(harv_tonnes_abn, na.rm = T))

# map each level to district and district name

#create reference tables
block_to_district <- block_to_rake_joined %>% 
  distinct(block_id, district_code, district_name)

farm_to_district <- block_to_rake_joined %>% 
  distinct(farm_code, district_code, district_name)


# map block to district
block_level <- block_level %>% left_join(block_to_district, by = "block_id") %>% 
  distinct(district_code, district_name)

#map farm to district
farm_level <- farm_level %>%  left_join(farm_to_district, by = "farm_code")

# write to csv

block_level %>% 
  write_csv("data/pipeline/block_level.csv")

farm_level %>% 
  write_csv("data/pipeline/farm_level.csv")

abn_level %>% 
  write_csv("data/pipeline/abn_level.csv")

# end


