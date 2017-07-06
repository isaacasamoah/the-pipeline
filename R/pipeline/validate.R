# validate

# expand in next version to create mapping tables for each mill using stringr library

# setup
rm(list = ls())

library(tidyverse)
library(stringr)

# load appended data

rake <- read_csv("data/pipeline/rake_appended.csv")
block <- read_csv("data/pipeline/block_appended.csv")
block_to_rake <- read_csv("data/pipeline/block_to_rake_appended.csv")

# load valid varieties list
valid_varieties <- read_csv("data/raw-data/valid-variety_list.csv") %>% 
  rename(variety = Variety)

# tidy up varieties -  rake


# check_varieties <- anti_join(rake %>% distinct(variety), valid_varieties, by = "variety")
# 
# recode_varieties <- check_varieties %>% 
#   filter(variety != "MIXED", variety != "EXP")

# check varieties - Q126 is from another region, Q239 does not exist in SPIDNET records 


# recode_varieties %>% 
# str_locate_all(check_varieties)

rake <- rake %>% 
  mutate(mixed_variety = if_else(variety == "MIXED", 1,0),
         check_variety = if_else(variety == "Q239", 1,0))

#check varieties
rake %>% 
  filter(check_variety == 1) %>% 
  distinct(variety)


rake$variety <- str_replace(rake$variety, "H56", "H56-752")
rake$variety <- str_replace(rake$variety, "CASS", "CASSIUS")
rake$variety <- str_replace(rake$variety, "KQ228A", "KQ228")


rake <-  rake %>% 
  mutate(variety = replace(variety, which(variety %in% c("EXP","MIXED") ), NA))

# check remapped varieties
anti_join(rake %>% distinct(variety), valid_varieties, by = "variety")


# tidy up varieties -  block

anti_join(block %>% distinct(variety), valid_varieties, by = "variety")

block <- block %>% 
  mutate(mixed_variety = if_else(variety == "MIXED", 1,0),
         check_variety = if_else(variety %in% c("Q239", "Q118"), 1,0))

#check varieties 
block %>% 
  filter(check_variety == 1) %>% 
  distinct(variety)

block$variety <- str_replace(block$variety, "H56", "H56-752")
block$variety <- str_replace(block$variety, "CASS", "CASSIUS")
block$variety <- str_replace(block$variety, "KQ228A", "KQ228")

block <-  block %>% 
  mutate(variety = replace(variety, which(variety %in% c("EXP","MIXED") ), NA))

anti_join(block %>% distinct(variety), valid_varieties, by = "variety")

# tidy up crop classes - rake
rake %>% 
  distinct(crop_class)

rake <- rake %>% 
  mutate( standover = if_else(crop_class == "SO", 1, 0),
          mixed_ratoons =  if_else(crop_class == "MIXED", 1, 0),
          check_ratoons = if_else(crop_class %in% c("R1R", "R2R", "R3R", "RP", "SO"), 1,0))

# check crop classes
rake %>% 
  filter(check_ratoons == 1) %>% 
  distinct(crop_class)

rake$crop_class <- str_replace(rake$crop_class, "PL", "P")
rake$crop_class <- str_replace(rake$crop_class, "RP", "P")
rake$crop_class <- str_replace(rake$crop_class, "R1R", "1R")
rake$crop_class <- str_replace(rake$crop_class, "R2R", "2R")
rake$crop_class <- str_replace(rake$crop_class, "R3R", "3R")
rake$crop_class <- str_replace(rake$crop_class, "OR", "4R_or_older")

rake <-  rake %>% 
  mutate(crop_class = replace(crop_class, which(crop_class %in% c("MIX","SO") ), NA))


rake %>% 
  distinct(crop_class)

# tidy up crop classes - block
block %>% 
  distinct(crop_class)

block <- block %>% 
  mutate( standover = if_else(crop_class == "SO", 1, 0),
          mixed_ratoons =  if_else(crop_class == "MIXED", 1, 0),
          check_ratoons = if_else(crop_class %in% c("R1R", "R2R", "R3R", "RP", "SO"), 1,0))

#check crop_classes
block %>% 
  filter(check_ratoons == 1)  %>% 
  distinct(crop_class)

block$crop_class <- str_replace(block$crop_class, "PL", "P")
block$crop_class <- str_replace(block$crop_class, "RP", "P")
block$crop_class <- str_replace(block$crop_class, "R1R", "1R")
block$crop_class <- str_replace(block$crop_class, "R2R", "2R")
block$crop_class <- str_replace(block$crop_class, "R3R", "3R")
block$crop_class <- str_replace(block$crop_class, "OR", "4R_or_older")

block <-  block %>% 
  mutate(crop_class = replace(crop_class, which(crop_class %in% c("MIX","SO") ), NA))


block %>% 
  distinct(crop_class)

# rebuild ids depeding on variety and cropclass
block <- block %>%
  mutate(block_crop_id = paste(block_id, variety, season, crop_class, sep = ""),
         block_crop_cycle_id =  paste(block_id, variety, sep = "")
  ) 

# write to csv  
rake %>% 
  write_csv("data/pipeline/rake_validated.csv")

block %>% 
  write_csv("data/pipeline/block_validated.csv")

block_to_rake %>% 
  write_csv("data/pipeline/block_to_rake_validated.csv")

# end