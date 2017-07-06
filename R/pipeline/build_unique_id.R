# build unique id to link cane block layer and productivity data


# to do

# check length and characteristics of each string and apply transofrmations dynamically
# apply to all years of cane block layer
# setup
rm(list = ls())

library(tidyverse)
library(stringr)

# load data

block_clusters <- read_csv("data/pipeline/clustered_block_1994_2016.csv")


cane_block_layer <- read_csv("data/T_CBL_16.csv") %>% 
  mutate(unique_id = Block_No)

cane_block_layer %>% 
  mutate(letter = str_sub(cane_block_layer$unique_id,9,9)) %>%
  filter(letter %in% c("1","2", "3","4","5", "6", "7", "8", "9")) %>% 
  select(unique_id, letter)

cane_block_layer %>% 
  mutate(letter = str_sub(cane_block_layer$unique_id,9,9)) %>%
  filter(letter %in% c("1","2", "3","4","5", "6", "7", "8", "9")) %>% 
  distinct(letter)

# look at letters
cane_block_layer %>% 
  distinct(unique_id) %>% 
  mutate(letter = str_sub(unique_id,9,9)) %>% 
  distinct(letter)

# look at max length id
cane_block_layer %>% 
  distinct(unique_id) %>% 
  mutate(letter = str_sub(unique_id,9,9),
         length = str_length(unique_id)) %>% 
  filter(str_length(unique_id) == max(str_length(unique_id)))

# check lengths
cane_block_layer %>% 
  distinct(unique_id) %>% 
  mutate(letter = str_sub(unique_id,9,9),
         length = str_length(unique_id)) %>% 
  distinct(length)  

# add trailing zero on < 10 ids with 1:9 as last char
cane_block_layer$unique_id[str_length(cane_block_layer$unique_id) == 9 & 
                             substr(cane_block_layer$unique_id,9,9) %in% 
                             c("1","2", "3","4","5", "6", "7", "8", "9")] <- 
  
  str_c( cane_block_layer$unique_id[str_length(cane_block_layer$unique_id) == 9 & 
                                      substr(cane_block_layer$unique_id,9,9) %in% 
                                      c("1","2", "3","4","5", "6", "7", "8", "9")],"0")

# check letter
cane_block_layer %>% 
  distinct(unique_id) %>% 
  mutate(letter = str_sub(unique_id,10,10)) %>% 
  distinct(letter)

# remove dash or add padding 0

str_sub(
  
  str_sub(cane_block_layer$unique_id[str_length(cane_block_layer$unique_id) < 10],
          1,max(str_length(cane_block_layer$unique_id))),
  
  6,6) <- "0"

str_sub(
  
  str_sub(cane_block_layer$unique_id[str_length(cane_block_layer$unique_id) == 10],
          1,max(str_length(cane_block_layer$unique_id))),
  
  6,6) <- ""




# check lengths
cane_block_layer %>% 
  distinct(unique_id) %>% 
  mutate(letter = str_sub(unique_id,9,9),
         length = str_length(unique_id)) %>% 
  distinct(length)

# add padding 0 at end for ids with no trailing letter
str_sub(cane_block_layer$unique_id[str_length(cane_block_layer$unique_id) == 8],1,8) <- 
  str_c(str_sub(cane_block_layer$unique_id[str_length(cane_block_layer$unique_id) == 8],1,8),"0")

# check lengths
cane_block_layer %>% 
  distinct(unique_id) %>% 
  mutate(length = str_length(unique_id)) %>% 
  distinct(length)

# remove leading F
str_sub(cane_block_layer$unique_id,1,1) <- ""

# check ids have no "f"
cane_block_layer %>% 
  distinct(unique_id)

# check lengths
cane_block_layer %>% 
  distinct(unique_id) %>% 
  mutate(length = str_length(unique_id)) %>% 
  distinct(length)

cane_block_layer %>% 
  mutate(letter = str_sub(cane_block_layer$unique_id,8,8)) %>% 
  distinct(letter)



# map letters to numbers
str_sub(cane_block_layer$unique_id,8,8) <- 
  
  if_else(str_sub(cane_block_layer$unique_id,8,8) == "A","1",
          if_else(str_sub(cane_block_layer$unique_id,8,8) == "B","2",
                  if_else(str_sub(cane_block_layer$unique_id,8,8) == "C","3",
                          if_else(str_sub(cane_block_layer$unique_id,8,8) == "D","4",
                                  if_else(str_sub(cane_block_layer$unique_id,8,8) == "E","5",
                                          if_else(str_sub(cane_block_layer$unique_id,8,8) == "F","6",
                                                  if_else(str_sub(cane_block_layer$unique_id,8,8) == "G","7",
                                                          if_else(str_sub(cane_block_layer$unique_id,8,8) == "0","0","check"))))))))


# check final unique_id
cane_block_layer %>% 
  distinct(unique_id) %>% 
  arrange(unique_id)


block_clusters_for_lookup <- block_clusters %>% 
  mutate(unique_id = as.character(unique_id))


inner_join(cane_block_layer %>% distinct(unique_id), block_clusters_for_lookup) %>% 
  arrange(unique_id)

anti_join(cane_block_layer %>% distinct(unique_id), block_clusters_for_lookup) %>% 
  arrange(unique_id)

anti_join(block_clusters_for_lookup %>% 
            distinct(unique_id),cane_block_layer %>% distinct(unique_id)) %>% 
  arrange(unique_id)


# create lookup table
# 
# left_join(block_clusters_for_lookup, cane_block_layer, by = "unique_id" )

# write cane block layer with unique id to csv

cane_block_layer %>% 
write_csv("data/tully_cane_block_layer_16_unique_id.csv")

# end
