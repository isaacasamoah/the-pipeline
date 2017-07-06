# aggregate

# setup
rm(list = ls())

library(tidyverse)
library(stringr)
library(lme4)

# load prepare data adn predict over functions 

# load data preparation funcitons to get ready for prediction
source("R/functions/prepare_data.R")

# load predict_over functions to summarise over all seasons
source("R/functions/predict_over_.R")

# load filtered data

block_level_2016032 <- read_csv("data/pipeline/block_level_2016032.csv")

farm_level_2016032 <-  read_csv("data/pipeline/farm_level_2016032.csv")

abn_level_2016_032 <- read_csv("data/pipeline/abn_level_2016_032.csv")

# setup user inputs for removing seasonal effects

# user inputs
summarising_over <-  "season"
number_of_datasets <- 3
max_no_unique_ids <- 1000
# column_naming_prefix <- "predicted"


# setup user input lists

# vectors of type list, length = number_of_datasets
data_list  <-  vector("list", number_of_datasets)
unique_id_list <- vector("list", number_of_datasets)
threshold_list <- vector("list", number_of_datasets)
threshold_test_results <- vector("list", number_of_datasets)

# user input lists
data_list <-  list(abn = abn_level_2016_032, farm = farm_level_2016032, block = block_level_2016032)
unique_id_list <-  list("abn", "farm_code", "block_id")
prediction_variables <- list("tch","ccs", "tsh", "harvested_area")


# prepare data for predicting over seasons

prepared_data_list <- prepare_data(threshold_list, max_no_unique_ids, data_list, unique_id_list, summarising_over)

# predict outputs of interest over seasons for 2d clustering

predicted_over_data_list <- predict_over_(prediction_variables, prepared_data_list, max_no_unique_ids, unique_id_list)

# export predicted_over_data_list for cluster anlaysis

filenames <- paste("data/pipeline/predicted_",names(predicted_over_data_list),"_over_season.csv", sep = "")

# use map 2 to make names human readable

map2(predicted_over_data_list, filenames, write_csv)

# end
