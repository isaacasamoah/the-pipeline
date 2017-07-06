# aggregate analysis

# setup

rm(list = ls())

library(tidyverse)

# load functions
source("R/functions/prepare_cluster_data.R")
source("R/functions/predict_over_.R")
source("R/functions/cluster_tidy_rebuild.R") 

analysis_type <- "cluster"


# cluster analysis
if(analysis_type == "cluster"){
  
  # load aggregated data  
 aggregate_data_list <- list(abn = read_csv("data/pipeline/predicted_abn_over_season.csv") , 
                                    farm = read_csv("data/pipeline/predicted_farm_over_season.csv"),
                                    block = read_csv("data/pipeline/predicted_block_over_season.csv"))
  
grower_lookup <- read_csv("data/pipeline/grower_name_lookup_table.csv")


# prepare for clustering
clustering_variables <- list("predicted_tch", "predicted_ccs", "predicted_tsh")

prepared_cluster_data_list <- prepare_cluster_data(clustering_variables, aggregate_data_list, hectare_flag = 1)

# scree - think about best place for this
prepared_cluster_data_list$predicted_tch %>% 
  map(scree_this)

prepared_cluster_data_list$predicted_ccs %>% 
  map(scree_this)

prepared_cluster_data_list$predicted_tsh %>% 
  map(scree_this)

# cluster
clustering_variables <- list("predicted_tch", "predicted_ccs", "predicted_tsh")
cluster_no <- 8
column_naming_prefix <- "clustered"

clustered_data_list <- cluster_tidy_rebuild(clustering_variables, prepared_cluster_data_list, cluster_no,
                                            aggregate_data_list, column_naming_prefix)

clustered_data_list_grower <-  list(
  
  abn = clustered_data_list$abn %>% 
    rename(abn = unique_id) %>% 
    inner_join(grower_lookup %>% distinct(abn, grower_name), by = "abn"),
  
  farm = clustered_data_list$farm %>% 
    rename(farm_code = unique_id) %>% 
    inner_join(grower_lookup %>% distinct(farm_code, grower_name), by = "farm_code")
)


# write to csv  
# export clustered_data_list for secondary anlaysis

filenames <- paste("data/pipeline/clustered_",names(clustered_data_list),"_1994_2016.csv", sep = "")

# use map 2 to make names human readable

map2(clustered_data_list, filenames, write_csv)

# export clustered_data_list with grower for secondary anlaysis

filenames_with_grower <- paste("data/pipeline/clustered_",names(clustered_data_list_grower),
                               "_with_grower_name_1994_2016.csv", sep = "")

# use map 2 to make names human readable

map2(clustered_data_list_grower, filenames_with_grower, write_csv)

}


# end
