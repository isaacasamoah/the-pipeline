prepare_cluster_data <- function(clustering_variables, input_data_list, hectare_flag = 1) {
  
  prepared_cluster_data <- vector("list", length(input_data_list))
  prepared_cluster_data_list <- vector("list", length(clustering_variables))
  
  for(i in seq_along(clustering_variables)) {
    
    for(j in seq_along(input_data_list)) {
      
    if(hectare_flag == 1)   {
      prepared_cluster_data[[j]] <- input_data_list[[j]][
        
        colnames(input_data_list$block) %in%
          
          c("predicted_harvested_area",clustering_variables[[i]])] %>%
        
        na.omit %>% 
        
        scale 
    } else {
      
      prepared_cluster_data[[j]] <- input_data_list[[j]][
        
        colnames(input_data_list$block) %in%
          
          clustering_variables[[i]]] %>%
        
        na.omit %>% 
        
        scale 
          }
      
    }
    names(prepared_cluster_data) <- names(input_data_list)
    prepared_cluster_data_list[[i]] <- prepared_cluster_data
  }
  names(prepared_cluster_data_list) <- clustering_variables
  
  prepared_cluster_data_list
  
}