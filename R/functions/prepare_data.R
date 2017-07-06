test_threshholds <-  function(data_list, max_no_unique_ids) {
  
  threshold_list <-  rep_along(data_list, max_no_unique_ids)
  
  for(i in seq_along(threshold_list)){
    
    threshold_test_results[[i]] <- length(unique(data_list[[i]][[unique_id_list[[i]]]])) <= threshold_list[[i]]
    
  }
  
  threshold_test_results
}


check_factors <-  function(data_list, unique_id_list, summarising_over) {
  
  for( i in seq_along(data_list)) {
    
    
    if(is.factor(data_list[[i]][[unique_id_list[[i]]]]) == F) {
      
      data_list[[i]][[unique_id_list[[i]]]] <- as.factor( data_list[[i]][[unique_id_list[[i]]]])
    }
    
    if(is.factor(data_list[[i]][[summarising_over]]) == F) {
      
      data_list[[i]][[summarising_over]] <- as.factor( data_list[[i]][[summarising_over]])
    }
    
  }
  
  checked_data_list <- data_list
  
  checked_data_list
}


create_unique_ids <- function(checked_data_list, threshold_test_results, unique_id_list) {
  
  for(i in seq_along(checked_data_list)) {
    
    if(threshold_test_results[[i]] == F) {
      
      if(!is.null(checked_data_list[[i]][["variety"]]) 
         & !is.null(checked_data_list[[i]][["crop_class"]])) {
        
        checked_data_list[[i]][["unique_id"]] <-  paste(checked_data_list[[i]][["variety"]], checked_data_list[[i]][["crop_class"]], sep = "")
        
        # checked_data_list[[i]][["unique_id"]] <- as.factor(checked_data_list[[i]][["unique_id"]])
        
        checked_data_list[[i]] <- subset(checked_data_list[[i]],!is.na(checked_data_list[[i]][["variety"]]) & !is.na(checked_data_list[[i]][["crop_class"]]))
        
      } else {
        
        stop("unique_id has too many levels, no alternatives for unique_id found")
        
      }
      
    } else {
      
      checked_data_list[[i]][["unique_id"]] <-  as.factor(checked_data_list[[i]][[unique_id_list[[i]]]])
      
    }
  }
  
  unique_id_data_list <- checked_data_list
  
  unique_id_data_list
}


prepare_data <- function(threshold_list, max_no_unique_ids, data_list, unique_id_list, summarising_over = "season" ) {
  
  threshold_test_results <- test_threshholds(data_list, max_no_unique_ids)
  
  checked_data_list <- check_factors(data_list, unique_id_list, summarising_over)
  
  unique_id_data_list <- create_unique_ids(data_list, threshold_test_results, unique_id_list)
  
  prepared_data_list <- unique_id_data_list
  
  prepared_data_list
}
