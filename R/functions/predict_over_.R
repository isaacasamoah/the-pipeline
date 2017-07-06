cross_predict <-  function(prediction_variables, prepared_data_list) {
  
  prediction_results <-  vector("list", length = length(prediction_variables))
  
  for(i in seq_along(prediction_variables)){
    
    prediction_results[[i]]  <-   prepared_data_list %>%  
      
      map(~ predict(
        lmer(.[[prediction_variables[[i]]]] ~ unique_id + (1|season)
             , data =.)))
  }
  prediction_results
}


tidy_up <- function(prediction_variables, prepared_data_list, prediction_results, column_naming_prefix) {
  
  names(prediction_results) <- prediction_variables
  datasets <- names(prepared_data_list)
  
  for(i in seq_along(prediction_variables)) {
    
    for (j in seq_along(datasets)) {
      
      prediction_results[[i]][[datasets[[j]]]] <- data_frame(prediction_results[[i]][[datasets[[j]]]])
      names(prediction_results[[i]][[datasets[[j]]]]) <-   
        paste(column_naming_prefix,"_",prediction_variables[[i]], sep="")
      
    }
  }
  tidy_results <- prediction_results
  
  tidy_results
  
}



rebuild <- function(prepared_data_list, tidy_results) {
  
  rebuit_data_list <- prepared_data_list 
  
  for(i in seq_along(tidy_results)){
    
    rebuit_data_list <-  map2(rebuit_data_list, tidy_results[[i]], bind_cols)
    
  }
  rebuit_data_list 
}

remap_unique_ids <- function(rebuilt_data_list, max_no_unique_ids, unique_id_list) {
  
  threshold_test_results <-  test_threshholds(rebuilt_data_list, max_no_unique_ids)
  
  for(i in seq_along(rebuilt_data_list)) {
    
    if(threshold_test_results[i] == F) {
      
      names(rebuilt_data_list[[i]])[names(rebuilt_data_list[[i]]) %in% "unique_id" ] <- "unique_id_for_predictions"
      names(rebuilt_data_list[[i]])[names(rebuilt_data_list[[i]]) %in% unique_id_list[[i]]] <- "unique_id"
    }
    
  }
  
  remapped_data_list <- rebuilt_data_list
  
  remapped_data_list
}



summarise_over <- function(prediction_variables, remapped_data_list) {
  
  select_set <-  vector("character",length(prediction_variables))
  summary_data_list <- vector("list", length(remapped_data_list))
  
  for(i in seq_along(prediction_variables)) {
    select_set[i] <- paste("predicted_",prediction_variables[[i]], sep = "")
  }
  
  for(i in seq_along(remapped_data_list)) {
    summary_data_list[[i]] <-  remapped_data_list[[i]][c("unique_id",select_set)] %>% 
      ungroup %>% 
      group_by(unique_id) %>% 
      summarise_each(funs(mean))
  }
  names(summary_data_list) <- names(remapped_data_list)
  summary_data_list
  
}

predict_over_ <- function(prediction_variables, prepared_data_list, max_no_unique_ids, unique_id_list, column_naming_prefix = "predicted") {
  
  prediction_results <- cross_predict(prediction_variables, prepared_data_list)
  
  tidy_results <- tidy_up(prediction_variables, prepared_data_list, prediction_results, column_naming_prefix)
  
  rebuilt_data_list <- rebuild(prepared_data_list, tidy_results)
  
  remapped_data_list <- remap_unique_ids(rebuilt_data_list, max_no_unique_ids, unique_id_list)
  
  summary_data_list <- summarise_over(prediction_variables, remapped_data_list)  
  
  predicted_over_data_list <- summary_data_list
  
  predicted_over_data_list
}
