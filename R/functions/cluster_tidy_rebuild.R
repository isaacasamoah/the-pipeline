scree_this <- function(prepared_cluster_data) {
  
  wss <- (nrow(prepared_cluster_data)-1) * sum(apply(prepared_cluster_data,2,var))
  
  for (i in 2:15) wss[i] <- sum(kmeans(prepared_cluster_data,centers=i)$withinss)
  
  plot(1:15, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")
  
}

cluster_this <- function(clustering_variables, prepared_cluster_data_list, cluster_no) {
  
  set.seed(1)
  
  clusters_list <- vector("list", length(clustering_variables))
  
  for(i in seq_along(clustering_variables)) {
    clusters_list[[i]] <- prepared_cluster_data_list[[i]] %>% 
      map(kmeans,cluster_no) %>% 
      map("cluster")
  }
  names(clusters_list) <- clustering_variables
  clusters_list
}


cluster_tidy_rebuild <-  function(clustering_variables, prepared_cluster_data_list, cluster_no,
                                  input_data_list,column_naming_prefix) {
  
  clusters_list <-  cluster_this(clustering_variables, prepared_cluster_data_list, cluster_no)
  
  tidy_clusters_list <- tidy_up(clustering_variables, input_data_list, clusters_list, column_naming_prefix)
  
  rebuit_data_list <- rebuild(input_data_list,tidy_clusters_list)
  
  clustered_data_list <- rebuit_data_list
  
  clustered_data_list
  
}