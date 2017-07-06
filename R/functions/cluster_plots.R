# plots
plot_tch_clusters <- function(clustered_data_list) {
  
  # tch
  clustered_data_list %>% 
    map(~ ggplot(data = .
                 ,aes(x = predicted_harvested_area, y = predicted_tch,
                      col = as.factor(clustered_predicted_tch))) +
          geom_point() + 
          
          labs(title = "Clusters of Tonnes of cane per hectare by harvested area", x = " Harvested area (ha)", 
               y = "Tonnes of cane per hectare") + 
          
          theme(plot.title = element_text(face = "bold", size = 16, colour = "black")) + 
          
          theme(axis.title= element_text(face = "bold", size = 14, colour = "black")) + 
          
          theme(axis.text = element_text(colour = " black", face = "bold", size = 12)) + 
          
          theme(legend.title = element_text(face = "bold", colour = "black", size = 12)) +
          
          scale_colour_discrete(name = "Cluster") +
          
          scale_y_continuous(breaks=seq(40,160,40), limits = c(40,160)) +
          
          geom_hline(yintercept=85, colour = "dark green", linetype = 2, size = 0.75)
        
    )
}

# ccs
plot_ccs_clusters <- function(clustered_data_list) {
  clustered_data_list %>% 
    map(~ ggplot(data = .
                 ,aes(x = predicted_harvested_area, y = predicted_ccs,
                      col = as.factor(clustered_predicted_ccs))) +
          geom_point() + 
          
          labs(title = "Clusters of CCS by harvested area", x = " Harvested area (ha)", 
               y = "CCS") + 
          
          theme(plot.title = element_text(face = "bold", size = 16, colour = "black")) + 
          
          theme(axis.title= element_text(face = "bold", size = 14, colour = "black")) + 
          
          theme(axis.text = element_text(colour = " black", face = "bold", size = 12)) + 
          
          theme(legend.title = element_text(face = "bold", colour = "black", size = 12)) +
          
          scale_colour_discrete(name = "Cluster") +
          
          scale_y_continuous(breaks=seq(9,16,1), limits = c(9,16))
    )
}

# tsh
plot_tsh_clusters <- function(clustered_data_list) {
  clustered_data_list %>% 
    map(~ ggplot(data = .
                 ,aes(x = predicted_harvested_area, y = predicted_tsh,
                      col = as.factor(clustered_predicted_tsh))) +
          geom_point()  + 
          
          labs(title = "Clusters of Tonnes of sugar per hectare by harvested area", x = " Harvested area (ha)", 
               y = "Tonnes of sugar per hectare") + 
          
          theme(plot.title = element_text(face = "bold", size = 16, colour = "black")) + 
          
          theme(axis.title= element_text(face = "bold", size = 14, colour = "black")) + 
          
          theme(axis.text = element_text(colour = " black", face = "bold", size = 12)) + 
          
          theme(legend.title = element_text(face = "bold", colour = "black", size = 12)) +
          
          scale_colour_discrete(name = "Cluster")  +
          
          scale_y_continuous(breaks=seq(5,17,2), limits = c(5,17))
    )
  
}