Node_betweenness_centrality_plot <- function(eigenvector_centrality_all, labels_y, max_year) {
  library(reshape2)
  library(dplyr)
  library(ggplot2)
  
  # Reshape the data
  data_long <- melt(eigenvector_centrality_all)
  colnames(data_long) <- c("Provinces", "Time", "Betweenness")
  data_long$Provinces <- factor(data_long$Provinces, levels = unique(data_long$Provinces), labels = labels_y)
  
  # Identify maximum values
  max_values <- data_long %>%
    group_by(Time) %>%
    slice(which.max(Betweenness)) %>%
    ungroup()
  
  # Create the plot
  p_eigen <- ggplot(data_long, aes(x = Time, y = Provinces, fill = Betweenness)) + 
    geom_tile(color = NA) +  
    scale_fill_gradient(low = NA, high =  "red") + 
    geom_point(data = max_values, aes(x = Time, y = Provinces), color = "#009E73", shape = 21, fill = "yellow", size = 1.1, stroke = 1) +  
    scale_x_continuous(breaks = seq(from = 0, to = max_year*365.25, by = 365.25), labels = seq(2020, 2020+max_year, 1)) +
    theme_classic() +labs(y = NULL, fill = legend)  +theme(text = element_text(size = 10),axis.text = element_text(size = 6)) 
  
  return(p_eigen)
}