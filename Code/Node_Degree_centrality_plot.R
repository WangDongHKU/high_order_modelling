Node_Degree_centrality_plot <- function(eigenvector_centrality_all, labels_y, max_year,legend) {
  library(reshape2)
  library(dplyr)
  library(ggplot2)
  
  # Reshape the data
  data_long <- melt(eigenvector_centrality_all)
  colnames(data_long) <- c("Provinces", "Time", "Degree")
  data_long$Provinces <- factor(data_long$Provinces, levels = unique(data_long$Provinces), labels = labels_y)
  
  # Identify maximum values
  max_values <- data_long %>%
    group_by(Time) %>%
    slice(which.max(Degree)) %>%
    ungroup()
  
  
  
  p_eigen <- ggplot(data_long, aes(x = Time, y = Provinces, fill = sqrt(Degree + 1))) + 
    geom_tile(color = NA) +  
    scale_fill_gradient2(low = NA, high =  "red") + 
    # geom_line(data = max_values, aes(x = Time, y = Provinces), color = "#009E73", shape = 19, fill = "yellow", size = 0.1, stroke = 1) + 
    geom_point(data = max_values, aes(x = Time, y = Provinces), color = "#009E73", shape = 21, fill = "yellow", size = 1, stroke = 1) + 
    scale_x_continuous(breaks = seq(from = 0, to = max_year*365.25, by = 365.25), labels = seq(2020, 2020+max_year, 1)) +
    labs(y = "Provinces", fill = expression(sqrt("Degree(out)"))) + 
    theme_classic() +theme(text = element_text(size = 10),axis.text = element_text(size = 6)) 
  return(p_eigen)
  
  
  
  
  # p_eigen <- ggplot(data_long, aes(x = Time, y = Provinces, fill = sqrt(Degree + 1))) + 
  #   geom_tile(color = NA) +  
  #   scale_fill_gradient2(low = NA, high = "red") + 
  #   geom_point(data = max_values, aes(x = Time, y = Provinces), color = "#009E73", shape = 21, fill = "yellow", size = 1, stroke = 0.3) + 
  #   # geom_line(data = max_values, aes(x = Time, y = Provinces), color = "#009E73", size = 1) +
  #   scale_x_continuous(breaks = seq(from = 0, to = max_year * 365.25, by = 365.25), labels = seq(2020, 2020 + max_year, 1)) +
  #   labs(y = "Provinces", fill = expression(sqrt("Degree(out)"))) +  
  #   labs(x = NULL) +
  #   theme_bw() +
  #   theme(
  #     text = element_text(size = 10),  # 设置所有文本的默认大小为10
  #     axis.text = element_text(size = 4.5)  # 设置坐标轴刻度标签的大小为4.5
  #   )+ coord_cartesian(xlim = c(40, 1115))
  # 
  # return(p_eigen)
  
  
  # Create the plot

  
  
  
  
}