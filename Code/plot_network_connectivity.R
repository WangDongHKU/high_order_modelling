plot_network_connectivity <- function(g) {
  library(igraph)
  library(gplots)
  library(ggplot2)
  library(ggraph)
  library(maps)
  library(dplyr)
  labels_y <- c('Beijing', 'Tianjin', 'Hebei', 'Shanxi', 'Inner Mongolia', 'Liaoning', 
                'Jilin', 'Heilongjiang', 'Shanghai', 'Jiangsu', 'Zhejiang', 'Anhui', 
                'Fujian', 'Jiangxi', 'Shandong', 'Henan', 'Hubei', 'Hunan', 'Guangdong', 
                'Guangxi', 'Hainan', 'Chongqing', 'Sichuan', 'Guizhou', 'Yunnan', 'Tibet', 
                'Shaanxi', 'Gansu', 'Qinghai', 'Ningxia', 'Xinjiang')
  E(g)$attributes <- similarity(g, method="jaccard") %>% as.vector()
  E(g)$importance <- E(g)$attributes
  adj_matrix <- as_adjacency_matrix(g, attr="attributes", sparse=FALSE) %>% t() %>% melt()
  adj_matrix <- adj_matrix %>% rename(From = Var1, To = Var2)
  adj_matrix$From <- factor(adj_matrix$From, levels = 1:length(labels_y), labels = labels_y)
  adj_matrix$To <- factor(adj_matrix$To, levels = 1:length(labels_y), labels = labels_y)
  
  ggplot(data = adj_matrix, aes(x=To, y=From, fill=value)) +
    geom_tile() +
    scale_fill_gradient2(low = "lightblue", high = "darkblue", space = "Lab", 
                         name="Betweenness") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                     size = 12, hjust = 1))
}
