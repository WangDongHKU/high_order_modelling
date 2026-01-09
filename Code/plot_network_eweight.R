plot_network_eweight <- function(g) {
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
  E(g)$eweight <- E(g)$weight
  E(g)$importance <- E(g)$eweight
  adj_matrix <- as_adjacency_matrix(g, attr="eweight", sparse=FALSE) %>% t() %>% melt()
  adj_matrix <- adj_matrix %>% rename(Sink = Var1, Source = Var2)
  adj_matrix$Source <- factor(adj_matrix$Source, levels = 1:length(labels_y), labels = labels_y)
  adj_matrix$Sink <- factor(adj_matrix$Sink, levels = 1:length(labels_y), labels = labels_y)
  
  p <- ggplot(data = adj_matrix, aes(x=Sink, y=Source, fill=sqrt(value))) +
    geom_tile() +
    scale_fill_gradient2(low = "#ACD4D6", mid = "white",high = "darkblue",midpoint =1,name=expression(sqrt(Weights)),na.value = NA) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90, vjust = 1, 
                                     size = 5, hjust = 1),
          axis.text.y = element_text(size = 5))
  return(p)
}
