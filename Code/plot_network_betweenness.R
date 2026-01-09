
plot_network_betweenness <- function(g) {
  library(igraph)
  library(gplots)
  library(ggplot2)
  library(ggraph)
  library(maps)
  library(dplyr)
  labels_y <- c('Beijing', 'Tianjin', 'Hebei', 'Shanxi', 'InnerMongolia', 'Liaoning', 
                'Jilin', 'Heilongjiang', 'Shanghai', 'Jiangsu', 'Zhejiang', 'Anhui', 
                'Fujian', 'Jiangxi', 'Shandong', 'Henan', 'Hubei', 'Hunan', 'Guangdong', 
                'Guangxi', 'Hainan', 'Chongqing', 'Sichuan', 'Guizhou', 'Yunnan', 'Tibet', 
                'Shaanxi', 'Gansu', 'Qinghai', 'Ningxia', 'Xinjiang')
  E(g)$betweenness <- edge.betweenness(g,directed = TRUE,weights =1/(10^(-10)+E(g)$weight))
  E(g)$importance <- E(g)$betweenness
  adj_matrix <- as_adjacency_matrix(g, attr="betweenness", sparse=FALSE) %>% melt()
  adj_matrix <- adj_matrix %>% rename(Sink = Var1, Source = Var2)
  adj_matrix$Source <- factor(adj_matrix$Source, levels = 1:length(labels_y), labels = labels_y)
  adj_matrix$Sink <- factor(adj_matrix$Sink, levels = 1:length(labels_y), labels = labels_y)
  
  ggplot(data = adj_matrix, aes(x=Sink, y=Source, fill=(value))) +
    geom_tile() +
    scale_fill_gradient2(low = "white", high = "#D55E00", space = "Lab", 
                         name="Betweenness") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90, vjust = 1, 
                                     size = 5, hjust = 1),
          axis.text.y = element_text(size = 5))
}

# 使用示例


# 假设 g 是你的图对象
# plot_network_heatmap(g, labels_y)
