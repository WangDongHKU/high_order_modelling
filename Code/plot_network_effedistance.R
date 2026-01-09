
plot_network_effedistance <- function(g) {
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
  adj_matrix <- as_adjacency_matrix(g, attr="weight", sparse=FALSE)
  # 计算每个节点的总流量
  total_flux <- rowSums(adj_matrix)
  # 计算流量比例矩阵 P
  P_matrix <- adj_matrix / total_flux
  
  n <- nrow(P_matrix)
  eff_dist <- matrix(0, n, n)
  for (i in 1:n) {
    for (j in 1:n) {
      if (i != j && P_matrix[i, j] > 0) {
        eff_dist[i, j] <-  log(P_matrix[i, j])
      } else {
        eff_dist[i, j] <- Inf  # 无连接时设为无穷大
      }
    }
  }
  adj_matrix <- eff_dist %>% t()%>% melt()
  adj_matrix <- adj_matrix %>% rename(To = Var1, From = Var2)
  adj_matrix$From <- factor(adj_matrix$From, levels = 1:length(labels_y), labels = labels_y)
  adj_matrix$To <- factor(adj_matrix$To, levels = 1:length(labels_y), labels = labels_y)
  ggplot(data = adj_matrix, aes(x=To, y=From, fill=value)) +
    geom_tile() +
    scale_fill_gradient2(low ="lightblue" , high = "darkblue", na.value = NA,
                         guide = "colourbar",name="Effective distance") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                     size = 5, hjust = 1),
          axis.text.y = element_text(size = 5))
  
}

# 使用示例


# 假设 g 是你的图对象
# plot_network_heatmap(g, labels_y)
