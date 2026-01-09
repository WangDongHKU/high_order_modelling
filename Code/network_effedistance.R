network_effedistance <- function(adj_matrix) {
#adj_matrix <- as_adjacency_matrix(g, attr="weight", sparse=FALSE)
# 计算每个节点的总流量
total_flux <- rowSums(adj_matrix)
# 计算流量比例矩阵 P
P_matrix <- adj_matrix / total_flux

n <- nrow(P_matrix)
eff_dist <- matrix(0, n, n)
for (i in 1:n) {
  for (j in 1:n) {
    if (i != j && P_matrix[i, j] > 0) {
      eff_dist[i, j] <-  1-log(P_matrix[i, j])
    } else {
      eff_dist[i, j] <- Inf 
    }
  }
  eff_dist[i, i] <- 0 
}

return(eff_dist)

# g <- graph_from_adjacency_matrix(eff_dist %>% t(), mode = "directed", weighted = TRUE)
# # 计算最短路径
# shortest_paths_matrix <- distances(g, mode = "out")
# return(shortest_paths_matrix)

 
}