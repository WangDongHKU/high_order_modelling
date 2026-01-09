plot_movement_hub3 <- function( ){
library(igraph)
library(ggraph)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)

# 读取CSV文件并创建图对象
edges <- read.csv("E:\\Dong\\one_drive\\OneDrive - The University Of Hong Kong\\_HMRF_2021_HIGHER_ORDER_MODEL\\movement_3.csv")
edges <- edges[-19, ]
g <- graph_from_data_frame(d=edges, directed=TRUE)
# V(g)$name <- c('Shanghai','Beijing', 'Tianjin', 'Hebei', 'Shanxi', 'Inner Mongol', 'Liaoning', 'Jilin', 'Heilongjiang',  'Jiangsu', 'Zhejiang', 'Anhui', 'Fujian', 'Jiangxi', 'Shandong', 'Henan', 'Hubei', 'Hunan', 'Guangdong', 'Guangxi', 'Hainan', 'Chongqing', 'Sichuan', 'Guizhou', 'Yunnan', 'Xizang', 'Shaanxi', 'Gansu', 'Qinghai', 'Ningxia', 'Xinjiang')

E(g)$eweight <- edges$Weight

# 设置节点的经纬度
V(g)$lon <- c(113.4244,116.4142,117.323,114.9042,112.2922,113.9448,122.6085,126.1923,127.7615,121.4491,119.455,120.0934,117.2264,117.9874,115.7221,118.1498,113.614,112.2707,111.7088,108.7881,109.7453,107.874,102.7103,106.8748,101.487,88.0924,108.8701,104.2861,95.9956,106.1655,85.2401)
V(g)$lat <- c(23.3417,40.1824,39.3054,37.8957,37.5777,44.0935,41.2956,43.6661,47.862,31.202,32.9711,29.1832,31.8257,26.0789,27.614,36.3427,33.882,30.9756,27.6104,23.8298,19.1959,30.0572,30.6171,26.8154,24.974,31.6927,35.1917,35.7518,35.7452,37.2692,41.1129)
layout_matrix <- cbind(V(g)$lon, V(g)$lat)

# 设置节点的省份

# 获取中国和台湾的地图数据
china_province_map_data <- ne_states(country = "China", returnclass = "sf")
taiwan_map_data <- ne_countries(country = "Taiwan", returnclass = "sf")
common_columns <- intersect(colnames(china_province_map_data), colnames(taiwan_map_data))
china_province_map_data <- china_province_map_data[, common_columns]
taiwan_map_data <- taiwan_map_data[, common_columns]
merged_map_data <- rbind(china_province_map_data, taiwan_map_data)



V(g)$weighted_indegree <- strength(g, mode = "in", weights = E(g)$eweight)


# 创建一个唯一的颜色向量

node_colors <- rainbow(31)

# 将颜色分配给每个节点
V(g)$color <- node_colors
# 获取每条边的目标节点
edge_targets <- ends(g, E(g), names = FALSE)[, 2]
# 将边的颜色设置为目标节点（入度节点）的颜色
E(g)$color <- V(g)$color[edge_targets]

# 可视化
plot1  <-  ggraph(g, layout = layout_matrix) +
  geom_sf(data = merged_map_data, color = "black", fill = "white", alpha = 0.0) +
  geom_edge_link(aes(width = eweight, alpha = 0.5, color = color), arrow = arrow(length = unit(2, 'mm'), type = "open")) +
  geom_node_point(aes(color = node_colors, size = weighted_indegree)) +  # 确保这里的 size 映射到 weighted_indegree
  scale_color_manual(values = node_colors) +
  scale_size_continuous(range = c(0.5, 2)) +  # 调整节点大小范围
  scale_edge_width_continuous(range = c(0.25, 0.5)) +  # 调整边大小范围
  theme_void() +
  guides(color = "none", size = "none", alpha = "none", width = "none", edge_color = "none", edge_width = "none", edge_alpha = "none")

return(plot1)
}