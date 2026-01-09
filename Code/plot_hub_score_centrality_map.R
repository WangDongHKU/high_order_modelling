# 定义函数
plot_hub_score_centrality_map <- function(g) {
  library(igraph)
  library(ggplot2)
  library(ggraph)
  library(maps)
  library(dplyr)
  library(sf)
  library(stringr)
  library(stringr)
  library(leiden)
  library(rnaturalearth)
  library(rnaturalearthdata)
  library(ggplot2)
  library(sf)
  library(dplyr)
  # 读取中国省份的地理数据
  # china_map <- st_read("E:/Dong/one_drive/OneDrive - The University Of Hong Kong/_HMRF_2021_HIGHER_ORDER_MODEL/china/cn.shp")
  # china_map <- china_map %>%
  #   mutate(NAME = str_replace(name, " .*", ""))
  province <-c('Beijing', 'Tianjin', 'Hebei', 'Shanxi', 'Inner Mongol', 'Liaoning', 'Jilin', 'Heilongjiang', 'Shanghai', 'Jiangsu', 'Zhejiang', 'Anhui', 'Fujian', 'Jiangxi', 'Shandong', 'Henan', 'Hubei', 'Hunan', 'Guangdong', 'Guangxi', 'Hainan', 'Chongqing', 'Sichuan', 'Guizhou', 'Yunnan', 'Xizang', 'Shaanxi', 'Gansu', 'Qinghai', 'Ningxia', 'Xinjiang')
  
  china_province_map_data <- ne_states(country = "China", returnclass = "sf")
  taiwan_map_data <- ne_countries(country = "taiwan")
  common_columns <- intersect(colnames(china_province_map_data), colnames(taiwan_map_data))
  china_province_map_data <- china_province_map_data[, common_columns]
  taiwan_map_data <- taiwan_map_data[, common_columns]
  china_map <- rbind(china_province_map_data, taiwan_map_data)
  
  # china_map <- china_map %>%
  #   mutate(name = str_replace(name, " .*", ""))
  province_data <- data.frame(name = province,value = hub_score(g,weights = E(g)$weight)$vector)
  
  # 确保数据合并正确
  china_map <- china_map %>%full_join(province_data, by = "name")
  # 绘制地图
  plot1 <- ggplot(data = china_map) +
    geom_sf(aes(fill = value)) +
    # scale_fill_gradient(low = "white", high = "#D55E00") +
    scale_fill_gradient2(low = "#4A7056", mid = "white",high = "#976666",midpoint = 0.25) +
  
    theme_void() +
    labs(fill = "Betweenness")+
    theme(
      legend.title = element_text(size = 5),  # 调整图例标题字体大小
      legend.text = element_text(size = 5),   # 调整图例文本字体大小
      plot.title = element_text(size = 5),    # 调整标题字体大小
      legend.position = c(0.9, 0.3)
    )
  return(plot1)
}

# 使用示例
# g <- make_graph(...) # 创建或读取你的图对象
# plot_degree_centrality_map(g, "path_to_your_shapefile/china_shapefile.shp")
