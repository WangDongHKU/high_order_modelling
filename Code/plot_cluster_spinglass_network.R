plot_cluster_spinglass_network <- function(g, data_lst) {
  library(igraph)
  library(ggplot2)
  library(ggraph)
  library(maps)
  library(dplyr)
  library(leiden)
  library(rnaturalearth)
  library(rnaturalearthdata)
  set.seed(123)
  V(g)$lon <- c(116.4142,117.323,114.9042,112.2922,113.9448,122.6085,126.1923,127.7615,121.4491,119.455,120.0934,117.2264,117.9874,115.7221,118.1498,113.614,112.2707,111.7088,113.4244,108.7881,109.7453,107.874,102.7103,106.8748,101.487,88.0924,108.8701,104.2861,95.9956,106.1655,85.2401)
  V(g)$lat <- c(40.1824,39.3054,37.8957,37.5777,44.0935,41.2956,43.6661,47.862,31.202,32.9711,29.1832,31.8257,26.0789,27.614,36.3427,33.882,30.9756,27.6104,23.3417,23.8298,19.1959,30.0572,30.6171,26.8154,24.974,31.6927,35.1917,35.7518,35.7452,37.2692,41.1129)
  layout_matrix <- cbind(V(g)$lon, V(g)$lat)
  V(g)$weight <- data_lst
  E(g)$eweight <- E(g)$weight
  V(g)$province <- c('Beijing', 'Tianjin', 'Hebei', 'Shanxi', 'Inner Mongol', 'Liaoning', 'Jilin', 'Heilongjiang', 'Shanghai', 'Jiangsu', 'Zhejiang', 'Anhui', 'Fujian', 'Jiangxi', 'Shandong', 'Henan', 'Hubei', 'Hunan', 'Guangdong', 'Guangxi', 'Hainan', 'Chongqing', 'Sichuan', 'Guizhou', 'Yunnan', 'Xizang', 'Shaanxi', 'Gansu', 'Qinghai', 'Ningxia', 'Xinjiang')
  
  china_province_map_data <- ne_states(country = "China", returnclass = "sf")
  taiwan_map_data <- ne_countries(country = "taiwan")
  common_columns <- intersect(colnames(china_province_map_data), colnames(taiwan_map_data))
  china_province_map_data <- china_province_map_data[, common_columns]
  taiwan_map_data <- taiwan_map_data[, common_columns]
  merged_map_data <- rbind(china_province_map_data, taiwan_map_data)
  
  communities <- cluster_optimal(g,weights = E(g)$weight) 
  # communities <- cluster_fast_greedy(g,weights = E(g)$weight) 
  
    #communities <- cluster_spinglass(g,weights = E(g)$weight) 
  #communities <- cluster_louvain(g) 
  # communities <- cluster_infomap(g)
  V(g)$community <- communities$membership
 # V(g)$community <- match(communities$membership, unique(communities$membership))

  province_communities <- data.frame(
    province = V(g)$province,
    community = V(g)$community)
  
  province_main_community <- aggregate(community ~ province, data = province_communities, 
                                       FUN = function(x) as.numeric(names(sort(table(x), decreasing = TRUE)))[1])
  
  merged_map_data$community <- province_main_community$community[match(merged_map_data$name, province_main_community$province)]
  
  community_colors <- rainbow(length(unique(merged_map_data$community)))
  names(community_colors) <- unique(merged_map_data$community)
  # merged_map_data$community <- factor(merged_map_data$community, levels = sort(unique(merged_map_data$community), decreasing = TRUE))
  
  
  plot_combined <- ggraph(g, layout = layout_matrix) +
    geom_sf(data = merged_map_data, aes(fill = as.factor(community)), color = "black", alpha = 0.3) +
    scale_fill_manual(values = community_colors, name = "Communities") +
    geom_edge_link(aes(width = eweight, alpha = eweight, color = eweight), arrow = arrow(length = unit(1, 'mm'), type = "open")) +
    geom_node_point(aes(size = weight, color = as.factor(community))) +
    scale_size_continuous(name = "Infection No", range = c(0.1, 2.5)) +
    scale_edge_color_gradient(name = "Infection Move", low = "blue", high = "darkblue") +
    scale_edge_width_continuous(name = "Link Weights", range = c(0.025, 0.5)) +
    scale_edge_alpha_continuous(name = "Link Transparency", range = c(0.025, 1)) +
    facet_wrap(~ Month, ncol = 5, nrow = 4, scales = "fixed",
               labeller = labeller(Month = setNames(as.list(labelsmain), unique(data$Month)))) +
    theme_void() +
    theme(strip.background = element_rect(fill = "#F5F5F5", color = NA),
      legend.position = "right",  # 将图例放在右下角
      legend.text = element_text(size = 6),  # 调整图例文本的字体和字号
      legend.title = element_text(size = 6)  # 调整图例标题的字体和字号
    )   +
    guides(
      size = guide_legend(title= "Infection No",keywidth = unit(0.01, "cm"), keyheight = unit(0.1, "cm"), order = 1),#"none",
      edge_alpha = "none",#guide_legend(title= "Infection Move",keywidth = unit(0.01, "cm"), keyheight = unit(0.1, "cm"), order = 2),#
      edge_color=guide_legend(title= "Infection Move",keywidth = unit(0.01, "cm"), keyheight = unit(0.1, "cm"), order = 2),#"none",#
      edge_width="none",#guide_legend(title= "Infection Move",keywidth = unit(0.01, "cm"), keyheight = unit(0.1, "cm"), order = 3),#
      color ="none",# guide_legend(title= "Communities",keywidth = unit(0.1, "cm"), keyheight = unit(0.3, "cm")),#
      fill = guide_legend(title= "Communities",keywidth = unit(0.4, "cm"), keyheight = unit(0.3, "cm"), order = 3),#"none",#"none"#
    )
  return(plot_combined)
}
