plot_move_infection_heatmap <- function(move_infection, time_s, time_e) {
  library(ggplot2)
  library(tidyverse)
  library(ggpubr)
  time_s <- as.integer(time_s)
  time_e <- as.integer(time_e)
  
  if (time_s < 1 || time_e > 1155 || time_s > time_e) {
    stop("time_s 和 time_e 的范围不正确")
  }
  
  # 计算前30天的移动感染总和
  labels_y <- c('Beijing', 'Tianjin', 'Hebei', 'Shanxi', 'Inner Mongolia', 'Liaoning', 'Jilin', 'Heilongjiang', 'Shanghai', 'Jiangsu', 'Zhejiang', 'Anhui', 'Fujian', 'Jiangxi', 'Shandong', 'Henan', 'Hubei', 'Hunan', 'Guangdong', 'Guangxi', 'Hainan', 'Chongqing', 'Sichuan', 'Guizhou', 'Yunnan', 'Tibet', 'Shaanxi', 'Gansu', 'Qinghai', 'Ningxia', 'Xinjiang')
  sum_move_infection1 <- apply(move_infection, c(1, 2), function(x) sum(x[time_s:time_e]))
  
  # 创建数据框
  data_frame1 <- data.frame(country1 = rep(1:31, each = 31),
                            country2 = rep(1:31, 31),
                            time_period = 1,
                            value = as.vector(sum_move_infection1))
  
  # 合并数据框
  move_infection_data <- rbind(data_frame1)
  
  # 设置因子水平和标签
  move_infection_data$country1 <- factor(move_infection_data$country1, levels = 1:31, labels = labels_y)
  move_infection_data$country2 <- factor(move_infection_data$country2, levels = 1:31, labels = labels_y)
  
  # 绘制热图
  heatmap_plot <- ggplot(move_infection_data, aes(x = country1, y = country2, fill = value)) +
    geom_tile() +
    scale_fill_gradient(low = "white", high = "red") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    labs(x = "Provinces", y = "Provinces", fill = "Movement")
  
  return(heatmap_plot)
}

# 使用示例
# 假设 move_infection 是你的移动感染数据矩阵，labels_y 是省份标签
# heatmap_plot1 <- plot_move_infection_heatmap(move_infection, labels_y)
# print(heatmap_plot1)
