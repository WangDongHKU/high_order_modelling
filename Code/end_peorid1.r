end_peorid1 <-  function(dailyincidence, Mobility,p_report) {
  # 创建一个向量来存储每个省达到200个病例的日期
  dates_100_cases <- rep(0, nrow(dailyincidence))
  date_index <- rep(0, nrow(dailyincidence))
  location <- c(1)
  
  province_location <- c('Beijing', 'Tianjin', 'Hebei', 'Shanxi', 'Inner', 'Liaoning', 'Jilin', 'Heilongjiang', 'Shanghai', 'Jiangsu', 'Zhejiang', 'Anhui', 'Fujian', 'Jiangxi', 'Shandong', 'Henan', 'Hubei', 'Hunan', 'Guangdong', 'Guangxi', 'Hainan', 'Chongqing', 'Sichuan', 'Guizhou', 'Yunnan', 'Tibet', 'Shaanxi', 'Gansu', 'Qinghai', 'Ningxia', 'Xinjiang') %>% as.data.frame()
  # 遍历每个省
  
  # p_report11 <- apply(p_report, c(2,3), mean)
  
  dailyincidencerisk <- dailyincidence/1
  
  for (i in 1:nrow(dailyincidence)) {
    # 计算累计病例数
    cumulative_cases<- cumsum(as.numeric(dailyincidence[i,1034:1155])) %>% t()
    # 找到累计病例数达到100的日期
    date_index[i] <- which(cumulative_cases >= 100)[1]
    if (!is.na(date_index[i])) {
      dates_100_cases[i] <- colnames(dailyincidence)[date_index[i]]
    }
  }
  # 将结果转换为数据框
  result_date <- data.frame(Province = rownames(dailyincidence), Date_100_Cases = dates_100_cases)
  cumulative_mobility <- array(0, dim = c(31, 31))
  # 遍历每个省份和每个目标省份
  for (i in 1:31) {
    for (j in 1:31) {
      cumulative_mobility[i,j] <- sum(Mobility[1000:1155,i, j])
    }
  }
  # for (i in 1:31) {
  # cumulative_mobility[i,i] <- sum(cumulative_mobility[i,])
  # }
  source("network_effedistance.r")
  Deff <- network_effedistance(cumulative_mobility)
  # plot((Deff[19,]),log(date_index))# sigmoid function
  ###%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  # selected_elements <- c(1:(location), (location+1):31)
  dates_20_cases <- rep(NA, nrow(dailyincidence))
  for (i in 1:nrow(dailyincidence)) {
    # 计算累计病例数
    dates_20_cases[i]<- sum(as.numeric(dailyincidencerisk[i,1000:1155])) %>% t()
  }
  dates_20_cases <- data.frame(Province = rownames(dailyincidencerisk), dates_20_cases = dates_20_cases)
  # plot((Deff[19,]),(date_index))# sigmoid function
  # plot(log(cumulative_mobility[19,selected_elements]),(dates_20_cases[selected_elements,2]))
  # plot((Deff[19,]),log(dates_20_cases[,2])) 
  
  source("linearfitting_extend_end.r")
  # selected_elements <- which(dates_20_cases[,2] != 0)
  selected_elements <- c(1:(location-1), (location+1):31)
  filtered_dates <- (dates_20_cases[selected_elements, 2])
  filtered_mobility <- (cumulative_mobility[2, selected_elements])
  # 进行线性拟合
  p1 <- linearfitting_extend_end(1+filtered_mobility, 1+filtered_dates) + labs(
    y = "Cumulative reported infection"
  ) 
  
  source("linearfitting_extend_end1.r")
  # p2 <- linearfitting1(Deff[16,selected_elements],log(1+dates_20_cases[selected_elements,2]))
  #selected_elements <- which(dates_20_cases[,2] != 0)
  #filtered_dates <- (1+dates_20_cases[selected_elements, 2])
  filtered_deff <- Deff[location,selected_elements]
  
  # 进行线性拟合
  p2 <- linearfitting_extend_end1(filtered_deff, 1+filtered_dates)+ labs(
    y = "Cumulative reported infection"
  ) 
  
  
  source("nonlinearfitting.r")
  source("nonlinearfitting2.r")
  # nonlinearfitting((Deff[19,]),log(date_index))
  # Deff_no_na <- na.omit(Deff[2,])[c(1:31)]
  # log_date_index_no_na <- na.omit(log(date_index))
  
  selected_elements <- which(date_index != "NA")
  date_index_no_na <- (date_index[selected_elements])
  Deff_no_na <- Deff[selected_elements,location]
  
  p3 <- nonlinearfitting2(Deff_no_na,date_index_no_na)
  library(magick)
  library(pdftools)
  # pdf_image <- image_read("E:/Dong/one_drive/OneDrive - The University Of Hong Kong/_HMRF_2021_HIGHER_ORDER_MODEL/movement_Shanghai_2.tif")
  # 
  # # 将 PDF 图片转换为 ggplot 对象
  # p4 <- ggdraw() +
  #   draw_image(pdf_image, x = -0.2, y = -0.5, width = 1.2, height = 2) +
  #   theme_void()
  source("plot_movement_hub3.r")
  p4 <- plot_movement_hub3()
  p4 <- p4 + theme_classic()
  
  
  # p4 <- ggdraw() +
  #   draw_image(p4, x = -0.2, y = -0.5, width = 1.2, height = 2) +
  #   theme_void()(plot.margin = unit(c(-0,-0, -0, -0), "cm"))  
  
  library(cowplot)
  
  # 假设 p1, p2, p3 是你的图形对象
  figure <- plot_grid(
    plotlist = list(p1, p2),
    ncol = 1,nrow = 2,
    labels = c("c","f"),
    label_size = 20,
    hjust = 0.01,
    align = "v", # 水平和垂直对齐
    rel_heights = c(1, 1) # 保持相对高度不变
  )
  
  movement <- tibble(
    Source = rep('Shanghai', 31),
    Target = province_location[,1],
    Weight = dates_20_cases[, 2]
  )
  write.csv(movement, "movement_2.csv", row.names = FALSE)
  
  ggsave("figure_2Shanghai.pdf", figure, width = 12, height = 3)
  return(figure)
  
}