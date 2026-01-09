mid_peorid <-  function(dailyincidence, Mobility,p_report) {
  # 创建一个向量来存储每个省达到200个病例的日期
  dates_100_cases <- rep(0, nrow(dailyincidence))
  date_index <- rep(0, nrow(dailyincidence))
  province_location <- c('Beijing', 'Tianjin', 'Hebei', 'Shanxi', 'Inner', 'Liaoning', 'Jilin', 'Heilongjiang', 'Shanghai', 'Jiangsu', 'Zhejiang', 'Anhui', 'Fujian', 'Jiangxi', 'Shandong', 'Henan', 'Hubei', 'Hunan', 'Guangdong', 'Guangxi', 'Hainan', 'Chongqing', 'Sichuan', 'Guizhou', 'Yunnan', 'Tibet', 'Shaanxi', 'Gansu', 'Qinghai', 'Ningxia', 'Xinjiang') %>% as.data.frame()
  number <- 31
  location <- 9 # 16
  
  # province_location <- c('Beijing', 'Hebei',  'Inner', 'Liaoning', 'Jilin', 'Heilongjiang', 'Shanghai', 'Jiangsu', 'Zhejiang', 'Anhui', 'Fujian', 'Jiangxi', 'Shandong', 'Henan', 'Hubei', 'Hunan', 'Guangdong', 'Guangxi',  'Chongqing', 'Sichuan',  'Yunnan',  'Shaanxi', 'Gansu','Xinjiang') %>% as.data.frame()
  # number <- 24
  # location <- 14 # 16
  p_report11 <- apply(p_report, c(2,3), mean)
  #p_report11 <- 1
  #dailyincidence <- dailyincidence
  
  dailyincidencerisk <- dailyincidence/p_report11
  
  for (i in 1:nrow(dailyincidence)) {
    # 计算累计病例数
    cumulative_cases<- cumsum(as.numeric(dailyincidence[i,700:1000])) %>% t()
    # 找到累计病例数达到100的日期
    date_index[i] <- which(cumulative_cases >= 100)[1]
    if (!is.na(date_index[i])) {
      dates_100_cases[i] <- colnames(dailyincidence)[date_index[i]]
    }
  }
  # 将结果转换为数据框
  result_date <- data.frame(Province = rownames(dailyincidence), Date_100_Cases = dates_100_cases)
  cumulative_mobility <- array(0, dim = c(number, number))
  # 遍历每个省份和每个目标省份
  
  for (i in 1:number) {
    for (j in 1:number) {
      # 计算前60天的累计数值
      cumulative_mobility[i,j] <- sum(Mobility[700:1000,i, j])
    }
  }
  
  source("network_effedistance.r")
  Deff <- network_effedistance(cumulative_mobility)

  ###%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  dates_20_cases <- rep(NA, nrow(dailyincidence))
  for (i in 1:nrow(dailyincidence)) {
    # 计算累计病例数
    dates_20_cases[i]<- sum(as.numeric(dailyincidencerisk[i,700:1000])) %>% t()
  }
  dates_20_cases <- data.frame(Province = rownames(dailyincidencerisk), dates_20_cases = dates_20_cases)
  # plot((Deff[19,]),(date_index))# sigmoid function
  # plot(log(cumulative_mobility[19,selected_elements]),(dates_20_cases[selected_elements,2]))
  # plot((Deff[19,]),log(dates_20_cases[,2])) 
  
  source("linearfitting_mid.r")
  selected_elements <- c(1:(location-1), (location+1):number)
  
  # selected_elements <- which(dates_20_cases[,2] != 0)
  filtered_dates <-  dates_20_cases[selected_elements, 2]
  filtered_mobility <- (cumulative_mobility[location, selected_elements])
  filtered_deff <- Deff[location,selected_elements]
  # 进行线性拟合
  p1 <- linearfitting_mid(filtered_mobility, (1+filtered_dates))
  
  source("linearfitting_mid1.r")
  filtered_deff <- Deff[location,selected_elements]  
  # 进行线性拟合
  p2 <- linearfitting_mid1(filtered_deff, 1+filtered_dates)
  
  
  source("nonlinearfitting_mid.r")
  #selected_elements <- which(date_index != "NA")
  date_index_no_na <- date_index[selected_elements]
  Deff_no_na <- Deff[location,selected_elements]
  
  # Deff_no_na <- na.omit(Deff[16,])
  # log_date_index_no_na <- na.omit(log(date_index))
  
  p3 <- nonlinearfitting_mid(Deff_no_na,date_index_no_na)
  library(magick)
  library(pdftools)
  # pdf_image <- image_read("E:/Dong/one_drive/OneDrive - The University Of Hong Kong/_HMRF_2021_HIGHER_ORDER_MODEL/movement_Shanghai_2.tif")
  # 
  # # 将 PDF 图片转换为 ggplot 对象
  # p4 <- ggdraw() +
  #   draw_image(pdf_image, x = -0.2, y = -0.5, width = 1.2, height = 2) +
  #   theme_void()
  # source("plot_movement_hub2.r")
  # p4 <- plot_movement_hub2()
  # p4 <- p4 + theme_classic()  
  
  
  # p4 <- ggdraw() +
  #   draw_image(p4, x = -0.2, y = -0.5, width = 1.2, height = 2) +
  #   theme_void()(plot.margin = unit(c(-0,-0, -0, -0), "cm"))
  
  library(cowplot)
  
  # 假设 p1, p2, p3 是你的图形对象
  figure <- plot_grid(
    plotlist = list(p1, p2, p3),
    ncol = 3,
    labels = c("f","g","h"),
    label_size = 20,
    hjust = 0.01,
    align = "hv", # 水平和垂直对齐
    rel_widths = c(1, 1, 1), # 增加 p4 的相对宽度
    rel_heights = c(1, 1,1) # 保持相对高度不变
  )
  
  movement <- tibble(
    Source = rep('Shanghai', number),
    Target = province_location[,1],
    Weight = dates_20_cases[, 2]
  )
  write.csv(movement, "movement_2.csv", row.names = FALSE)
  
  ggsave("figure_2Shanghai.pdf", figure, width = 12, height = 3)
  return(figure)
  
}