initial_peorid <-  function(dailyincidence, Mobility,p_report) {
  # 创建一个向量来存储每个省达到200个病例的日期
  library(dplyr)
  dates_100_cases <- rep(NA, nrow(dailyincidence))
  date_index <- rep(0, nrow(dailyincidence))
   province_location <- c('Beijing', 'Tianjin', 'Hebei', 'Shanxi', 'Inner', 'Liaoning', 'Jilin', 'Heilongjiang', 'Shanghai', 'Jiangsu', 'Zhejiang', 'Anhui', 'Fujian', 'Jiangxi', 'Shandong', 'Henan', 'Hubei', 'Hunan', 'Guangdong', 'Guangxi', 'Hainan', 'Chongqing', 'Sichuan', 'Guizhou', 'Yunnan', 'Tibet', 'Shaanxi', 'Gansu', 'Qinghai', 'Ningxia', 'Xinjiang') %>% as.data.frame()
   
   number <- 31
   location <- 17
   
 # province_location <- c('Beijing', 'Hebei',  'Inner', 'Liaoning', 'Jilin', 'Heilongjiang', 'Shanghai', 'Jiangsu', 'Zhejiang', 'Anhui', 'Fujian', 'Jiangxi', 'Shandong', 'Henan', 'Hubei', 'Hunan', 'Guangdong', 'Guangxi',  'Chongqing', 'Sichuan',  'Yunnan',  'Shaanxi', 'Gansu','Xinjiang') %>% as.data.frame()
  # rownames(dailyincidence) <- province_location[,1]
  # location <- 15
  p_report11 <- apply(p_report, c(2,3), mean)
  # p_report11 <- 1
  dailyincidencerisk <- dailyincidence/p_report11
  
  
  # 遍历每个省
  for (i in 1:nrow(dailyincidence)) {
    # 计算累计病例数
    cumulative_cases<- cumsum(as.numeric(dailyincidence[i,1:700])) %>% t()
    # 找到累计病例数达到10的日期
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
      cumulative_mobility[i,j] <- sum(Mobility[1:700,i, j])
    }
  }
  source("network_effedistance.r")
  Deff <- network_effedistance(cumulative_mobility)
  #plot((Deff[17,]),log(date_index))# sigmoid function
  ###%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  selected_elements <- c(1:(location-1), (location+1):number)
  dates_20_cases <- rep(NA, nrow(dailyincidence))
  for (i in 1:nrow(dailyincidence)) {
    # 计算累计病例数
    dates_20_cases[i]<- sum(as.numeric(dailyincidencerisk[i,1:700])) %>% t()
  }
  dates_20_cases <- data.frame(Province = rownames(dailyincidencerisk), dates_20_cases = dates_20_cases)
  # plot((Deff[17,]),(date_index))# sigmoid function
  # plot(log(cumulative_mobility[17,selected_elements]),(dates_20_cases[selected_elements,2]))
  # plot((Deff[17,]),log(dates_20_cases[,2])) 
  
  source("linearfitting_initial.r")
  p1 <- linearfitting_initial((1+cumulative_mobility[location,selected_elements]),(1+dates_20_cases[selected_elements,2]))
  source("linearfitting_initial1.r")
  p2 <- linearfitting_initial1(Deff[location,selected_elements],(1+dates_20_cases[selected_elements,2]))
  
  source("nonlinearfitting_initial.r")
  # nonlinearfitting((Deff[17,]),log(date_index))    Deff <- Deff[location,]  dates_20_cases <- dates_20_cases[,2] 
  
  # Deff_no_na <- na.omit(Deff[17,])
  # log_date_index_no_na <- na.omit(log(date_index))
  
  selected_elements <- which(date_index != "NA")
  date_index_no_na <- date_index[selected_elements]
  Deff_no_na <- Deff[location,selected_elements]
  
  
  p3 <- nonlinearfitting_initial(Deff_no_na,date_index_no_na)
  p3 <-  p3 + theme_classic()
  
  # p3 <- nonlinearfitting1((Deff[17,]),log(date_index))   (plot.margin = unit(c(-0,-0, -0, -0), "cm"))  
  library(magick)
  library(pdftools)


  source("plot_movement_hub1.r")
  # p4 <- plot_movement_hub1()
  # p4 <- p4 + theme(plot.margin = unit(c(-0,-0, -0, 0), "cm"))  
  
  library(cowplot)
  
  # 假设 p1, p2, p3 是你的图形对象
  figure <- plot_grid(
    plotlist = list(p1, p2, p3),
    labels = c("b","c","d"),
    label_size = 20,
    hjust = 0.01,
    ncol = 3,
    align = "hv",
    rel_widths = c(1, 1,  1), 
    rel_heights = c(1, 1,1)
  )

  
  ggsave("figure_1Hubei.pdf", figure, width = 12, height = 3)
  
  
  movement <- tibble(
    Source = rep('Hubei', number),
    Target = province_location[,1],
    Weight = dates_20_cases[, 2]
  )
  write.csv(movement, "movement_1.csv", row.names = FALSE)
  
  
  return(figure)
  
}