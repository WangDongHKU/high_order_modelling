linearfitting_mid1 <- function(Deff,dates_20_cases) {
  library(minpack.lm)  # More robust fitting than base nls
  library(propagate)
  library(minpack.lm)
  library(ggplot2)
  Deff 
  dates_20_cases 
  
  data <- data.frame(Deff, dates_20_cases)
  # 拟合线性模型
  # model <- lm(log(dates_20_cases) ~ Deff, data = data)
  # 
  # pred <- predict(model, interval = "confidence")
  # # 将预测值添加到数据框
  # data$fit <- pred[, "fit"]
  # data$lwr <- pred[, "lwr"]
  # data$upr <- pred[, "upr"]
  # 
  # ggplot(data, aes(x = Deff, y = dates_20_cases+1)) +
  #   geom_point() +
  #   geom_line(aes(y = exp(fit)), color = "#77AC30") +
  #   geom_ribbon(aes(ymin = exp(lwr), ymax = exp(upr)), alpha = 0.2, fill = "#77AC30") +
  #   labs(
  #     x = "Effective distance",
  #     y = "Estimated infection"
  #   ) +
  #   theme_classic() +scale_y_log10()+
  #   theme(
  #     axis.title.x = element_text(size = 12),
  #     axis.title.y = element_text(size = 12, margin = margin(r = -5)) # Adjust
  #   )

  
  
 ggplot(data, aes(x = Deff, y = dates_20_cases)) +
    geom_point(color = "black", size = 1, alpha = 0.8) +
    #geom_text_repel(aes(label = Province), size = 3, color = "black")+
    labs(x = "Effective distance", y = "Cumulative estimated infection") +
    theme_classic(base_size = 12) +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold"),
      panel.grid.minor = element_blank()
    ) + scale_x_log10()+scale_y_log10()+
    geom_smooth(method = "lm", formula = y ~ x, size=0.5,
                fill = "#77AC30", color = "#77AC30", se = TRUE,alpha = 0.2) +
    stat_cor(  # 添加Pearson相关系数和p值
      method = "pearson",
      label.x = 0.3, label.y = 4,  # 标签位置
      size = 4, color = "black",
      cor.coef.name = "r"
    )+
    theme(
      axis.text  = element_text(color = "black", size = 12),
      plot.margin = margin(10, 20, 10, 20)
    )

  
  
  
}