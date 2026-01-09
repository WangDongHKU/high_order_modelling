nonlinearfitting1 <- function(Deff_no_na, date_index_no_na) {
  library(robustbase)
  library(minpack.lm)
  library(ggplot2)
  
  # 1. Data Preparation
  data <- data.frame(Deff = Deff_no_na, y = log(date_index_no_na)) |> 
    na.omit()  # Remove NA values

  # 2. Proper Exponential Model (not sigmoid)
  library(quantreg)
  
  # Fit models for τ = 0.1, 0.5, 0.9
  # First fit τ=0.2
  model_q10 <- nlrq(
    y ~ a * exp(b * Deff) + c * exp(d * Deff),
    data = data,
    start = list(a = 3.426, b = 0.036, c = 0.0131, d = 3.232),
    tau = 0.2
  )
  
  model_q50 <- nlrq(  # Your existing median model
    y ~ a * exp(b * Deff) + c * exp(d * Deff),
    data = data,
    start = list(a = 3.426, b = 0.036, c = 0.0131, d = 3.232),
    tau = 0.5
  )
  
  model_q90 <- nlrq(
    y ~ a * exp(b * Deff) + c * exp(d * Deff),
    data = data,
    start = list(a = 3.426, b = 0.036, c = 0.0131, d = 3.232), 
    tau = 0.8
  )
  
  
  newdata <- data.frame(Deff = seq(min(data$Deff), max(data$Deff), length.out = 100))
  
  # Predictions for τ = 0.1, 0.5, 0.9
  newdata$y_q10 <- predict(model_q10, newdata = newdata)
  newdata$y_q50 <- predict(model_q50, newdata = newdata)
  newdata$y_q90 <- predict(model_q90, newdata = newdata)
  
  
  library(ggplot2)
  
  # Assuming you've already:
  # 1. Created models (model_q10, model_q50, model_q90)
  # 2. Generated newdata with predictions
  
  ggplot(data, aes(x = Deff)) +  # Only x here, y is in individual geoms
    geom_point(aes(y = y), alpha = 1) +
    geom_line(data = newdata, aes(y = y_q50), color = "#D95319") +
    geom_ribbon(
      data = newdata,
      aes(ymin = y_q10, ymax = y_q90),
      fill = "#D95319", alpha = 0.2
    ) +scale_y_log10()+
    labs(x = "Effective distance", y = "Timing (log)") +
    theme_classic()
  
  
  
  ggplot(data, aes(x = Deff, y = y)) +
    geom_point(color = "black", size = 1, alpha = 0.8) +
    labs(x = "Effective distance", y = "Timing (log)") +
    theme_classic(base_size = 12) +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold"),
      panel.grid.minor = element_blank()
    ) + scale_x_log10()+scale_y_log10()+
    geom_smooth(method = "lm", formula = y ~ exp(x), size=0.5,
                fill = "#D95319", color = "#D95319", se = TRUE,alpha = 0.2) +
    stat_cor(  # 添加Pearson相关系数和p值
      method = "pearson",
      label.x = 0.48, label.y = 0.7,  # 标签位置
      size = 5, color = "black",
      cor.coef.name = "r"
    )+
    theme(
      axis.text  = element_text(color = "black", size = 12),
      plot.margin = margin(20, 20, 30, 20)
    )
  
  
  

}