linearfitting_end <- function(cumulative_mobility,dates_20_cases) {
  
cumulative_mobility 
dates_20_cases 
      
data <- data.frame(cumulative_mobility, dates_20_cases)
# 拟合线性模型
# model <- lm(log(dates_20_cases) ~ log(cumulative_mobility), data = data)
# 
# 
# 
# # 预测值和置信区间
# pred <- predict(model, interval = "confidence")
# # 将预测值添加到数据框
# data$fit <- pred[, "fit"]
# data$lwr <- pred[, "lwr"]
# data$upr <- pred[, "upr"]
# # 绘图
# ggplot(data, aes(x = cumulative_mobility, y = dates_20_cases)) +
#   geom_point() +
#   geom_line(aes(y = exp(fit)), color = "#7E2F8E") +
#   geom_ribbon(aes(ymin = exp(lwr), ymax = exp(upr)), alpha = 0.2, fill = "#7E2F8E") +
#   labs(
#     x = "Cumulative Mobility",
#     y = "Estimated infection"
#   ) +scale_y_log10()+scale_x_log10()+
#   theme_classic() +
#   theme(
#     axis.title.x = element_text(size = 12),
#     axis.title.y = element_text(size = 12, margin = margin(r = -1)) # Adjust
#   )




 ggplot(data, aes(x = cumulative_mobility, y = dates_20_cases)) +
  geom_point(color = "black", size = 1, alpha = 0.8) +
  #geom_text_repel(aes(label = Province), size = 3, color = "black")+
  labs(x = "Cumulative mobility", y = "Cumulative estimated infection") +
  theme_classic(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    panel.grid.minor = element_blank()
  ) + scale_x_log10()+scale_y_log10()+
  geom_smooth(method = "lm", formula = y ~ x, size=0.5,
              fill = "#7E2F8E", color = "#7E2F8E", se = TRUE,alpha = 0.2) +
  stat_cor(  # 添加Pearson相关系数和p值
    method = "pearson",
    label.x = 4, label.y = 4.5,  # 标签位置
    size = 4, color = "black",
    cor.coef.name = "r"
  )+
  theme(
    axis.text  = element_text(color = "black", size = 12),
    plot.margin = margin(20, 20, 30, 20)
  )




}