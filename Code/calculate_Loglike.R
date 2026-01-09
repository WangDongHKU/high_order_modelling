Sys.setenv("CXXFLAGS" = "-O3 -march=native -ffast-math")
# 可选：针对多核 CPU 启用并行编译（如 Make 的 -j 参数）
Sys.setenv("MAKEFLAGS" = "-j4")  # 根据 CPU 核心数调整（例如 4 核）
options(mc.cores = parallel::detectCores())
library(rstan)
library(tidyverse)
library(ggpubr)
library(R.matlab)
library(ggplot2)
library(dplyr)
library(quantreg)
library(dplyr)
library("bayesplot")
library("igraph")
library(gridExtra)
library(cowplot)
library(tidyr)
library(viridis)
library(ggrepel)
rstan_options(auto_write = TRUE)
knitr::opts_chunk$set(cache = T, echo = T, message = F, warning = F,include = T)
theme_set(theme_bw())
# Colourblind friendly colours
options(max.print = 10000)
setwd("/home/Dong/Higher_model_mainland/higher_010")
# ===========================================
# 置信区间计算和可视化函数库
# ===========================================
province_names <- c('Beijing', 'Tianjin', 'Hebei', 'Shanxi', 'Inner Mongolia', 'Liaoning', 'Jilin', 'Heilongjiang', 'Shanghai', 'Jiangsu', 'Zhejiang', 'Anhui', 'Fujian', 'Jiangxi', 'Shandong', 'Henan', 'Hubei', 'Hunan', 'Guangdong', 'Guangxi', 'Hainan', 'Chongqing', 'Sichuan', 'Guizhou', 'Yunnan', 'Tibet', 'Shaanxi', 'Gansu', 'Qinghai', 'Ningxia', 'Xinjiang')


load("/home/Dong/Higher_model_mainland/higher_010/Higer_order_hope_final_drift4003.Rdata")
m 
tail(multistrain_fit$log_lik[,seq(1:31),seq(1:165)], 4000) %>% mean()
nuts_fit_4_summary <- summary(stan_fit, pars = c("S0","E0","I0","p","sp","v","npi","thetap","rate","phi","div_rate","p_drift"))$summary
print(nuts_fit_4_summary,scientific=FALSE,digits=3,probs = c(0.025,0.975))

# 结果：31 × 165（省份 × 时间）
weighted_pred_cases <- log(apply(multistrain_fit$pred_cases[,seq(1:31),seq(1:165)],c(1,2),sum) * multistrain_fit$p[,seq(1:31)])

proportion_median <-  apply(weighted_pred_cases, c(2), function(x) quantile(x, probs = 0.5))
proprtion_mean <- apply(weighted_pred_cases, c(2), mean)

#proportion_median <- apply(proportion, c(2), function(x) quantile(x, probs = 0.5))
#proprtion_mean <- apply(proportion, c(2), mean)

highmedianloglike <- apply(tail(multistrain_fit$log_lik[,seq(1:31),seq(1:165)], 4000), c(2), function(x) quantile(x, probs = 0.5))
highmeanloglike <-apply(tail(multistrain_fit$log_lik[,seq(1:31),seq(1:165)], 4000), c(2), mean)
high_loglike <- tail(multistrain_fit$log_lik[,seq(1:31),seq(1:165)], 4000) 
save(proportion_median,proprtion_mean,highmedianloglike,highmeanloglike,high_loglike, file = "high_loglike3.Rdata")



load("/home/Dong/Higher_model_mainland/higher_010/high_loglike3.Rdata")

load("/home/Dong/Higher_model_mainland/standard_10_copy/standard_model_drift4000.Rdata")
tail(multistrain_fit$log_lik[,seq(1:31),seq(1:165)], 4000) %>% mean()
nuts_fit_4_summary <- summary(stan_fit, pars = c("S0","E0","I0","npi","phi","thetap","rate","p_drift"))$summary
#print(nuts_fit_4_summary,scientific=FALSE,digits=3,probs = c(0.025,0.975))

stdmedianloglike <- apply(tail(multistrain_fit$log_lik[,seq(1:31),seq(1:165)], 4000), c(2), function(x) quantile(x, probs = 0.5))
stdmeanloglike <-apply(tail(multistrain_fit$log_lik[,seq(1:31),seq(1:165)], 4000), c(2), mean)
std_loglike <- tail(multistrain_fit$log_lik[,seq(1:31),seq(1:165)], 4000) 
delta_loglike <- high_loglike - std_loglike
save(stdmedianloglike,stdmeanloglike,delta_loglike,std_loglike, file = "std_loglike3.Rdata")

df1 <- data.frame(Proportion = proportion_median,Improvement = highmedianloglike  - stdmedianloglike)
df2 <- data.frame(Proportion = proprtion_mean,Improvement = highmeanloglike - stdmeanloglike)
df3 <- data.frame(Proportion = proportion_median,Improvement = apply(delta_loglike, c(2), function(x) quantile(x, probs = 0.5)))
df4 <- data.frame(Proportion = proprtion_mean,Improvement = apply(delta_loglike, c(2), mean))

p_final1_1 <- ggplot(df1, aes(x = Proportion, y = Improvement)) +
  geom_point(color = "#2c7bb6", size = 3, alpha = 0.8) +  # 蓝色散点
  labs(x = "Affected proportion \n (p)", y = "Improvement of loglike \n (\u0394 loglike)") +
  theme_classic(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    panel.grid.minor = element_blank()
  )+
  geom_smooth(method = "lm", formula = y ~ x,
               fill = "#fdae61", se = TRUE,linetype = 0) +  # 红色拟合线+橙色置信带
  geom_quantile(quantiles = 0.5,  color = "#d7191c",  size = 1)+
  theme(
    axis.text  = element_text(color = "black",size = 14),
    axis.title = element_text(size = 18),
    strip.text.x = element_text(size = 18, margin = margin(1.5,0,1.5,0)),
    plot.margin = margin(30,20,30,20))
p_final1_1

p_final1_2 <- ggplot(df2, aes(x = Proportion, y = Improvement)) +
  geom_point(color = "#2c7bb6", size = 3, alpha = 0.8) +  # 蓝色散点
  labs(x = "Affected cases\n(log)", y = "Improvement of loglike \n (\u0394 loglike)") +
  theme_classic(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    panel.grid.minor = element_blank()
  )+
  geom_smooth(method = "lm", formula = y ~ x,
               fill = "#fdae61",color = "#d7191c", se = TRUE) +  # 红色拟合线+橙色置信带
  theme(
    axis.text  = element_text(color = "black",size = 14),
    axis.title = element_text(size = 18),
    strip.text.x = element_text(size = 18, margin = margin(1.5,0,1.5,0)),
    plot.margin = margin(30,20,30,20))
p_final1_2
df3$Province <- province_names

p_final1_3 <- ggplot(df3, aes(x = Proportion, y = Improvement)) +
  geom_point(color = "#2c7bb6", size = 3, alpha = 0.8) +
  geom_text_repel(aes(label = Province), size = 4, color = "black")+
  labs(x = "Affected cases\n(log)", y = "Improvement of loglike \n (\u0394 loglike)") +
  theme_classic(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    panel.grid.minor = element_blank()
  ) +
  geom_smooth(method = "lm", formula = y ~ x,
              fill = "#fdae61", color = "#d7191c", se = TRUE) +
  theme(
    axis.text  = element_text(color = "black", size = 14),
    axis.title = element_text(size = 18),
    strip.text.x = element_text(size = 18, margin = margin(1.5, 0, 1.5, 0)),
    plot.margin = margin(30, 20, 30, 20)
  )
p_final1_3

p_final1_4 <- ggplot(df4, aes(x = Proportion, y = Improvement)) +
  geom_point(color = "#2c7bb6", size = 3, alpha = 0.8) +  # 蓝色散点
  labs(x = "Affected cases\n(log)", y = "Improvement of loglike \n (\u0394 loglike)") +
  theme_classic(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    panel.grid.minor = element_blank()
  )+
  geom_smooth(method = "lm", formula = y ~ x,
               fill = "#fdae61",color = "#d7191c", se = TRUE) +  # 红色拟合线+橙色置信带
  theme(
    axis.text  = element_text(color = "black",size = 14),
    axis.title = element_text(size = 18),
    strip.text.x = element_text(size = 18, margin = margin(1.5,0,1.5,0)),
    plot.margin = margin(30,20,30,20))
p_final1_4

ggsave("high_loglike1_1.pdf",p_final1_1,width = 6,height = 4)
ggsave("high_loglike1_2.pdf",p_final1_2,width = 6,height = 4)
ggsave("high_loglike1_3.pdf",p_final1_3,width = 6,height = 4)
ggsave("high_loglike1_4.pdf",p_final1_4,width = 6,height = 4)



delta_stats <- data.frame(
  Province = province_names,
  Median = apply(delta_loglike, 2, median),
  CI95_lower = apply(delta_loglike, 2, function(x) quantile(x, 0.025)),
  CI95_upper = apply(delta_loglike, 2, function(x) quantile(x, 0.975))
)

delta_stats_plot1 <- ggplot(delta_stats, aes(x = reorder(Province, Median), y = Median)) +
  geom_point(color = "#2c7bb6", size = 3) +
  geom_errorbar(aes(ymin = CI95_lower, ymax = CI95_upper), width = 0.2, color = "#d7191c") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  coord_flip() +
  labs(
    x = "省份",
    y = "Delta LogLikelihood (中位数)",
    title = "31省份Delta LogLikelihood及95%置信区间"
  ) +
  theme_classic(base_size = 14)
ggsave("delta_stats_plot1.pdf",delta_stats_plot1,width = 6,height = 10)
delta_stats_plot1
