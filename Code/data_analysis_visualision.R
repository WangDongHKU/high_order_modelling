# 设置编译器优化级别为最高 (-O3)，并启用本地 CPU 架构优化 (-march=native)
Sys.setenv("CXXFLAGS" = "-O3 -march=native -ffast-math")

# 可选：针对多核 CPU 启用并行编译（如 Make 的 -j 参数）
Sys.setenv("MAKEFLAGS" = "-j4")  # 根据 CPU 核心数调整（例如 4 核）

library(rstan)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)
library(tidyverse)
library(ggpubr)
library(R.matlab)
#library(xlsx)
knitr::opts_chunk$set(cache = T, echo = T, message = F, warning = F,include = T)
theme_set(theme_bw())
# Colourblind friendly colours
cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#999999")
scale_colour_discrete <- function(...)
  scale_colour_manual(..., values = cbPalette)
scale_fill_discrete <- function(...)
  scale_fill_manual(..., values = cbPalette)
setwd("/home/Dong/Higher_model_mainland/")

load("/home/Dong/Higher_model_mainland/higher_010/Higer_order_hope_final_drift4003.Rdata")
m
a_raw <-  matrix(apply(multistrain_fit$a_raw, c(2,3),  function(x) quantile(x, probs = 0.5)), ncol = num_knots + spline_degree - 1, nrow  = K)
a_raw

setwd("/home/Dong/Higher_model_mainland/higher_010")
print(nuts_fit_4_summary,scientific=FALSE,digits=3, 
      probs = c(0.025, 0.975))
tail(multistrain_fit$log_lik[,seq(1:31),seq(1:165)], 1000) %>% mean()

rstan::check_divergences(stan_fit)
#  nuts_fit_4_summary <- summary(stan_fit, pars = c("p","sp","v","npi","air","temp","thetap","thetap2","rate","phi"))$summary
# print(nuts_fit_4_summary,scientific=FALSE,digits=3)
#save(list = ls(), file = "/home/Dong/Rstan_Globally/model_str_multi_Base/model_str_multi_Base_NB.Rdata")
#library(sf)
library(ggplot2)
library(reshape2)
pred_cases <- tail(multistrain_fit$pred_cases[,seq(1:31),seq(1:165)], 2000)
# Reshape the data
data_frame <- reshape2::melt(sqrt(1+pred_cases))
colnames(data_frame) <- c("Iteration", "Variable", "Length", "Value")
# Calculate credible intervals
credible_intervals <- data_frame %>%
  group_by(Variable, Length) %>%
  summarize(
    lower = quantile(Value, 0.025),
    upper = quantile(Value, 0.975),
    lower1 = quantile(Value, 0.25),
    upper1 = quantile(Value, 0.75),
    mean = mean(Value)#quantile(Value, 0.5)
  ) %>%
  mutate(Variable = factor(Variable))

# Prepare ILI data
data_ILI <- sqrt(1+data_lst$cases) %>% tibble::as_data_frame() %>% mutate(week = row_number())
colnames(data_ILI) <- seq_len(ncol(data_ILI))
data_ILI$id <- seq_len(nrow(data_ILI))
# data_ILI$Variable <- seq_len(nrow(data_ILI))

data_ILI_long <- reshape2::melt(data_ILI, id.vars = "id")
colnames(data_ILI_long) <- c("Length", "Variable", "Value")

# Combine credible intervals and ILI data
credible_intervals <- credible_intervals %>%
  left_join(data_ILI_long, by = c("Variable", "Length"))

custom_labeller <- function(variable, value) {
  return(c('Beijing', 'Tianjin', 'Hebei', 'Shanxi', 'Inner Mongolia', 'Liaoning', 'Jilin', 'Heilongjiang', 'Shanghai', 'Jiangsu', 'Zhejiang', 'Anhui', 'Fujian', 'Jiangxi', 'Shandong', 'Henan', 'Hubei', 'Hunan', 'Guangdong', 'Guangxi', 'Hainan', 'Chongqing', 'Sichuan', 'Guizhou', 'Yunnan', 'Xizang', 'Shaanxi', 'Gansu', 'Qinghai', 'Ningxia', 'Xinjiang'))
}

plot1 <- ggplot() +
  geom_point(data = credible_intervals, aes(x = Length, y = Value, alpha = "Data", show.legend = "TRUE"), size = 0.3) + 
  geom_ribbon(data = credible_intervals, aes(x = Length, ymin = lower, ymax = upper, fill = "Fitting"), alpha = 0.15) +
  geom_ribbon(data = credible_intervals, aes(x = Length, ymin = lower1, ymax = upper1, fill = "Fitting"), alpha = 0.3) +
  geom_line(data = credible_intervals, aes(x = Length, y = mean, color = "Fitting")) +  # 共享 color 图例
  theme_classic() +
  theme(
    axis.text.y = element_text(size = 20),
    axis.text.x = element_blank(),
    axis.ticks.x = element_line(color = "black"),
    axis.title.y = element_text(size = 20),
    axis.title.x = element_text(size = 20),
    # axis.text.x = element_text(ifelse(Variable == bottom_row, element_text(size = 12), 12)),
    # panel.background = element_rect(fill = "transparent",color=NA),
    # plot.background = element_rect(fill = "transparent",color=NA),
    axis.line = element_line(color = "black"),
    legend.position = c(0.97, 0.075),
    legend.justification = c(1, 0),
    strip.text = element_text(size = 16, hjust = 0.5, vjust = -0.5),
    strip.placement = "inside",
    legend.spacing = unit(0.0, "cm"),  # 调整图例项之间的水平间距
    legend.spacing.y = unit(-0.0, "cm")  # 调整图例项之间的垂直间距
  ) + #scale_y_sqrt()+
  facet_wrap(~ Variable, ncol = 8, scales = "free", labeller = custom_labeller) +#, strip.position = "top"
  labs(
    x = "Time",
    y = expression(sqrt(Incidences))
  ) +
  scale_x_continuous(breaks = c(1, 53.25, 105.5, 151.75), labels = c("2020", "2021", "2022", "2023")) +
  scale_alpha_manual(
    name = "",  # 设置共享图例名称
    values = c("Data" = 255)
  ) +  
  scale_fill_manual(
    name = "",  # 设置共享图例名称
    values = c("Fitting" = "#F8766D")
  ) +
  scale_color_manual(
    name = "",  # 设置共享图例名称
    values = c("Fitting" = "#F8766D")
  ) +
  guides(
    alpha = guide_legend(theme = theme(legend.text = element_text(size = 18)),order = 1, override.aes = list(fill = "black", color = "black")),
    fill = guide_legend(theme = theme(legend.text = element_text(size = 18)),order = 2),
    color = guide_legend(theme = theme(legend.text = element_text(size = 18)),order = 2)
  )

ggsave("Fig3_fitting_hig_10762.pdf", plot1, width = 19, height = 11)


library(showtext)
#font_add("dejavu","C:\Windows\Fonts")
showtext_auto()
library(ggplot2)
library(ggridges)
library(cowplot)
library(viridis)
# Custom color scales for each parameter
library(extrafont)
loadfonts(device="pdf")          # 加载字体，特别针对 PDF 输出


# Convert Stan samples to a tidy data frame
p_samples <- as.data.frame(multistrain_fit$p) %>% 
  setNames(c('Beijing', 'Tianjin', 'Hebei', 'Shanxi', 'Inner Mongolia', 
             'Liaoning', 'Jilin', 'Heilongjiang', 'Shanghai', 'Jiangsu', 
             'Zhejiang', 'Anhui', 'Fujian', 'Jiangxi', 'Shandong', 
             'Henan', 'Hubei', 'Hunan', 'Guangdong', 'Guangxi', 
             'Hainan', 'Chongqing', 'Sichuan', 'Guizhou', 'Yunnan', 
             'Tibet', 'Shaanxi', 'Gansu', 'Qinghai', 'Ningxia', 'Xinjiang')) %>%
  pivot_longer(everything(), names_to = "Province", values_to = "p")


sp_samples <- as.data.frame(multistrain_fit$sp) %>% 
  setNames(c('Beijing', 'Tianjin', 'Hebei', 'Shanxi', 'Inner Mongolia', 
             'Liaoning', 'Jilin', 'Heilongjiang', 'Shanghai', 'Jiangsu', 
             'Zhejiang', 'Anhui', 'Fujian', 'Jiangxi', 'Shandong', 
             'Henan', 'Hubei', 'Hunan', 'Guangdong', 'Guangxi', 
             'Hainan', 'Chongqing', 'Sichuan', 'Guizhou', 'Yunnan', 
             'Tibet', 'Shaanxi', 'Gansu', 'Qinghai', 'Ningxia', 'Xinjiang')) %>%
  pivot_longer(everything(), names_to = "Province", values_to = "sp")


v_samples <- as.data.frame(multistrain_fit$v) %>% 
  setNames(c("Ancestral","Alpha, Delta", "Omicron")) %>%
  pivot_longer(everything(), names_to = "Periods", values_to = "v")


fig_p <- ggplot(p_samples, aes(x = p, y = reorder(Province, p, median), fill = ..x..)) +
    geom_density_ridges_gradient(scale = 1.5, alpha = 0.7) +
    scale_fill_viridis_c(name = "p", option = "plasma") +  # Plasma palette
    labs( x = "p", y = "Provinces") +
    theme_ridges() + 
    theme_classic() +
    theme(axis.title.y = element_text(size = 18))  # Correct way to set y-axis title size

fig_p
ggsave("Fig4_p.pdf", fig_p, width = 5.3, height = 9)
fig_phi <- ggplot(sp_samples, aes(x = sp, y = reorder(Province, sp, median), fill = ..x..)) +
    geom_density_ridges_gradient(scale = 1.5, alpha = 0.7) +
    scale_fill_viridis_c(name = expression(varphi), option = "turbo") +  # Viridis palette
    labs( x = expression(varphi), y = NULL) +
    theme_ridges() + 
    theme_classic()+
    theme(axis.title.y = element_text(size = 18),
          text = element_text(family = "dejavu"))  # Correct way to set y-axis title size

fig_phi
ggsave("Fig4_phi.pdf", fig_phi, width = 5.3, height = 9)
fig_v <- ggplot(v_samples, aes(x = v, y = reorder(Periods, v, median), fill = ..x..)) +
    geom_density_ridges_gradient(scale = 1.5, alpha = 0.7) +
    scale_fill_viridis_c(name = "v", option = "dodge") +  # Magma palette
    labs( x = "v", y = "Periods") +
    theme_ridges() + 
    theme_classic() +
    theme(axis.title.y = element_text(size = 18))  # Correct way to set y-axis title size
fig_v
ggsave("Fig4_v.pdf", fig_v, width = 5.3, height = 9)


library(ggplot2)
library(cowplot)
library(extrafont)
loadfonts(device = "pdf")  # 每个session跑一次即可

p1 <- fig_p + theme(text = element_text(family = "Arial"))
p2 <- fig_phi + theme(text = element_text(family = "Arial"))
p3 <- fig_v + theme(text = element_text(family = "Arial"))

combined_plot<-plot_grid(
  p1, p2, p3,
  ncol = 3,
  align = "h",
  rel_widths = c(1.0, 1.0, 1.0),
  labels = "AUTO",
  label_size = 18,
  label_fontfamily = "Arial"    # cowplot >= 1.0支持该参数
)



# Save or display
ggsave("combined_posteriors.pdf", combined_plot, width = 16, height = 9)

CairoPDF("combined_posteriors1.pdf", family = "Arial")
combined_plot
dev.off()



