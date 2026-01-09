Sys.setenv("CXXFLAGS" = "-O3 -march=native -ffast-math")
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
load("/home/Dong/Higher_model_mainland/standard_10_copy/standard_model_10_copy.Rdata")
#multistrain_fit <- rstan::extract(stan_fit)
m <- rstan::stan_model('/home/Dong/Higher_model_mainland/higher_010/higher_order_model_10.stan')
m
options(max.prints=999)
subtypes <- c('Beijing', 'Tianjin', 'Hebei', 'Shanxi', 'Inner Mongolia', 'Liaoning', 'Jilin', 'Heilongjiang', 'Shanghai', 'Jiangsu', 'Zhejiang', 'Anhui', 'Fujian', 'Jiangxi', 'Shandong', 'Henan', 'Hubei', 'Hunan', 'Guangdong', 'Guangxi', 'Hainan', 'Chongqing', 'Sichuan', 'Guizhou', 'Yunnan', 'Tibet', 'Shaanxi', 'Gansu', 'Qinghai', 'Ningxia', 'Xinjiang')
skip <- 0
row_names <-subtypes
weeklydata <- readMat("Case/weeklydata.mat")# DAILY 1 delay 
weeklydata <- t(weeklydata[["weeklydata"]]);
weeklydata[weeklydata < 0] <- 0
weeklydata <- weeklydata[1:165,]

NPI <- read.csv("NPI/NPI_province_China_20200101_20230228.csv",skip = 0, nrows = 31, row.names = row_names)
NPI <- NPI[, -seq(1,2)]  

AirQuality <- read.csv("city_AirQualitys.csv",skip = 0, nrows = 1155, col.names = row_names)
Mean_Temp <- read.csv("city_mean_temps.csv",skip = 0, nrows = 1155, col.names = row_names)
diversity  <- read.csv("/home/Dong/Global_COVID/genomic_analysis/diversity_weekly_wide.csv")
diversity <- as.numeric(diversity[1,2:166])
drift <- read.csv("/home/Dong/Global_COVID/genomic_analysis/drift_weekly_wide.csv")
drift <- as.numeric(drift[1,2:166])


# mobility
C <-  read.csv("mobility/C.csv",skip = 0)
# C <- t(C)
nl <-  read.csv("mobility/nl.csv", row.names=NULL)
part <-  read.csv("mobility/part.csv",skip = 0)
pop_density <- read.csv("population/pop_density.csv",skip = 0)

# NPI <-colnames(NPI) <- paste(seq(as.Date("2020-01-01"), as.Date("2023-02-28"), by = "day"), sep = "")

K=31;error <- 10^(-10)
N = c(21893095,13866009,74610235,34915616,24049155,42591407,24073453,31850088,24870895,84748016,
64567588,61027171,41540086,45188635,101527453,99365519,57752557,66444864,126012510,50126804,10081232,32054159,83674866,38562148,47209277,3648100,39528999,25019831,5923957,7202654,25852345);
#load("/home/Dong/Higher_model_mainland/a_raw_hig.Rdata")  

X <-  seq(from=1, to=1155, by=1);num_knots  <-  10; spline_degree <- 3;
a_raw <-  matrix(apply(multistrain_fit$a_raw, c(2,3),  function(x) quantile(x, probs = 0.5, na.rm = TRUE)), ncol = num_knots + spline_degree - 1, nrow  = K)

# sp_ini <-apply(tail(multistrain_fit$sp[,seq(1:31)], 5000), c(2), median)

phi_ini = apply(tail(multistrain_fit$phi[,seq(1:31)], 2000), c(2), median)
# thetap =apply(tail(multistrain_fit$thetap[,seq(1:31)], 5000), c(2), median)
# p_ini <- apply(tail(multistrain_fit$p[,seq(1:31)], 5000), c(2), median)


data_lst <- list(
  K = K, 
  T = nrow(weeklydata)*7,
  W = nrow(weeklydata),
  N = N,
  cases = weeklydata %>% as.matrix(), 
  NPI1 = NPI%>% as.matrix(),
  C= C,
  nl=nl[,1],
  part=part[,1],
  phi0 = 1,
  error = 10^(-10),
  num_data = nrow(weeklydata)*7,
  num_knots = num_knots,
  knots = unname(quantile(X,probs=seq(from=0, to=1, length.out = num_knots))),
  spline_degree = spline_degree,
  X = X,
  a_raw1=a_raw,
  diversity = diversity,
  drift = drift
  # p_ini = p_ini,
  # sp_ini = sp_ini
)


num_chains <- 4
init_lst <- purrr::map(1:num_chains, function(i) {
  list(
    thetap =runif(1,0.0061,0.0075),
    S0 =  c(0.98,0.99),
    E0 =  c(0.0055,0.00005),
    I0 =  c(0.0095,0.00005),
    npi = runif(1,0.008,0.011),
    phi   = phi_ini,
    v= c(0.5,0.7,0.6),  
    a_raw = a_raw, 
    p =  runif(K,0.1,0.15),
    sp = runif(K,1.4,1.6),
    rate = runif(1,0.032,0.034),
    div_rate = runif(1,0,0.1)
  )})

parameters_stoch = c("log_lik","Rt1","p","sp","pred_cases","phi","a_raw","v","thetap","thetapP","rate","S0","E0","I0","npi","S","R","I","E","div_rate")

stan_fit <- rstan::sampling(
  m, 
  data = data_lst, 
  pars = parameters_stoch,
  chains = num_chains, 
  iter = 4000,
  thin = 1, 
  cores = num_chains, 
  verbose = TRUE,
  show_messages = TRUE,
  seed = sample.int(.Machine$integer.max, 1),
  control = list(adapt_delta = 0.95, max_treedepth = 15),
  init = init_lst
)


multistrain_fit <- rstan::extract(stan_fit)
nuts_fit_4_summary <- summary(stan_fit, pars = c("S0","E0","I0","p","sp","v","npi","thetap","rate","phi","div_rate"))$summary
save(list = ls(), file = "/home/Dong/Higher_model_mainland/higher_010/Higer_order_hope_final1.Rdata")
print(nuts_fit_4_summary,scientific=FALSE,digits=3, probs = c(0.025, 0.975))


tail(multistrain_fit$log_lik[,seq(1:31),seq(1:165)], 2000) %>% mean()
print(nuts_fit_4_summary,scientific=FALSE,digits=3)
setwd("/home/Dong/Higher_model_mainland/higher_010")
rstan::check_divergences(stan_fit)
# nuts_fit_4_summary <- summary(stan_fit, pars = c("phi0","k0","npi","lp__"))$summary

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
  geom_ribbon(data = credible_intervals, aes(x = Length, ymin = lower, ymax = upper, fill = "Fitting"), alpha = 0.2) +
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
  
ggsave("Fig3_HIG_10_hope_final1.pdf", plot1, width = 19, height = 11)


a_raw <-  a_raw[1:31,1:11]
interpolate_row <- function(row) {
  x_original <- 1:11
  x_new <- seq(1, 11, length.out = 12)
  spline(x_original, row, xout = x_new, method = "natural")$y
}
a_raw <- t(apply(a_raw, 1, interpolate_row))  

a_raw <- matrix(0, nrow = K, ncol = num_knots + spline_degree - 1)
a_raw[a_raw > 3] <- 2
a_raw[1,] <- c(0.801222,	-4.80054,	0.643602,	-3.28125,	-0.94929,	-2.4285,	-1.13262,	-1.64875,	-1.2337,	-2.23154,	-0.99381,	-0.6313)
a_raw[2,] <- c(0.117915,	-2.85876,	-2.22149,	-0.99767,	-2.23328,	-1.57107,	-1.83243,	-1.11144,	-2.07148,	-1.64196,	-1.01956,	-2.49237)
a_raw[3,] <- c(-1.02449,	-3.78635,	-4.71541,	-4.37492,	1.880201,	-8.54644,	-1.22418,	-0.69359,	-6.99844,	-5.73526,	-4.11422,	-3.82405)
a_raw[4,] <- c(-3.30741,	-1.19155,	-3.27115,	-1.54657,	-1.47793,	-2.31682,	-1.24074,	-3.44736,	-0.29617,	-5.44191,	1.328841,	-2.26242)
a_raw[5,] <- c(-2.19827,	-1.0763,	-2.7914,	-1.23578,	-1.85347,	-2.09885,	-1.27134,	-0.9883,	-4.00799,	0.83568,	-3.24572,	-0.78569)
a_raw[6,] <- c(0.639461,	-5.16813,	-0.06391,	-2.48979,	-1.14931,	-2.70684,	-1.01446,	-1.46209,	-4.24779,	-0.09355,	-2.13057,	-1.77249)
a_raw[7,] <- c(-0.48259,	-3.92554,	0.808191,	-5.73504,	1.04014,	-6.44878,	-7.37056,	0.035908,	0.42771,	-1.45726,	-2.14338,	-1.06032)
a_raw[8,] <- c(-2.12239,	0.403204,	-4.14056,	-4.61008,	1.90999,	-7.42683,	1.266003,	-2.88263,	-2.32204,	-2.43624,	0.101375,	-3.04327)
a_raw[9,] <- c(-0.52397,	-2.65598,	-1.61849,	-1.39487,	-1.79562,	-1.8483,	-1.65892,	-1.64183,	-0.21688,	-4.32329,	1.032404,	-4.43548)
a_raw[10,] <- c(-1.77932,	-3.57194,	-7.66504,	-4.44626,	-9.28744,	-2.61177,	0.893586,	-10.0978,	-7.58326,	-5.99425,	1.507133,	-2.5632)
a_raw[11,] <- c(-1.68854,	-1.51757,	-5.7753,	-3.24878,	-2.33074,	-1.50517,	-2.10626,	-0.73585,	-2.65852,	-2.76443,	-0.60969,	-2.31548)
a_raw[12,] <- c(-0.66179,	-3.37504,	-11.2949,	-7.05842,	-6.62542,	-0.88935,	-2.97742,	-9.54028,	-3.54865,	1.1362,	-8.44159,	-2.16273)
a_raw[13,] <- c(-2.08508,	-2.70367,	-2.55516,	-0.76191,	-2.32817,	-1.43153,	-1.46323,	-2.03676,	-0.87175,	-2.54051,	-0.61754,	-1.8178)
a_raw[14,] <- c(-0.53974,	-4.21577,	-8.926,	-4.73428,	-4.72903,	-9.00393,	0.127841,	-2.99307,	-0.97689,	-2.44332,	-3.22108,	-2.50947)
a_raw[15,] <- c(-0.52878,	-3.35598,	-2.65149,	-0.96369,	-2.76969,	-1.64823,	-1.48532,	-1.5261,	-2.9202,	-1.56121,	-0.71707,	-4.39289)
a_raw[16,] <- c(-2.35315,	-7.78487,	-4.85458,	-1.58787,	-2.39651,	-1.8833,	-1.21327,	-1.79306,	-1.4097,	-3.49704,	-0.50826,	-0.7292)
a_raw[17,] <- c(-0.42651,	-0.10068,	-2.98159,	1.136575,	0.94651,	1.832458,	1.806429,	0.595233,	0.824325,	-4.68846,	1.256065,	0.729946)
a_raw[18,] <- c(-2.51381,	-6.54712,	-10.4607,	-4.31107,	-1.5412,	-1.88074,	-1.17956,	-2.52722,	-0.74483,	-6.18925,	0.789553,	-1.71889)
a_raw[19,] <- c(-3.83365,	-3.20622,	-1.61437,	-1.40818,	-2.12384,	-1.20324,	-2.30162,	-0.96268,	-1.67989,	-1.8003,	-1.16545,	-0.63314)
a_raw[20,] <- c(0.048726,	-5.01944,	-4.94504,	-0.8115,	-3.16845,	-1.76882,	-1.91107,	-0.57316,	-2.96959,	-0.00203,	-4.04968,	-3.4448)
a_raw[21,] <- c(0.88345,	-6.28309,	-0.03501,	-3.78426,	-2.36615,	-0.48556,	-4.77915,	-2.21735,	-2.87619,	1.861788,	-6.32931,	-0.96703)
a_raw[22,] <- c(-2.22861,	-6.20169,	-1.64991,	-2.00825,	-2.935,	-1.39321,	-2.60002,	-1.54702,	-2.90096,	-0.79158,	-1.69968,	-0.95419)
a_raw[23,] <- c(-1.94332,	-4.4837,	-1.07236,	-1.62884,	-2.00086,	-1.2976,	-2.17007,	-1.5271,	-1.81306,	-1.041968,	-1.43064,	-0.9003)
a_raw[24,] <- c(-1.20147,	-4.32438,	-6.1673,	-5.52313,	-4.1707,	-7.61591,	-1.39933,	-1.36774,	-7.87869,	-2.49952,	1.31123,	-5.96065)
a_raw[25,] <- c(-1.3415,	-4.44718,	-1.34648,	-1.32214,	-2.24366,	-0.73504,	-1.60863,	-1.59083,	-1.86389,	-1.79261,	-1.0941,	-1.27918)
a_raw[26,] <- c(-1.46181,	-5.07376,	-1.49374,	-2.78542,	-2.84254,	-2.32478,	-3.02205,	-2.57452,	-4.88197,	1.741058,	-2.39155,	-3.38838)
a_raw[27,] <- c(-3.55039,	-2.18,	-2.14789,	-0.88193,	-2.51987,	-0.88524,	-2.85211,	0.370426,	-3.64146,	-0.18976,	-1.82777,	-1.0833)
a_raw[28,] <- c(-0.00342,	-4.4233,	-0.13955,	-2.87473,	-1.0147,	-2.68988,	-0.87705,	-1.54889,	-2.48081,	0.436892,	-6.39412,	-3.86392)
a_raw[29,] <- c(0.703906,	-5.67338,	-3.25152,	-3.30698,	-2.93913,	-4.1045,	-0.37977,	-2.67124,	-0.72828,	-2.94732,	0.412564,	-4.40379)
a_raw[30,] <- c(1.355251,	-4.95553,	-3.22623,	-3.33032,	-2.97583,	-4.24707,	-0.01843,	-2.23388,	-6.38286,	-3.55789,	0.979484,	-4.95853)
a_raw[31,] <- c(0.80051,	-5.80519,	-0.03664,	-1.09006,	-3.92058,	-4.25263,	-1.71257,	-0.67983,	-3.83404,	-2.01948,	1.151769,	-4.3108)