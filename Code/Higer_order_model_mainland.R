library(rstan)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)
library(tidyverse)
library(ggpubr)
library(R.matlab)
library(xlsx)
knitr::opts_chunk$set(cache = T, echo = T, message = F, warning = F,include = T)
theme_set(theme_bw())
# Colourblind friendly colours
cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#999999")
scale_colour_discrete <- function(...)
  scale_colour_manual(..., values = cbPalette)
scale_fill_discrete <- function(...)
  scale_fill_manual(..., values = cbPalette)
setwd("/home/Dong/Higher_model_mainland/")
m <- stan_model('higher_model_covid_mainland_spline.stan')
options(max.prints=999)
subtypes <- c('Beijing', 'Tianjin', 'Hebei', 'Shanxi', 'Inner Mongolia', 'Liaoning', 'Jilin', 'Heilongjiang', 'Shanghai', 'Jiangsu', 'Zhejiang', 'Anhui', 'Fujian', 'Jiangxi', 'Shandong', 'Henan', 'Hubei', 'Hunan', 'Guangdong', 'Guangxi', 'Hainan', 'Chongqing', 'Sichuan', 'Guizhou', 'Yunnan', 'Tibet', 'Shaanxi', 'Gansu', 'Qinghai', 'Ningxia', 'Xinjiang')
skip <- 0
row_names <-subtypes
weeklydata <- readMat("Case/weeklydata.mat")# DAILY 1 delay 
weeklydata <- t(weeklydata[["weeklydata"]]);
weeklydata[weeklydata < 0] <- 0
NPI <- read.csv("NPI/NPI_province_China_20200101_20230228.csv",skip = 0, nrows = 31, row.names = row_names)
NPI <- NPI[, -seq(1,2)]  # 删除第3,5列
AirQuality <- read.csv("city_AirQualitys.csv",skip = 0, nrows = 1155, col.names = row_names)
Mean_Temp <- read.csv("city_mean_temps.csv",skip = 0, nrows = 1155, col.names = row_names)
# mobility
C <-  read.csv("mobility/C.csv",skip = 0)
# C <- t(C)
nl <-  read.csv("mobility/nl.csv", row.names=NULL)
part <-  read.csv("mobility/part.csv",skip = 0)

# NPI <-colnames(NPI) <- paste(seq(as.Date("2020-01-01"), as.Date("2023-02-28"), by = "day"), sep = "")

K=31;error <- 10^(-18)
N = c(21893095,13866009,74610235,34915616,24049155,42591407,24073453,31850088,24870895,84748016,
      64567588,61027171,41540086,45188635,101527453,99365519,57752557,66444864,126012510,50126804,10081232,32054159,83674866,38562148,47209277,3648100,39528999,25019831,5923957,7202654,25852345);
# knots = 1+7*c(0,51,)
X <-  seq(from=1, to=1156, by=1);num_knots  <-  6; spline_degree <- 4;

data_lst <- list(
  K = K, 
  T = nrow(weeklydata)*7,
  W = nrow(weeklydata),
  N = N,
  cases = weeklydata %>% as.matrix(), 
  NPI1 = NPI%>% as.matrix(),
  Mean_Temp = Mean_Temp  %>% as.matrix(),
  AirQuality= AirQuality %>% as.matrix(),
  C= C,
  nl=nl[,1],
  part=part[,1],
  phi0 = 2,
  error = error,
  num_data = nrow(weeklydata)*7 + 1,
  num_knots = num_knots,
  knots = unname(quantile(X,probs=seq(from=0, to=1, length.out = num_knots))),
  spline_degree = spline_degree,
  X = X
)
opt <- optimizing(m, data = data_lst)
num_chains <- 2
init_lst <- purrr::map(1:num_chains, function(i) {
  list(
    # beta0 =  rnorm(K,opt[["par"]][paste0("beta0[", 1:31, "]")],0.001),
    thetap =opt[["par"]][paste0("thetap[", 1:31, "]")]+error,
    S0 =  runif(K,0.96,1),
    E0 =  opt[["par"]][paste0("E0[", 1:31, "]")]+error,
    I0 =  opt[["par"]][paste0("I0[", 1:31, "]")]++error,
    air =  opt[["par"]][paste0("air")]+error,
    temp =opt[["par"]][paste0("temp")]+error, 
    npi = opt[["par"]][paste0("npi")]+error,
    phi = opt[["par"]][paste0("phi")]+error,
    beta2=opt[["par"]][paste0("beta2")]+error,
    a_raw = matrix(sample(-1:0, (num_knots+spline_degree-1)*K, replace = TRUE), ncol = num_knots + spline_degree - 1, nrow  = K)
  )})
parameters_stoch = c("pred_cases","phi","a_raw","beta2","thetap","S0","E0","I0","npi","air","temp","S","R","I","E","beta")

stan_fit <- rstan::sampling(
  m, 
  data = data_lst, 
  pars = parameters_stoch,
  chains = num_chains, 
  iter = 1000,  
  thin = 1, 
  cores = num_chains, 
  verbose = TRUE,
  show_messages = TRUE,
  seed = sample.int(.Machine$integer.max, 1),
  init = init_lst
)

multistrain_fit <- rstan::extract(stan_fit)
save(list = ls(), file = "/home/Dong/Higher_model_mainland/Higer_order_model_mainland.Rdata")

nuts_fit_4_summary <- summary(stan_fit, pars = c("thetap","beta2","npi","air","temp","ap1"))$summary
print(nuts_fit_4_summary,scientific=FALSE,digits=6)



