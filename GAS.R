library(GAS)
library(readr)
library(xts)
library(parallel)
library(dplyr)
library(highfrequency)
library(TTR)
library(rugarch)
library(ggplot2)
library(TSstudio)
library(forecast)
nCores <- detectCores() - 1
cluster <- makeCluster(nCores)

kernel_cov <- read.csv("Returns & RV/RV_kernel.csv", sep = " ")
kernel_cov$Index <-
  as.POSIXct(kernel_cov$Index, format = "%Y-%m-%d %H:%M:%S")
kernel_cov <- xts(kernel_cov$V1, kernel_cov$Index)


ret <-
  read.csv(file = "Returns & RV/Open_to_close_log_returns.csv", sep = " ")
ret$Index <- as.POSIXct(ret$Index, format = "%Y-%m-%d %H:%M:%S")
ret <- xts(ret$V1, ret$Index)

n_test <- 252 * 2
for (name in c(
  "norm",
  "snorm",
  "std",
  "sstd",
  "ast",
  "ast1",
  "ald",
  "poi",
  "gamma",
  "exp",
  "beta",
  "negbin",
  "skellam"
)){
  for(scaling in c("Identity", "Inv", "InvSqrt")) {
  print(paste(name, scaling))
  
  spec <- UniGASSpec(
    Dist = name,
    ScalingType = scaling,
    GASPar = list(
      location = TRUE,
      scale = TRUE,
      skewness = TRUE,
      shape = TRUE,
      shape2 = TRUE
    )
  )
  
  # fit <-
  #    UniGASFit(spec, ret, fn.optimizer = fn.optim, Compute.SE = TRUE)
  
  rol_for <- UniGASRoll(
    ret,
    spec,
    ForecastLength = n_test,
    RefitEvery = 1,
    RefitWindow = "moving",
    cluster = cluster,
    Compute.SE = FALSE
  )
  true_value <- tail(kernel_cov, n_test)
  
  forc_gas <- sqrt(rol_for@Forecast$Moments[, 2])
  write.csv(
    forc_gas,
    file = paste0("Forecasts/Open_to_close_GAS_", name, "_", scaling, "_Forecast.csv"),
    row.names = FALSE
  )
  MAE_gas <- as.numeric(abs(forc_gas - true_value))
  write.csv(
    MAE_gas,
    file = paste0("MAE/Open_to_close_GAS_", name, "_", scaling, "_MAE.csv"),
    row.names = FALSE
  )
  }
}

MAE_roc <- read.csv("MAE/Open_to_close_RealGARCH_MAE.csv")$x
MAE_oc <- read.csv("MAE/Open_to_close_GARCH_MAE.csv")$x
MAE_e <- read.csv("MAE/Open_to_close_EGARCH_MAE.csv")$x
MAE_gjr <- read.csv("MAE/Open_to_close_GJRGARCH_MAE.csv")$x

dm.test(MAE_gas_std, MAE_roc, alternative = "two.sided", h = 1)
dm.test(MAE_oc, MAE_roc, alternative = "two.sided", h = 1)
