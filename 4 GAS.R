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

## This script is an implementation of the GAS (Generalized Autoregressive Score) 
## method for volatility forecasting. It uses the R libraries GAS, readr, xts, parallel, 
## dplyr, highfrequency, TTR, rugarch, ggplot2, TSstudio, and forecast.
## The script loads and processes two datasets, "Open_to_close_log_returns.csv" and 
## "Close_to_close_log_returns.csv", which contain log returns of financial assets. 
## Then, the script performs a series of forecasts using the GAS method. The UniGASSpec 
## function is used to specify the model's distribution, scaling, and parameters. 
## The UniGASFit function is used to fit the model to the log returns data, and 
## the UniGASRoll function is used to produce rolling window forecasts of volatility. 
## The script writes the log densities, forecasts, and mean absolute errors to separate 
## CSV files for each combination of distribution and scaling. The script closes 
## with commented out code for plotting the true volatility and the forecasted volatility.

nCores <- detectCores() - 1
cluster <- makeCluster(nCores)

n_test <- 252 * 2
kernel_cov <- read.csv("Returns & RV/RV_kernel2.csv", sep = " ")
kernel_cov$Index <-
  as.POSIXct(kernel_cov$Index, format = "%Y-%m-%d %H:%M:%S")
kernel_cov <- as.numeric(xts(kernel_cov$V1, kernel_cov$Index))

ret <-
  read.csv(file = "Returns & RV/Open_to_close_log_returns.csv", sep = " ")
ret$Index <- as.POSIXct(ret$Index, format = "%Y-%m-%d %H:%M:%S")
ret <- xts(ret$V1, ret$Index)


################################################################################
################################################################################
## Rolling window

for (name in c("std",
               "sstd",
               "ast",
               "ald")) {
  for (scaling in c("Identity")) {
    print(paste(name, scaling))
    
    spec <- UniGASSpec(
      Dist = name,
      ScalingType = scaling,
      GASPar = list(
        location = FALSE,
        scale = TRUE,
        skewness = FALSE,
        shape = FALSE,
        shape2 = FALSE
      )
    )
    
    fit <-
      UniGASFit(spec, ret, fn.optimizer = fn.optim, Compute.SE = TRUE)
    ld_gas <- as.numeric(fit@GASDyn$vLLK)
    write.csv(
      ld_gas,
      file = paste0("Log density/OCld_GAS_", name, "_", scaling, ".csv"),
      row.names = FALSE
    )
    
    rol_for <- UniGASRoll(
      ret,
      spec,
      ForecastLength = n_test,
      RefitEvery = 3,
      RefitWindow = "moving",
      cluster = cluster,
      Compute.SE = FALSE
    )
    true_value <- tail(kernel_cov, n_test)
    
    forc_gas_ret <- as.numeric(rol_for@Forecast$Moments[, 1])
    write.csv(
      forc_gas_ret,
      file = paste0(
        "Forecasts/OCret_GAS_",
        name,
        "_",
        scaling,
        "_Forecast.csv"
      ),
      row.names = FALSE
    )
    
    forc_gas <- as.numeric(rol_for@Forecast$Moments[, 2])
    write.csv(
      forc_gas,
      file = paste0(
        "Forecasts/OCvol_GAS_",
        name,
        "_",
        scaling,
        "_Forecast.csv"
      ),
      row.names = FALSE
    )
    MAE_gas <- as.numeric(abs(forc_gas - true_value))
    write.csv(
      MAE_gas,
      file = paste0("MAE/OCvol_GAS_", name, "_", scaling, "_MAE.csv"),
      row.names = FALSE
    )
  }
}

################################################################################
################################################################################
## Close-to-close

ret_cc <-
  read.csv(file = "Returns & RV/Close_to_close_log_returns.csv", sep = " ")
ret_cc$Index <-
  as.POSIXct(ret_cc$Index, format = "%Y-%m-%d %H:%M:%S")
ret_cc <- xts(ret_cc$V1, ret_cc$Index)
ret_cc <- ret_cc[2:length(ret_cc), ]

## Rolling window estimation ####
n_test <- 252 * 2
for (name in c("ast")) {
  for (scaling in c("Identity")) {
    print(paste(name, scaling))
    
    spec <- UniGASSpec(
      Dist = name,
      ScalingType = scaling,
      GASPar = list(
        location = FALSE,
        scale = TRUE,
        skewness = FALSE,
        shape = FALSE,
        shape2 = FALSE
      )
    )
    
    fit <-
      UniGASFit(spec, ret, fn.optimizer = fn.optim, Compute.SE = TRUE)
    
    ld_gas <- as.numeric(fit@GASDyn$vLLK)
    write.csv(
      ld_gas,
      file = paste0("Log density/CCld_GAS_", name, "_", scaling, ".csv"),
      row.names = FALSE
    )
    
    rol_for <- UniGASRoll(
      ret_cc,
      spec,
      ForecastLength = n_test,
      RefitEvery = 3,
      RefitWindow = "moving",
      cluster = cluster,
      Compute.SE = FALSE
    )
    true_value <- tail(kernel_cov, n_test)
    
    forc_gas_ret <- as.numeric(rol_for@Forecast$Moments[, 1])
    write.csv(
      forc_gas_ret,
      file = paste0(
        "Forecasts/CCret_GAS_",
        name,
        "_",
        scaling,
        "_Forecast.csv"
      ),
      row.names = FALSE
    )
    
    forc_gas <- as.numeric(rol_for@Forecast$Moments[, 2])
    write.csv(
      forc_gas,
      file = paste0(
        "Forecasts/CCvol_GAS_",
        name,
        "_",
        scaling,
        "_Forecast.csv"
      ),
      row.names = FALSE
    )
    MAE_gas <- as.numeric(abs(forc_gas - true_value))
    write.csv(
      MAE_gas,
      file = paste0("MAE/CCvol_GAS_", name, "_", scaling, "_MAE.csv"),
      row.names = FALSE
    )
  }
}

################################################################################
################################################################################
## Produce MAE
# files <- list.files(path = "Forecasts", pattern = "^CCvol.*GAS", full.names = TRUE)
# for(file in files){
#   forc_gas <- read.csv(file)$x
#   MAE_gas <- as.numeric(abs(forc_gas - true_value))
#   name <- gsub('.*vol_(.*)_Forecast.*','\\1',file)
#   write.csv(
#     MAE_gas,
#     file = paste0("MAE/CCvol_", name, "_MAE.csv"),
#     row.names = FALSE
#   )
#
# }

## MSE simulation
for (name in c("std", "sstd", "ald", "ast")) {
  params <- c()
  spec <- UniGASSpec(
    Dist = name,
    ScalingType = "Identity",
    GASPar = list(
      location = FALSE,
      scale = TRUE,
      skewness = FALSE,
      shape = FALSE,
      shape2 = FALSE
    )
  )
  
  fit <-
    UniGASFit(spec, ret, fn.optimizer = fn.optim, Compute.SE = TRUE)
  
  for (i in 1:1000) {
    sim <- getObs(UniGASSim(fit = fit, T.sim = 100))
    fit_sim <-
      UniGASFit(spec, sim, fn.optimizer = fn.optim, Compute.SE = TRUE)
    params <-
      rbind(params, c(
        as.numeric(fit_sim@Estimates$lParList$vKappa),
        diag(fit_sim@Estimates$lParList$mA),
        diag(fit_sim@Estimates$lParList$mB)
      ))
  }
  params_fit <-
    c(
      as.numeric(fit@Estimates$lParList$vKappa),
      diag(fit@Estimates$lParList$mA),
      diag(fit@Estimates$lParList$mB)
    )
  
  means <- c()
  for (i in 1:ncol(params)) {
    means <- c(means, mean((params[, i] - params_fit[i]) ** 2))
  }
  write.csv(means, file = paste0("MSE/SIM_", name, "_MSE.csv"))
}


################################################################################
################################################################################
## 01/02/2023 -- SCRAP PAPER ####

spec <- UniGASSpec(
  Dist = "std",
  ScalingType = "Identity",
  GASPar = list(
    location = FALSE,
    scale = TRUE,
    skewness = FALSE,
    shape = FALSE,
    shape2 = FALSE
  )
)
fit <-
  UniGASFit(spec, ret_cc, fn.optimizer = fn.optim, Compute.SE = TRUE)
fit


n_test <- 252 * 2

rol_for <- UniGASRoll(
  ret_cc,
  spec,
  ForecastLength = n_test,
  RefitEvery = 3,
  RefitWindow = "moving",
  cluster = cluster,
  Compute.SE = FALSE
)

forc_gas <- as.numeric(rol_for@Forecast$Moments[, 2])

plot(kernel_cov[(length(kernel_cov) - n_test + 1):length(kernel_cov)],
     type = "l",
     xlab = "",
     ylab = "Vol")
lines(forc_gas, col = "red")


sim <- getObs(UniGASSim(fit = fit, T.sim = 10000))

fit_sim <-
  UniGASFit(spec, sim, fn.optimizer = fn.optim, Compute.SE = TRUE)

var <- getFilteredParameters(fit)[, "scale"] ** 2
plot(kernel_cov$V1, col = 'green', type = "l")
lines(var, type = "l", col =  'purple')
