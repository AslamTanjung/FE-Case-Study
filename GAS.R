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

n_test <- 252 * 2
kernel_cov <- read.csv("Returns & RV/RV_kernel.csv", sep = " ")
kernel_cov$Index <-
  as.POSIXct(kernel_cov$Index, format = "%Y-%m-%d %H:%M:%S")
kernel_cov <- tail(as.numeric(xts(kernel_cov$V1, kernel_cov$Index)), n_test)

#kernel_cov <- as.numeric(xts(kernel_cov$V1, kernel_cov$Index))
ret <-
  read.csv(file = "Returns & RV/Open_to_close_log_returns.csv", sep = " ")
ret$Index <- as.POSIXct(ret$Index, format = "%Y-%m-%d %H:%M:%S")
ret <- xts(ret$V1, ret$Index)


for (name in c(
  "std",
  "sstd",
  "ast",
  "ald"
)){
  for(scaling in c("Identity")) {
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
    file = paste0("Forecasts/OCret_GAS_", name, "_", scaling, "_Forecast.csv"),
    row.names = FALSE
  )

  forc_gas <- as.numeric(rol_for@Forecast$Moments[, 2])
  write.csv(
    forc_gas,
    file = paste0("Forecasts/OCvol_GAS_", name, "_", scaling, "_Forecast.csv"),
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
# dev.off()
# plot(as.numeric(kernel_cov), type = "l", xlab = "", ylab = "Vol")
# lines(as.numeric(forc_gas), col = "red")
# legend(x = 'topleft', legend = name, col = "red", lty = 1)

## Close-to-close
ret_cc <-
  read.csv(file = "Returns & RV/Close_to_close_log_returns.csv", sep = " ")
ret_cc$Index <- as.POSIXct(ret_cc$Index, format = "%Y-%m-%d %H:%M:%S")
ret_cc <- xts(ret_cc$V1, ret_cc$Index)
ret_cc <- ret_cc[2:length(ret_cc),]

n_test <- 252 * 2
for (name in c(
  "std",
  "sstd",
  "ast",
  "ald"
)){
  for(scaling in c("Identity")) {
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
      file = paste0("Forecasts/CCret_GAS_", name, "_", scaling, "_Forecast.csv"),
      row.names = FALSE
    )
    
    forc_gas <- as.numeric(rol_for@Forecast$Moments[,2])
    write.csv(
      forc_gas,
      file = paste0("Forecasts/CCvol_GAS_", name, "_", scaling, "_Forecast.csv"),
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

# MAE_roc <- read.csv("MAE/Open_to_close_RealGARCH_MAE.csv")$x
# MAE_oc <- read.csv("MAE/Open_to_close_GARCH_MAE.csv")$x
# MAE_e <- read.csv("MAE/Open_to_close_EGARCH_MAE.csv")$x
# MAE_gjr <- read.csv("MAE/Open_to_close_GJRGARCH_MAE.csv")$x
# 
# dm.test(MAE_gas_std, MAE_roc, alternative = "two.sided", h = 1)
# dm.test(MAE_oc, MAE_roc, alternative = "two.sided", h = 1)



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

