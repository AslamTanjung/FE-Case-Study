library(readr)
library(dplyr)
library(highfrequency)
library(xts)
library(TTR)
library(rugarch)
library(ggplot2)
library(TSstudio)
library(forecast)

## Here we manipulate the data
# #Load in data
# data <- read.csv(unz("Data/Disney.zip", "Disney.csv"), header=T)
# # Remove first row, because it is empty
# data <- data[2:nrow(data),]
# data <- data %>%
#   mutate(DATE_TIME = paste(DATE, TIME)) %>%
#   mutate(DATE_TIME = as.POSIXct(DATE_TIME, format = "%Y/%m/%d %H:%M:%S")) %>%
#   mutate(PRICE = as.numeric(PRICE))
# # Make open and close data
# data_open <- data %>%
#   group_by(DATE) %>%
#   slice_head(n = 1)
# 
# data_close <- data %>%
#   group_by(DATE) %>%
#   slice_tail(n = 1)
# 
# ts_open <- xts(data_open$PRICE, data_open$DATE_TIME)
# ts_close <- xts(data_close$PRICE, data_close$DATE_TIME)
# 
# ## Make returns
# ## LOG return
# ret_close <- momentum(100 * log(ts_close), n = 1)
# 
# ret_oc <- xts(100 *(as.numeric(log(ts_close)) - as.numeric(log(ts_open))), index(ts_close))
# 
# write.zoo(ret_close, file = "Returns & RV/Close_to_close_log_returns.csv", col.names = TRUE)
# write.zoo(ret_oc, file = "Returns & RV/Open_to_close_log_returns.csv", col.names = TRUE)

################################################################################
################################################################################
#can skip all above and immediately read in the return & Realized Kernel files
# Read in the returns and realized kernel
ret_close <- read.csv("Returns & RV/Close_to_close_log_returns.csv", sep = " ")
ret_close$Index <- as.POSIXct(ret_close$Index,format="%Y-%m-%d %H:%M:%S")
ret_close <- xts(ret_close$V1, ret_close$Index)

ret_oc <- read.csv("Returns & RV/Open_to_close_log_returns.csv", sep = " ")
ret_oc$Index <- as.POSIXct(ret_oc$Index,format="%Y-%m-%d %H:%M:%S")
ret_oc <- xts(ret_oc$V1, ret_oc$Index)

kernel_cov <- read.csv("Returns & RV/RV_kernel2.csv", sep = " ")
kernel_cov$Index <- as.POSIXct(kernel_cov$Index,format="%Y-%m-%d %H:%M:%S")
kernel_cov <- xts(kernel_cov$V1, kernel_cov$Index)

plot(kernel_cov)

rv <- read.csv("Returns & RV/RV2.csv", sep = " ")
rv$Index <- as.POSIXct(rv$Index,format="%Y-%m-%d %H:%M:%S")
rv <- xts(rv$V1, rv$Index)

plot(index(ret_close), ret_close, type = "l", xlab = "Time", ylab = "Close to Close Log Return")
plot(index(ret_oc), ret_oc, type = "l", xlab = "Time", ylab = "Open to Close Log Return")

################################################################################
################################################################################
#modelling various GARCH(1,1) models
## Model specification with Student t distribution: 
spec_oc <- ugarchspec(mean.model = list(armaOrder = c(0, 0), include.mean = FALSE), variance.model = list(model = 'sGARCH', garchOrder = c(1, 1)), distribution.model = "sstd")
spec_roc <- ugarchspec(mean.model = list(armaOrder = c(0, 0), include.mean = FALSE), variance.model = list(model = 'realGARCH', garchOrder = c(1, 1)), distribution.model = "std")
spec_e <- ugarchspec(mean.model = list(armaOrder = c(0, 0), include.mean = FALSE), variance.model = list(model = 'eGARCH', garchOrder = c(1, 1)), distribution.model = "std")
spec_gjr <- ugarchspec(mean.model = list(armaOrder = c(0, 0), include.mean = FALSE), variance.model = list(model = 'gjrGARCH', garchOrder = c(1, 1)), distribution.model = "std")

## Open to close fitting the data
fit_oc <- ugarchfit(spec = spec_oc, data = ret_oc[2:length(ret_oc)], solver = 'hybrid')
fit_r_oc <- ugarchfit(spec = spec_roc, data = ret_oc[2:length(ret_oc)], solver = 'hybrid', realizedVol = kernel_cov[2:length(kernel_cov)])
fit_e_oc <- ugarchfit(spec = spec_e, data = ret_oc[2:length(ret_oc)], solver = 'hybrid')
fit_gjr_oc <- ugarchfit(spec = spec_gjr, data = ret_oc[2:length(ret_oc)], solver = 'hybrid')

Logl = student_t_log_likelihood(ret_oc[2:length(ret_oc)], uncmean(fit_r_oc), sigma(fit_r_oc), coef(fit_r_oc)[8])
AIC = 2 * length(coef(fit_r_oc)) - 2 * Logl
BIC = length(coef(fit_r_oc)) * log(length(ret_oc[2:length(ret_oc)])) - 2 * Logl

#Close to close fit 
fit_cc <- ugarchfit(spec = spec_oc, data = ret_close[2:length(ret_close)], solver = 'hybrid')
fit_r_cc <- ugarchfit(spec = spec_roc, data = ret_close[2:length(ret_close)], solver = 'hybrid', realizedVol = kernel_cov[2:length(kernel_cov)])
fit_e_cc <- ugarchfit(spec = spec_e, data = ret_close[2:length(ret_close)], solver = 'hybrid')
fit_gjr_cc <- ugarchfit(spec = spec_gjr, data = ret_close[2:length(ret_close)], solver = 'hybrid')

################################################################################
################################################################################
## Partial loglikelihood
Logl = student_t_log_likelihood(ret_oc[2:length(ret_close)], uncmean(fit_r_cc), sigma(fit_r_cc), coef(fit_r_cc)[8])
AIC = 2 * length(coef(fit_r_cc)) - 2 * Logl
BIC = length(coef(fit_r_cc)) * log(length(ret_close[2:length(ret_close)])) - 2 * Logl

student_t_log_likelihood <- function(y, mu, sigma, nu) {
  d <- ncol(y)
  t <- (1 + (1/nu)*rowSums((y-mu)^2/sigma^2))^(-(nu+d)/2)
  log_l <- lgamma((nu+d)/2) - log(sqrt(pi*nu)) - lgamma(nu/2) - log(sigma) - (nu+d)/2 * log(1+(1/nu)*rowSums((y-mu)^2/sigma^2))
  return(sum(log_l))
}

#output of, parameters estimates and other statistics, open to close Garch models
fit_oc
fit_r_oc
fit_e_oc
fit_gjr_oc

#output of, parameters estimates and other statistics, Close to close Garch models
fit_cc
fit_r_cc
fit_e_cc
fit_gjr_cc

#useful plots of open to close Garch models
plot(fit_oc, which= 'all')
plot(fit_r_oc, which= 'all')
plot(fit_e_oc, which= 'all')
plot(fit_gjr_oc, which= 'all')

#useful plots of Close to Close Garch models
plot(fit_cc, which= 'all')
plot(fit_r_cc, which= 'all')
plot(fit_e_cc, which= 'all')
plot(fit_gjr_cc, which= 'all')

## Forecasts
## Because our goal is to produce models with accurate forecasts, we evaluate
## the quality of the models by looking at forecast prediction accuracy instead of
## widely used likelihood statistics, such as AIC and BIC.

## Rolling moving window forecast with re-estimation
#we use 5 years as in-sample, 2 years as out of sample
n_test <- 252*2
true_value <- kernel_cov[(length(kernel_cov)-n_test+1):length(kernel_cov)]

#Open-to-close forecasts
modelroll_oc <- ugarchroll (
  spec=spec_oc, data=ret_oc[2:length(ret_oc)], n.ahead = 1, forecast.length = n_test,
  refit.every = 3, refit.window = c("moving"),
  solver = "hybrid", calculate.VaR = TRUE, VaR.alpha = c(0.01,0.05)
)
modelroll_r_oc <- ugarchroll (
  spec=spec_roc, data=ret_oc[2:length(ret_oc)], n.ahead = 1, forecast.length = n_test,
  refit.every = 3, refit.window = c("moving"),
  solver = "hybrid", calculate.VaR = TRUE, VaR.alpha = c(0.01,0.05),
  realizedVol = kernel_cov[2:length(kernel_cov)]
)
modelroll_e_oc <- ugarchroll (
  spec=spec_e, data=ret_oc[2:length(ret_oc)], n.ahead = 1, forecast.length = n_test,
  refit.every = 3, refit.window = c("moving"),
  solver = "hybrid", calculate.VaR = TRUE, VaR.alpha = c(0.01,0.05)
)
modelroll_gjr_oc <- ugarchroll (
  spec=spec_gjr, data=ret_oc[2:length(ret_oc)], n.ahead = 1, forecast.length = n_test,
  refit.every = 3, refit.window = c("moving"),
  solver = "hybrid", calculate.VaR = TRUE, VaR.alpha = c(0.01,0.05)
)

#Return forecasts Open to close
forcret_r_oc <- modelroll_r_oc@forecast$density[,"Mu"]
write.csv(forcret_r_oc, file = "Forecasts/OCret_RealGARCH_Forecast.csv", row.names = FALSE)
forcret_oc <- modelroll_oc@forecast$density[,"Mu"]
write.csv(forcret_oc, file = "Forecasts/OCret_GARCH_Forecast.csv", row.names = FALSE)
forcret_gjr_oc <- modelroll_gjr_oc@forecast$density[,"Mu"]
write.csv(forcret_gjr_oc, file = "Forecasts/OCret_GJRGARCH_Forecast.csv", row.names = FALSE)
forcret_e_oc <- modelroll_e_oc@forecast$density[,"Mu"]
write.csv(forcret_e_oc, file = "Forecasts/OCret_EGARCH_Forecast.csv", row.names = FALSE)

#Volatility forecasts Open to Close Volatility
forc_r_oc <- modelroll_r_oc@forecast$density[,"Sigma"]
write.csv(forc_r_oc, file = "Forecasts/OCvol_RealGARCH_Forecast.csv", row.names = FALSE)
forc_oc <- modelroll_oc@forecast$density[,"Sigma"]
write.csv(forc_oc, file = "Forecasts/OCvol_GARCH_Forecast.csv", row.names = FALSE)
forc_gjr_oc <- modelroll_gjr_oc@forecast$density[,"Sigma"]
write.csv(forc_gjr_oc, file = "Forecasts/OCvol_GJRGARCH_Forecast.csv", row.names = FALSE)
forc_e_oc <- modelroll_e_oc@forecast$density[,"Sigma"]
write.csv(forc_e_oc, file = "Forecasts/OCvol_EGARCH_Forecast.csv", row.names = FALSE)

MAE <- function(forc, true_value) {
  return(as.double(abs(forc-true_value)))
}

MAE_roc <- MAE(forc_roc**2, true_value)
write.csv(MAE_roc, file = "MAE/OCvol_RealGARCH_MAE.csv", row.names = FALSE)
MAE_oc <- MAE(forc_oc**2, true_value)
write.csv(MAE_oc, file = "MAE/OCvol_GARCH_MAE.csv", row.names = FALSE)
MAE_gjr <- MAE(forc_gjr**2, true_value)
write.csv(MAE_gjr, file = "MAE/OCvol_GJRGARCH_MAE.csv", row.names = FALSE)
MAE_e <- MAE(forc_e**2, true_value)
write.csv(MAE_e, file = "MAE/OCvol_EGARCH_MAE.csv", row.names = FALSE)

################################################################################
## Choosing GARCH(1,1) or GARCH(1,2)
## Open-and-close
spec_rchoose2 <- ugarchspec(mean.model = list(armaOrder = c(0, 0), include.mean = FALSE), variance.model = list(model = 'realGARCH', garchOrder = c(1, 2)), distribution.model = "sstd")
modelroll_rchoose2 <- ugarchroll (
  spec=spec_rchoose2, data=ret_oc[2:length(ret_oc)], n.ahead = 1, forecast.length = n_test,
  refit.every = 1, refit.window = c("recursive"),
  solver = "hybrid", calculate.VaR = TRUE, VaR.alpha = c(0.01,0.05),
  realizedVol = kernel_cov[2:length(kernel_cov)]
)

spec_rchoose1 <- ugarchspec(mean.model = list(armaOrder = c(0, 0), include.mean = FALSE), variance.model = list(model = 'realGARCH', garchOrder = c(1, 1)), distribution.model = "sstd")
modelroll_rchoose1 <- ugarchroll (
  spec=spec_rchoose1, data=ret_oc[2:length(ret_oc)], n.ahead = 1, forecast.length = n_test,
  refit.every = 1, refit.window = c("recursive"),
  solver = "hybrid", calculate.VaR = TRUE, VaR.alpha = c(0.01,0.05),
  realizedVol = kernel_cov[2:length(kernel_cov)]
)

forc_rchoose1 <- modelroll_rchoose1@forecast$density[,"Sigma"]
forc_rchoose2 <- modelroll_rchoose2@forecast$density[,"Sigma"]

MAE_rchoose1 <- as.double(abs(forc_rchoose1 - true_value))
MAE_rchoose2 <- as.double(abs(forc_rchoose2 - true_value))

dm.test(MAE_rchoose1, MAE_rchoose2, alternative = "two.sided", h = 1)
## The p-value is 0.4053 and the test statistic is -0.83353. There is not enough evidence
## to reject the null hypothesis that the models perform equally well. Thus we choose
## to continue with GARCH(1,1) for parsimionity.

################################################################################
################################################################################
########### Close-to-close #################################################
#Close-to-close forecasts

modelroll_cc <- ugarchroll (
  spec=spec_oc, data=ret_close[2:length(ret_close)], n.ahead = 1, forecast.length = n_test,
  refit.every = 3, refit.window = c("moving"),
  solver = "hybrid", calculate.VaR = TRUE, VaR.alpha = c(0.01,0.05)
)
modelroll_r_cc <- ugarchroll (
  spec=spec_roc, data=ret_close[2:length(ret_close)], n.ahead = 1, forecast.length = n_test,
  refit.every = 3, refit.window = c("moving"),
  solver = "hybrid", calculate.VaR = TRUE, VaR.alpha = c(0.01,0.05),
  realizedVol = kernel_cov[2:length(kernel_cov)]
)
modelroll_e_cc <- ugarchroll (
  spec=spec_e, data=ret_close[2:length(ret_close)], n.ahead = 1, forecast.length = n_test,
  refit.every = 3, refit.window = c("moving"),
  solver = "hybrid", calculate.VaR = TRUE, VaR.alpha = c(0.01,0.05)
)
modelroll_gjr_cc <- ugarchroll (
  spec=spec_gjr, data=ret_close[2:length(ret_close)], n.ahead = 1, forecast.length = n_test,
  refit.every = 3, refit.window = c("moving"),
  solver = "hybrid", calculate.VaR = TRUE, VaR.alpha = c(0.01,0.05)
)

#Return forecasts
forcret_r_cc <- modelroll_r_cc@forecast$density[,"Mu"]
write.csv(forcret_r_cc, file = "Forecasts/CCret_RealGARCH_Forecast.csv", row.names = FALSE)
forcret_cc <- modelroll_cc@forecast$density[,"Mu"]
write.csv(forcret_cc, file = "Forecasts/CCret_GARCH_Forecast.csv", row.names = FALSE)
forcret_gjr_cc <- modelroll_gjr_cc@forecast$density[,"Mu"]
write.csv(forcret_gjr_cc, file = "Forecasts/CCret_GJRGARCH_Forecast.csv", row.names = FALSE)
forcret_e_cc <- modelroll_e_cc@forecast$density[,"Mu"]
write.csv(forcret_e_cc, file = "Forecasts/CCret_EGARCH_Forecast.csv", row.names = FALSE)

#Volatility forecasts
forc_r_cc <- modelroll_r_cc@forecast$density[,"Sigma"]
write.csv(forc_r_cc, file = "Forecasts/CCvol_RealGARCH_Forecast.csv", row.names = FALSE)
forc_cc <- modelroll_cc@forecast$density[,"Sigma"]
write.csv(forc_cc, file = "Forecasts/CCvol_GARCH_Forecast.csv", row.names = FALSE)
forc_gjr_cc <- modelroll_gjr@forecast$density[,"Sigma"]
write.csv(forc_gjr_cc, file = "Forecasts/CCvol_GJRGARCH_Forecast.csv", row.names = FALSE)
forc_e_cc <- modelroll_e@forecast$density[,"Sigma"]
write.csv(forc_e_cc, file = "Forecasts/CCvol_EGARCH_Forecast.csv", row.names = FALSE)

factor <- var(ret_close[2:(length(ret_close)-n_test)])/mean(kernel_cov[2:(length(kernel_cov)-n_test)])

MAE <- function(forc, true_value) {
  return(as.double(abs(forc-true_value)))
}

MAE_r_cc <- MAE(forc_r_cc, true_value)
write.csv(MAE_r_cc, file = "MAE/CCvol_RealGARCH_MAE.csv", row.names = FALSE)
MAE_cc <- MAE(forc_cc, true_value)
write.csv(MAE_cc, file = "MAE/CCvol_GARCH_MAE.csv", row.names = FALSE)
MAE_gjr_cc <- MAE(forc_gjr_cc, true_value)
write.csv(MAE_gjr_cc, file = "MAE/CCvol_GJRGARCH_MAE.csv", row.names = FALSE)
MAE_e_cc <- MAE(forc_e_cc, true_value)
write.csv(MAE_e, file = "MAE/CCvol_EGARCH_MAE.csv", row.names = FALSE)

################################################################################
################################################################################
## Simulate voor MSE
## Failed simulation study

for(name in c("sGARCH", "eGARCH", "gjrGARCH", "realGARCH")){
  params <- c()
  spec <- ugarchspec(mean.model = list(armaOrder = c(0, 0), include.mean = FALSE), variance.model = list(model = name,garchOrder = c(1, 1)), distribution.model = "std")
  fit <- ugarchfit(spec = spec, data = ret_oc[2:length(ret_oc)], solver = 'hybrid', realizedVol = kernel_cov[2:length(kernel_cov)])
  
  for (i in 1:1000) {
    simul <- ugarchsim(fit = fit, n.sim = 2000, rseed = 0)
    sim <- xts(as.numeric(fitted(simul)), order.by = as.Date(1:2000))
    if(name == "realGARCH"){
      rv = xts(as.numeric(sigma(simul)), order.by = as.Date(1:2000))
    } else {
      rv = NULL
    }
    fit_sim <- ugarchfit(spec = spec, data = sim, realizedVol = rv)
    if(name == "realGARCH"){
      params <- rbind(params, as.numeric(fit_sim@model$pars[c("omega", "alpha1", "beta1", "eta11", "eta21", "delta", "lambda", "shape"),1]))
    } else{
    params <-
      rbind(params, as.numeric(fit_sim@model$pars[c("omega", "alpha1", "beta1", "shape"),1]))
    }
  }
  
  if(name == "realGARCH"){
    params_fit <- as.numeric(fit@model$pars[c("omega", "alpha1", "beta1", "eta11", "eta21", "delta", "lambda", "shape"),1])
  } else {
  params_fit <-
    as.numeric(fit@model$pars[c("omega", "alpha1", "beta1", "shape"),1])
  }
  
  means <- c()
  for (i in 1:ncol(params)) {
    means <- c(means, mean((params[, i] - params_fit[i]) ** 2))
  }
  write.csv(means, file = paste0("MSE/SIM_", name, "_MSE.csv"))
  
}