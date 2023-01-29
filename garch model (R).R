library(readr)
library(dplyr)
library(highfrequency)
library(xts)
library(TTR)
library(rugarch)
library(ggplot2)
library(TSstudio)
library(forecast)

data <- read.csv(unz("Data/Disney.zip", "Disney.csv"), header=T)
## Remove first row, because it is empty
data <- data[2:nrow(data),]
data <- data %>%
  mutate(DATE_TIME = paste(DATE, TIME)) %>%
  mutate(DATE_TIME = as.POSIXct(DATE_TIME, format = "%Y/%m/%d %H:%M:%S")) %>%
  mutate(PRICE = as.numeric(PRICE))
## Make open and close data
data_open <- data %>%
  group_by(DATE) %>%
  slice_head(n = 1)

data_close <- data %>%
  group_by(DATE) %>%
  slice_tail(n = 1)

ts_open <- xts(data_open$PRICE, data_open$DATE_TIME)
ts_close <- xts(data_close$PRICE, data_close$DATE_TIME)

## Make returns
## LOG return
ret_close <- momentum(100 * log(ts_close), n = 1)

ret_oc <- xts(100 *(as.numeric(log(ts_close)) - as.numeric(log(ts_open))), index(ts_close))

write.zoo(ret_close, file = "Returns & RV/Close_to_close_log_returns.csv", col.names = TRUE)
write.zoo(ret_oc, file = "Returns & RV/Open_to_close_log_returns.csv", col.names = TRUE)

################################################################################
################################################################################

# ret_close <- read.csv("Returns & RV/Close_to_close_log_returns.csv", sep = " ")
# ret_close$Index <- as.POSIXct(ret_close$Index,format="%Y-%m-%d %H:%M:%S")
# ret_close <- xts(ret_close$V1, ret_close$Index)

ret_oc <- read.csv("Returns & RV/Open_to_close_log_returns.csv", sep = " ")
ret_oc$Index <- as.POSIXct(ret_oc$Index,format="%Y-%m-%d %H:%M:%S")
ret_oc <- xts(ret_oc$V1, ret_oc$Index)

kernel_cov <- read.csv("Returns & RV/RV_kernel.csv", sep = " ")
kernel_cov$Index <- as.POSIXct(kernel_cov$Index,format="%Y-%m-%d %H:%M:%S")
kernel_cov <- xts(kernel_cov$V1, kernel_cov$Index)

rv <- read.csv("Returns & RV/RV.csv", sep = " ")
rv$Index <- as.POSIXct(rv$Index,format="%Y-%m-%d %H:%M:%S")
rv <- xts(rv$V1, rv$Index)

# plot(index(ret_close), ret_close, type = "l", xlab = "Time", ylab = "Close to Close Log Return")
plot(index(ret_oc), ret_oc, type = "l", xlab = "Time", ylab = "Open to Close Log Return")

################################################################################
################################################################################

## General model
## We could also compare the performance between GARCH(1,1) and GARCH(1,2).
## We never try multiple order for p, otherwise we will have substantial multicollinearity
## Close-to-close
# spec_rclose <- ugarchspec(mean.model = list(armaOrder = c(0, 0), include.mean = FALSE), variance.model = list(model = 'realGARCH', garchOrder = c(1, 1)), distribution.model = "sstd")
# fit_rclose <- ugarchfit(spec_rclose, ret_close[2:length(ret_close)], solver = 'hybrid', realizedVol = kernel_cov[2:length(kernel_cov)])
# 
# spec_close <- ugarchspec(mean.model = list(armaOrder = c(0, 0), include.mean = FALSE), variance.model = list(model = 'sGARCH', garchOrder = c(1, 1)), distribution.model = "sstd")
# fit_close <- ugarchfit(spec_close, ret_close[2:length(ret_close)], solver = 'hybrid')

## Model specification with Student t distribution (maybe  also with sstd to compare?)
spec_oc <- ugarchspec(mean.model = list(armaOrder = c(0, 0), include.mean = FALSE), variance.model = list(model = 'sGARCH', garchOrder = c(1, 1)), distribution.model = "sstd")
spec_roc <- ugarchspec(mean.model = list(armaOrder = c(0, 0), include.mean = FALSE), variance.model = list(model = 'realGARCH', garchOrder = c(1, 1)), distribution.model = "std")
spec_e <- ugarchspec(mean.model = list(armaOrder = c(0, 0), include.mean = FALSE), variance.model = list(model = 'eGARCH', garchOrder = c(1, 1)), distribution.model = "std")
spec_gjr <- ugarchspec(mean.model = list(armaOrder = c(0, 0), include.mean = FALSE), variance.model = list(model = 'gjrGARCH', garchOrder = c(1, 1)), distribution.model = "std")

#Open to close fit 
fit_oc <- ugarchfit(spec = spec_oc, data = ret_oc[2:length(ret_oc)], solver = 'hybrid')
fit_roc <- ugarchfit(spec = spec_roc, data = ret_oc[2:length(ret_oc)], solver = 'hybrid', realizedVol = sqrt(kernel_cov[2:length(kernel_cov)]))
fit_e <- ugarchfit(spec = spec_e, data = ret_oc[2:length(ret_oc)], solver = 'hybrid')
fit_gjr <- ugarchfit(spec = spec_gjr, data = ret_oc[2:length(ret_oc)], solver = 'hybrid')

#output of, parameters estimates and other statistics, open to close Garch models
fit_oc
fit_roc
fit_e
fit_gjr

#useful plots of open to close Garch models
plot(fit_oc, which= 'all')
plot(fit_roc, which= 'all')
plot(fit_e, which= 'all')
plot(fit_gjr, which= 'all')

## Forecasts
## Because our goal is to produce models with accurate forecasts, we evaluate
## the quality of the models by looking at forecast prediction accuracy instead of
## widely used likelihood statistics, such as AIC and BIC.
# forc_close <- modelroll_close@forecast$density[,"Sigma"]
# forc_rclose <- modelroll_rclose@forecast$density[,"Sigma"]

## Rolling window forecast with re-estimation
#we use 5 years as in sample, 2 years as out of sample
n_test <- 252*2
true_value <- kernel_cov[(length(kernel_cov)-n_test+1):length(kernel_cov)]

# #Close-to-close forecasts
# modelroll_close <- ugarchroll (
#   spec=spec_close, data=ret_oc[2:length(ret_close)], n.ahead = 1, forecast.length = n_test,
#   refit.every = 1, refit.window = c("moving"),
#   solver = "hybrid", calculate.VaR = TRUE, VaR.alpha = c(0.01,0.05)
# )
# modelroll_rclose <- ugarchroll (
#   spec=spec_rclose, data=ret_oc[2:length(ret_close)], n.ahead = 1, forecast.length = n_test,
#   refit.every = 1, refit.window = c("moving"),
#   solver = "hybrid", calculate.VaR = TRUE, VaR.alpha = c(0.01,0.05),
#   realizedVol = kernel_cov[2:length(kernel_cov)]
# )

#Open-to-close forecasts
modelroll_oc <- ugarchroll (
  spec=spec_oc, data=ret_oc[2:length(ret_oc)], n.ahead = 1, forecast.length = n_test,
  refit.every = 3, refit.window = c("moving"),
  solver = "hybrid", calculate.VaR = TRUE, VaR.alpha = c(0.01,0.05)
)
modelroll_roc <- ugarchroll (
  spec=spec_roc, data=ret_oc[2:length(ret_oc)], n.ahead = 1, forecast.length = n_test,
  refit.every = 3, refit.window = c("moving"),
  solver = "hybrid", calculate.VaR = TRUE, VaR.alpha = c(0.01,0.05),
  realizedVol = kernel_cov[2:length(kernel_cov)]
)
modelroll_e <- ugarchroll (
  spec=spec_e, data=ret_oc[2:length(ret_oc)], n.ahead = 1, forecast.length = n_test,
  refit.every = 3, refit.window = c("moving"),
  solver = "hybrid", calculate.VaR = TRUE, VaR.alpha = c(0.01,0.05)
)
modelroll_gjr <- ugarchroll (
  spec=spec_gjr, data=ret_oc[2:length(ret_oc)], n.ahead = 1, forecast.length = n_test,
  refit.every = 3, refit.window = c("moving"),
  solver = "hybrid", calculate.VaR = TRUE, VaR.alpha = c(0.01,0.05)
)


#Return forecasts
forcret_roc <- modelroll_roc@forecast$density[,"Mu"]
write.csv(forcret_roc, file = "Forecasts/OCret_RealGARCH_Forecast.csv", row.names = FALSE)
forcret_oc <- modelroll_oc@forecast$density[,"Mu"]
write.csv(forcret_oc, file = "Forecasts/OCret_GARCH_Forecast.csv", row.names = FALSE)
forcret_gjr <- modelroll_gjr@forecast$density[,"Mu"]
write.csv(forcret_gjr, file = "Forecasts/OCret_GJRGARCH_Forecast.csv", row.names = FALSE)
forcret_e <- modelroll_e@forecast$density[,"Mu"]
write.csv(forcret_e, file = "Forecasts/OCret_EGARCH_Forecast.csv", row.names = FALSE)

#Volatility forecasts
forc_roc <- modelroll_roc@forecast$density[,"Sigma"]
write.csv(forc_roc, file = "Forecasts/OCvol_RealGARCH_Forecast.csv", row.names = FALSE)
forc_oc <- modelroll_oc@forecast$density[,"Sigma"]
write.csv(forc_oc, file = "Forecasts/OCvol_GARCH_Forecast.csv", row.names = FALSE)
forc_gjr <- modelroll_gjr@forecast$density[,"Sigma"]
write.csv(forc_gjr, file = "Forecasts/OCvol_GJRGARCH_Forecast.csv", row.names = FALSE)
forc_e <- modelroll_e@forecast$density[,"Sigma"]
write.csv(forc_e, file = "Forecasts/OCvol_EGARCH_Forecast.csv", row.names = FALSE)

# factor <- var(ret_close[2:(length(ret_close)-n_test)])/mean(kernel_cov[2:(length(kernel_cov)-n_test)])
## Mean Absolute Value
# MAE_close <- as.double(abs(forc_close - true_value%*% factor))
# MAE_rclose <- as.double(abs(forc_rclose - true_value %*% factor))

MAE <- function(forc, true_value) {
  return(as.double(abs(forc-true_value)))
}

# MSE <- function(forc, true_value) {
#   return(as.double((forc-true_value)**2))
# }
MAE_roc <- MAE(forc_roc, true_value)
write.csv(MAE_roc, file = "MAE/OCvol_RealGARCH_MAE.csv", row.names = FALSE)
MAE_oc <- MAE(forc_oc, true_value)
write.csv(MAE_oc, file = "MAE/OCvol_GARCH_MAE.csv", row.names = FALSE)
MAE_gjr <- MAE(forc_gjr, true_value)
write.csv(MAE_gjr, file = "MAE/OCvol_GJRGARCH_MAE.csv", row.names = FALSE)
MAE_e <- MAE(forc_e, true_value)
write.csv(MAE_e, file = "MAE/OCvol_EGARCH_MAE.csv", row.names = FALSE)

################################################################################
################################################################################
## INLEZEN #####################################################################

MAE_roc <- read.csv("MAE/OCvol_RealGARCH_MAE.csv")$x
MAE_oc <- read.csv("MAE/OCvol_GARCH_MAE.csv")$x
MAE_e <- read.csv("MAE/OCvol_EGARCH_MAE.csv")$x
MAE_gjr <- read.csv("MAE/OCvol_GJRGARCH_MAE.csv")$x
forc_roc <- read.csv("Forecasts/OCvol_RealGARCH_Forecast.csv")$x
forc_oc <- read.csv("Forecasts/OCvol_GARCH_Forecast.csv")$x
forc_e <- read.csv("Forecasts/OCvol_EGARCH_Forecast.csv")$x
forc_gjr <- read.csv("Forecasts/OCvol_GJRGARCH_Forecast.csv")$x

################################################################################
################################################################################
## Diebold-Mariano #############################################################

#dm.test(MAE_close, MAE_rclose, alternative = "two.sided", h = 1)
dm.test(MAE_oc, MAE_roc, alternative = "two.sided", h = 1)
dm.test(MAE_roc, MAE_e, alternative = "two.sided", h = 1)
dm.test(MAE_oc, MAE_e, alternative = "two.sided", h = 1)
dm.test(MAE_roc, MAE_gjr, alternative = "two.sided", h = 1)
dm.test(MAE_oc, MAE_gjr, alternative = "two.sided", h = 1)
dm.test(MAE_e, MAE_gjr, alternative= "two.sided", h = 1)

################################################################################
################################################################################
## Choosing GARCH
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



plot(as.numeric(true_value), type = 'l', col = 'blue', xlim = c(0,100))
lines(forc_e, col = 'red')
lines(forc_oc, col = 'green')
lines(forc_roc, col = 'purple')
lines(forc_gjr, col = 'black')
lines(forc_gas, col = 'brown')
legend("topleft", legend = c("E", "TRUE", "GARCH", 'real', 'GJR', 'GAS'), col = c('red', 'blue', 'green', 'purple', 'black', 'brown'), lty=1)
