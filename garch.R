library(readr)
library(dplyr)
library(highfrequency)
library(xts)
library(TTR)
library(rugarch)
library(ggplot2)
library(TSstudio)
library(forecast)

# data <- read.csv(unz("Data/Disney.zip", "Disney.csv"), header=T)
# ## Remove first row, because it is empty
# data <- data[2:nrow(data),]
# data <- data %>%
#   mutate(DATE_TIME = paste(DATE, TIME)) %>%
#   mutate(DATE_TIME = as.POSIXct(DATE_TIME, format = "%Y/%m/%d %H:%M:%S")) %>%
#   mutate(PRICE = as.numeric(PRICE))
# ## Make open and close data
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
# ret_oc <- xts(100 *(as.numeric(log(ts_open)) - as.numeric(log(ts_close))), index(ts_close))
# 
# write.zoo(ret_close, file = "Close_to_close_log_returns.csv", col.names = TRUE)
# write.zoo(ret_oc, file = "Open_to_close_log_returns.csv", col.names = TRUE)

################################################################################
################################################################################

ret_close <- read.csv("Close_to_close_log_returns.csv", sep = " ")
ret_close$Index <- as.POSIXct(ret_close$Index,format="%Y-%m-%d %H:%M:%S")
ret_close <- xts(ret_close$V1, ret_close$Index)

ret_oc <- read.csv("Open_to_close_log_returns.csv", sep = " ")
ret_oc$Index <- as.POSIXct(ret_oc$Index,format="%Y-%m-%d %H:%M:%S")
ret_oc <- xts(ret_oc$V1, ret_oc$Index)

## Normal returns
# ret_close <- ROC(ts_close, n = 1)

plot(index(ret_close), ret_close, type = "l", xlab = "Time", ylab = "Open to Close Log Return")
plot(index(ret_oc), ret_oc, type = "l", xlab = "Time", ylab = "Close to Close Log Return")

################################################################################
################################################################################

kernel_cov <- kernel_cov * 10000

## General model
## We could also compare the performance between GARCH(1,1) and GARCH(1,2).
## We never try multiple order for p, otherwise we will have substantial multicollinearity
## Close-to-close
spec_rclose <- ugarchspec(mean.model = list(armaOrder = c(0, 0), include.mean = FALSE), variance.model = list(model = 'realGARCH', garchOrder = c(1, 1)), distribution.model = "sstd")
fit_rclose <- ugarchfit(spec_rclose, ret_close[2:length(ret_close)], solver = 'hybrid', realizedVol = kernel_cov[2:length(kernel_cov)])

spec_close <- ugarchspec(mean.model = list(armaOrder = c(0, 0), include.mean = FALSE), variance.model = list(model = 'sGARCH', garchOrder = c(1, 1)), distribution.model = "sstd")
fit_close <- ugarchfit(spec_close, ret_close[2:length(ret_close)], solver = 'hybrid')

## Open-to-close
spec_oc <- ugarchspec(mean.model = list(armaOrder = c(0, 0), include.mean = FALSE), variance.model = list(model = 'sGARCH', garchOrder = c(1, 1)), distribution.model = "sstd")
fit_oc <- ugarchfit(spec_oc, ret_oc[2:length(ret_oc)], solver = 'hybrid')

spec_roc <- ugarchspec(mean.model = list(armaOrder = c(0, 0), include.mean = FALSE), variance.model = list(model = 'realGARCH', garchOrder = c(1, 1)), distribution.model = "sstd")
fit_roc <- ugarchfit(spec_roc, ret_oc[2:length(ret_oc)], solver = 'hybrid', realizedVol = kernel_cov[2:length(kernel_cov)])


## Rolling window forecast with re-estimation
n_test <- 100
modelroll_close <- ugarchroll (
  spec=spec_close, data=ret_oc[2:length(ret_oc)], n.ahead = 1, forecast.length = n_test,
  refit.every = 1, refit.window = c("recursive"),
  solver = "hybrid", calculate.VaR = TRUE, VaR.alpha = c(0.01,0.05),
  cluster = NULL
)
modelroll_rclose <- ugarchroll (
  spec=spec_rclose, data=ret_oc[2:length(ret_oc)], n.ahead = 1, forecast.length = n_test,
  refit.every = 1, refit.window = c("recursive"),
  solver = "hybrid", calculate.VaR = TRUE, VaR.alpha = c(0.01,0.05),
  cluster = NULL, realizedVol = kernel_cov[2:length(kernel_cov)]
)
modelroll_oc <- ugarchroll (
  spec=spec_oc, data=ret_oc[2:length(ret_oc)], n.ahead = 1, forecast.length = n_test,
  refit.every = 1, refit.window = c("recursive"),
  solver = "hybrid", calculate.VaR = TRUE, VaR.alpha = c(0.01,0.05),
  cluster = NULL
)
modelroll_roc <- ugarchroll (
  spec=spec_roc, data=ret_oc[2:length(ret_oc)], n.ahead = 1, forecast.length = n_test,
  refit.every = 1, refit.window = c("recursive"),
  solver = "hybrid", calculate.VaR = TRUE, VaR.alpha = c(0.01,0.05),
  cluster = NULL, realizedVol = kernel_cov[2:length(kernel_cov)]
)


## Forecasts
true_value <- kernel_cov[(length(kernel_cov)-n_test+1):length(kernel_cov)]
forc_rclose <- modelroll_rclose@forecast$density[,"Sigma"]
forc_roc <- modelroll_roc@forecast$density[,"Sigma"]
forc_close <- modelroll_close@forecast$density[,"Sigma"]
forc_oc <- modelroll_oc@forecast$density[,"Sigma"]

factor <- var(ret_close[2:(length(ret_close)-n_test)])/mean(kernel_cov[2:(length(kernel_cov)-n_test)])
## Mean Absolute Value
MAE_close <- as.double(abs(forc_close - true_value%*% factor))
MAE_rclose <- as.double(abs(forc_rclose - true_value %*% factor))

MAE_roc <- as.double(abs(forc_roc - true_value))
MAE_oc <- as.double(abs(forc_oc - true_value))

## Diebold Mariano test

dm.test(MAE_close, MAE_rclose, alternative = "two.sided", h = 1)
## The p-value is not larger than 0.05, so we do not reject the null hypothesis of
## the models performing equally well. 

dm.test(MAE_oc, MAE_roc, alternative = "two.sided", h = 1)
## The p-value is not larger than 0.05, so we do not reject the null hypothesis of
## the models performing equally well.


################################################################################
################################################################################

