#install.packages('betategarch')
library(betategarch)
library(moments)
library(runner)
kernel_data <- read.csv(file = 'Returns & RV/RV_kernel.csv', sep = "")$V1
cc_data <- read.csv(file = 'Returns & RV/Close_to_close_log_returns.csv', sep = "")$V1
oc_data <- read.csv(file = 'Returns & RV/Open_to_close_log_returns.csv', sep = "")$V1

sample_stat <- function(data) {
  stats <- c(mean(data), min(data), max(data), sd(data), skewness(data), kurtosis(data))
  print(cbind(c('mean.', 'min.', 'max.', 'std.', 'skew.', 'kurt.'), stats))
  }

cc_data <- na.omit(cc_data)
oc_data <- na.omit(oc_data)

sample_stat(cc_data)
sample_stat(oc_data)
#plot(abs(cc_data), type = 'l', ylab = 'returns')
# In sample parameter  estimates 
est <- tegarch(cc_data)
print(est)

# Plotting kernel and esimated volatility
plot(kernel_data, type = 'l')
lines(exp(fitted(est)/2), col = 'red')

legend(1500, 13, legend=c("Kernel", "Beta-t-EGARCH"),
       col=c("black", "red"), lty =1:2, cex=0.8)

# Static window forecast h=252
predicts <- predict(est, n.ahead = 252)
plot(predicts, type = 'p')
lines(var(cc_data))

# Rolling window 1-step ahead forecasts h=252
rw_forecast <- function(x, steps){
  rw_preds <- c()
  rollapplyr(
    data = x,
    width = length(x) - 252,
    FUN = function(data) {
      ests <- tegarch(data)
      rw_preds <<- c(rw_preds, predict(ests, n.ahead = steps))
      print(length(rw_preds))
    }
  )
  as.numeric(rw_preds)
}

rw_preds_oc <- rw_forecast(oc_data, 1) # forecasts for open-to-close data
plot(tail(kernel_data, 252), type = 'l')
lines(tail(rw_preds_oc, 252), type = 'l', col='red')

rw_preds_cc <- rw_forecast(cc_data, 1) # forecasts for close-to-close data
plot(tail(kernel_data, 252), type = 'l')
lines(tail(rw_preds_oc, 252), type = 'l', col='red')

# Save predictions as csv
write.csv(rw_preds_oc, file = "Forecasts/Open_to_close_TEGARCH_Forecast.csv", row.names = FALSE)
write.csv(rw_preds_cc, file = "Forecasts/Close_to_close_TEGARCH_Forecast.csv", row.names = FALSE)


# Calculate MAE 
MAE <- function(forc, true_value) {
  return(as.double(abs(forc-true_value)))
}

true_vol <- tail(kernel_data, 252)
MAE_oc <- MAE(tail(rw_preds_oc, 252), true_vol)
MAE_cc <- MAE(tail(rw_preds_cc, 252), true_vol)

write.csv(MAE_oc, file = "MAE/Open_to_close_RealGARCH_MAE.csv", row.names = FALSE)
write.csv(MAE_cc, file = "MAE/Close_to_close_RealGARCH_MAE.csv", row.names = FALSE)
