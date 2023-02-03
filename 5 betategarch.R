#install.packages('betategarch')
library(betategarch)
library(moments)
library(runner)
library(readr)
library(dplyr)
library(highfrequency)
library(xts)
library(TTR)
library(ggplot2)
library(TSstudio)
library(forecast)
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

# In sample parameter  estimates
est <- tegarch(oc_data)
print(est)

# Plotting kernel and esimated volatility
plot(kernel_data, type = 'l')
lines(exp(fitted(est)/2), col = 'red')
plot(fitted(est, verbose= TRUE), col = 'red')

# Extract the score
fit <- fitted(est, verbose= TRUE)
u <- fit$u
e <- fit$epsilon
plot(e, u)

# News Impact Curve 
df <- data.frame(obs = e, score = u)
ggplot(data=df, aes(x=obs, y=u), size = 2) +
  #ggtitle(label = 'News Impact Curve', subtitle = 'Score vs standardized return') +
  xlab('standardized return') +
  ylab('u') +
  geom_line()+
  geom_point()

legend(1500, 13, legend=c("Kernel", "Beta-t-EGARCH"),
       col=c("black", "red"), lty =1:2, cex=0.8)

# # Static window forecast h=504
# predicts <- predict(est, n.ahead = 504)
# plot(predicts, type = 'p')
# lines(var(cc_data))

# Rolling window 1-step ahead forecasts h=504
rw_forecast <- function(x, steps){
  rw_preds <- c()
  rollapplyr(
    data = x,
    width = length(x) - 504,
    FUN = function(data) {
      ests <- tegarch(data)
      rw_preds <<- c(rw_preds, predict(ests, n.ahead = steps))
      print(length(rw_preds))
    }
  )
  as.numeric(rw_preds)
}

# Open-to-close forecasts
rw_preds_oc <- rw_forecast(oc_data, 1) # forecasts for open-to-close data
plot(tail(kernel_data, 504), type = 'l')
lines(tail(rw_preds_oc, 504), type = 'l', col='red')

# Close-to-close forecasts
rw_preds_cc <- rw_forecast(cc_data, 1) # forecasts for close-to-close data
plot(tail(kernel_data, 504), type = 'l')
lines(tail(rw_preds_cc, 504), type = 'l', col='red')

# Save predictions as csv
write.csv(rw_preds_oc, file = "Forecasts/Open_to_close_TEGARCH_Forecast.csv", row.names = FALSE)
write.csv(rw_preds_cc, file = "Forecasts/Close_to_close_TEGARCH_Forecast.csv", row.names = FALSE)


# Calculate MAE 
MAE <- function(forc, true_value) {
  return(as.double(abs(forc-true_value)))
}

true_vol <- tail(kernel_data, 504)
MAE_oc <- MAE(tail(rw_preds_oc, 504), true_vol) # Open-to-close MAE
MAE_cc <- MAE(tail(rw_preds_cc, 504), true_vol) # Close-to-close MAE

# Saving MAE values
write.csv(MAE_oc, file = "MAE/Open_to_close_RealGARCH_MAE.csv", row.names = FALSE)
write.csv(MAE_cc, file = "MAE/Close_to_close_RealGARCH_MAE.csv", row.names = FALSE)

# Importing MAE from different models
MAE_rg <- read.csv("MAE/Open_to_close_RealGARCH_MAE.csv")$x
MAE_g <- read.csv("MAE/Open_to_close_GARCH_MAE.csv")$x
MAE_eg <- read.csv("MAE/Open_to_close_EGARCH_MAE.csv")$x
MAE_gjr <- read.csv("MAE/Open_to_close_GJRGARCH_MAE.csv")$x

# Open-to-close MAE Diebold Mariano test
dm.test(MAE_oc, MAE_rg, alternative = "two.sided", h = 1)
dm.test(MAE_oc, MAE_g, alternative = "two.sided", h = 1)
dm.test(MAE_oc, MAE_rg, alternative = "two.sided", h = 1)
dm.test(MAE_oc, MAE_rg, alternative = "two.sided", h = 1)

# Close-to-close MAE Diebold Mariano test
dm.test(MAE_cc, MAE_rg, alternative = "two.sided", h = 1)
dm.test(MAE_cc, MAE_g, alternative = "two.sided", h = 1)
dm.test(MAE_cc, MAE_rg, alternative = "two.sided", h = 1)
dm.test(MAE_cc, MAE_rg, alternative = "two.sided", h = 1)

