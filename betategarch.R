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

# Out of sample forecast
predicts <- predict(est, n.ahead = 500)
plot(predicts, type = 'p')
lines(var(cc_data))

# Rolling window forecasts
rw_preds <- 0

rollapplyr(
  data = cc_data,
  width = length(cc_data) - 252,
  FUN = function(x) {
    ests <- tegarch(x)
    rw_preds <<- c(rw_preds, predict(ests, n.ahead = 1))
    print(length(rw_preds))
  }
)

plot(tail(kernel_data, 252), type = 'l')
lines(tail(rw_preds, 252), type = 'l', col='red')
