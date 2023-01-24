#install.packages('betategarch')
library(betategarch)

cc_data <- read.csv(file = 'Close_to_close_log_returns.csv', sep = "")$V1

cc_data
plot(abs(cc_data), type = 'l', ylab = 'returns')

est <- tegarch(cc_data)
print(est)
lines(fitted(est), col = 'red')

legend(1500, 13, legend=c("Abs. returns", "Beta-t-EGARCH"),
       col=c("black", "red"), lty =1:2, cex=0.8)
