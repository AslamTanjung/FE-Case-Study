library(readr)
library(dplyr)
library(highfrequency)
library(xts)

data <- read.csv(unz("Data/Disney.zip", "Disney.csv"), header=T)
## Remove first row, because it is empty
data <- data[2:nrow(data),]
data <- data %>%
  mutate(DATE_TIME = paste(DATE, TIME)) %>%
  mutate(DATE_TIME = as.POSIXct(DATE_TIME, format = "%Y/%m/%d %H:%M:%S")) %>%
  mutate(PRICE = as.numeric(PRICE))

ts <- xts(data$PRICE, data$DATE_TIME)
#make returns
ts1 <- momentum(100 * log(ts), n = 1)
ts1 <- ts1[-1]

ts2 <- 100*makeReturns(ts)
ts2 <- ts2[-1]
## Realized kernel estimator
kernel_cov <- rKernelCov(ts, cor = FALSE, alignBy = "minutes", alignPeriod = 1, makeReturns = TRUE, kernelType = "Parzen")
write.zoo(kernel_cov*10000, file = "Returns & RV/RV_kernel.csv", col.names = TRUE)

kernel_cov2 <- rKernelCov(ts2, cor = FALSE, alignBy = "minutes", alignPeriod = 1, makeReturns = FALSE, kernelType = "Parzen")
write.zoo(kernel_cov2, file = "Returns & RV/RV_kernel2.csv", col.names = TRUE)


## Realized volatility
cov <- rCov(ts, cor = FALSE, alignBy = "minutes", alignPeriod = 1, makeReturns = TRUE)
write.zoo(cov*10000, file = "Returns & RV/RV.csv", col.names = TRUE)
## Realized 
plot(cov, col = 'red')
lines(kernel_cov, col = 'blue')

plot(kernel_cov-cov)




