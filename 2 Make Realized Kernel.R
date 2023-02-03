library(readr)
library(dplyr)
library(highfrequency)
library(xts)

## Code for making the realized kernel

data <- read.csv(unz("Data/Disney.zip", "Disney.csv"), header=T)
## Remove first row, because it is empty
data <- data[2:nrow(data),]
data <- data %>%
  mutate(DATE_TIME = paste(DATE, TIME)) %>%
  mutate(DATE_TIME = as.POSIXct(DATE_TIME, format = "%Y/%m/%d %H:%M:%S")) %>%
  mutate(PRICE = as.numeric(PRICE))

ts <- xts(data$PRICE, data$DATE_TIME)

#make log returns and multiply by 100
ts2 <- 100*makeReturns(ts)
ts2 <- ts2[-1]

##### Realized kernel estimator #####
#old method
kernel_cov <- rKernelCov(ts, cor = FALSE, alignBy = "minutes", alignPeriod = 1, makeReturns = TRUE, kernelType = "Parzen")
write.zoo(kernel_cov*10000, file = "Returns & RV/RV_kernel.csv", col.names = TRUE)

#new method
kernel_cov2 <- rKernelCov(ts2, cor = FALSE, alignBy = "minutes", alignPeriod = 1, makeReturns = FALSE, kernelType = "Parzen")
write.zoo(kernel_cov2, file = "Returns & RV/RV_kernel2.csv", col.names = TRUE)

##### Realized volatility #####
#old method
cov <- rCov(ts, cor = FALSE, alignBy = "minutes", alignPeriod = 5, makeReturns = TRUE)
write.zoo(cov*10000, file = "Returns & RV/RV.csv", col.names = TRUE)

#new method
cov2 <- rCov(ts2, cor = FALSE, alignBy = "minutes", alignPeriod = 5, makeReturns = FALSE)
write.zoo(cov, file = "Returns & RV/RV2.csv", col.names = TRUE)

##### Plot comparison #####
#old method
plot(cov, col = 'red')
lines(kernel_cov, col = 'blue')

plot(kernel_cov-cov)

#new method
plot(cov2, col = 'red')
lines(kernel_cov2, col = 'blue')

plot(kernel_cov2-cov2)




