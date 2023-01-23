library(readr)
library(dplyr)
library(highfrequency)
library(xts)
library(TTR)
library(rugarch)
library(ggplot2)

data <- read.csv(unz("Data/Disney.zip", "Disney.csv"), header=T)
## Remove first row, because it is empty
data <- data[2:nrow(data),]
data <- data %>%
  mutate(DATE_TIME = paste(DATE, TIME)) %>%
  mutate(DATE_TIME = as.POSIXct(DATE_TIME, tryFormats = c("%Y/%m/%d %H:%M:%S"))) %>%
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
ret_open <- ROC(ts_open, n = 1)
ret_close <- ROC(ts_close, n = 1)


plot(index(ret_open), ret_open, type = "l", xlab = "Time", ylab = "Open Return")
plot(index(ret_close), ret_close, type = "l", xlab = "Time", ylab = "Close Return")

##