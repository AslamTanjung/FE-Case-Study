library(QRM)
library(fitdistrplus)
library(fitdistr)
library(moments)

ret_close <- read.csv("Returns & RV/Close_to_close_log_returns.csv", sep = " ")
ret_close$Index <- as.POSIXct(ret_close$Index,format="%Y-%m-%d %H:%M:%S")
ret_close <- xts(ret_close$V1, ret_close$Index)

ret_oc <- read.csv("Returns & RV/Open_to_close_log_returns.csv", sep = " ")
ret_oc$Index <- as.POSIXct(ret_oc$Index,format="%Y-%m-%d %H:%M:%S")
ret_oc <- xts(ret_oc$V1, ret_oc$Index)

#returns close to close
returns_close <- data.frame(ret_close[-1])
colnames(returns_close)[colnames(returns_close) == "ret_close..1."] ="returns"

#returns open to close
returns_oc <- data.frame(ret_oc[-1])
colnames(returns_oc)[colnames(returns_oc) == "ret_oc..1."] ="returns"

#density plot
plot(density(returns_close$returns))
plot(density(returns_oc$returns))

#qq-plot
qqnorm(returns_close$returns);qqline(returns_close$returns, col = 2)
qqnorm(returns_oc$returns);qqline(returns_oc$returns, col = 2)

#normality test
shapiro.test(returns_close$returns) #conclusion: not normal
shapiro.test(returns_oc$returns) #conclusion: not normal

#skewness test
skewness(returns_close$returns) #conclusion: positive skewness (close to close)
skewness(returns_oc$returns) #conclusion: positive skewness (open to close)


descdist(returns_close$returns, discrete = FALSE) #conclusion: high kurtosis
descdist(returns_oc$returns, discrete = FALSE) ##conclusion: high kurtosis


###### fitted student t on open to close returns ######
tfit = fit.st(returns_oc$returns)
tpars = tfit$par.ests
tpars

# Define tpars, nu, mu, and sigma
tpars <- tfit$par.ests
nu <- tpars[1]
mu <- tpars[2]
sigma <- tpars[3]

# Plot a histogram of djx
hist(returns_oc$returns, nclass = 80, probability = TRUE, ylim = range(0, 0.4))

# Compute the fitted t density at the values djx
yvals <- dt((returns_oc$returns - mu)/sigma, df = nu)/sigma

# Superimpose a red line to show the fitted t density
lines(returns_oc$returns, yvals, col = "red", type= 'p')




###### fitted student t on close to close returns ######
tfit2 = fit.st(returns_close$returns)

# Define tpars, nu, mu, and sigma
tpars2 <- tfit2$par.ests
nu2 <- tpars2[1]
mu2 <- tpars2[2]
sigma2 <- tpars2[3]

# Plot a histogram of djx
hist(returns_close$returns, nclass = 80, probability = TRUE, ylim = range(0, 0.4))

# Compute the fitted t density at the values djx
yvals2 <- dt((returns_close$returns - mu2)/sigma2, df = nu2)/sigma2


#Superimpose a red line to show the fitted t density
lines(returns_close$returns, yvals2, col = "red", type= 'p')

