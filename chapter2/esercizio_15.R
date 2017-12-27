###############################
#                             #
#        Excercise 16         #
#                             #
###############################  

# Load libraries
library(forecast)
library(haven)
library(gdata)
library(lmtest)
library(qcc)

# We import the dataset and we take only what we need.
dataset <- read.xls("/your_path/y_break.38134409.xls", sheet=1, header = TRUE)
yBreak = dataset[,2]
yBreakTs = ts(yBreak)
plot(yBreakTs, type="l")

# We try to fit an AR(1) model.
arOne = Arima(yBreakTs, order = c(1,0,0), fixed = c(NA,NA))
arOne # AIC=473.16   BIC=482.19

# Dummy variables allow us to capture changes that we suppose have occurred in the process. If these changes are really occurred
# the modelling of the process have to consider it.
# Dummy variablese can assume values 0 or 1.
# In our case we think that a break have occurred in the period 101.
# We test if this break is an intercept and/or a slope break using dummy variables.
# In order to test a break in the intercept we use a dummy d_{t} that have value 0 before period 101 and value 101 from 101 
# onwards.
# If the coefficient of d_{t} is signficant it means that a break occurred (we sum the value of the coefficient of d_{t}
# to the value of the intercept).
# Instead, in order to test a break in the slope we use a dummy the d_{t} multiplied by y_{t-1}. If the coefficient of d_{t} is 
# signficant it means that a break in the slope occurred (we sum the value of the coefficient of d_{t} to the value of 
# coefficient y_{t-1}).

# First we test a break in the intercept. 
dummy = rep(1,150)
dummy[1:100]=0
breakIntercept = Arima(yBreakTs, order = c(1,0,0), xreg=dummy)
coeftest(breakIntercept)

# Now we test a break in the intercept and in the slope, that is, in the AR coefficient of the first lag.
dummyLag = lag(yBreakTs)*dummy
B = matrix(c(dummy, dummyLag), nrow=length(dummy), ncol=2) 
breakInterceptSlope = Arima(yBreakTs, order = c(1,0,0),  xreg=B)
coeftest(breakInterceptSlope)

# Coefficients in both tests seems significant, then a break occurred.

# We estimate the series as an AR(2) process and we confront the performance with respect to the AR(1).
arTwo = Arima(yBreakTs, order = c(2,0,0), fixed = c(NA,NA,NA))
arTwo  # AIC=469.27, AICc=469.54, BIC=481.31

# Both AIC and BIC are lower for the AR(2).

# We plot the value of the AR coefficient of the first lag, ar_{1}, that we obtain estimating an AR model recursively 
# starting from 1. We see that a break occurred. 
coefficientFirstLag <- 0
for (i in 1:149) {
  model <- arima(window(yBreakTs, end=i+1),order=c(1,0,0),fixed=c(NA,NA))
  coefficientFirstLag[i] <-  model$coef
}
plot(coefficientFirstLag, type="l")

# We plot the CUSUM (Cumulative Sum) of an AR(2) to check if it is suitable.
error <- 0
for (i in 10:149) {
  model <- arima(window(yBreakTs, end=i),order=c(2,0,0),fixed=c(NA,NA,NA))
  prediction <- predict(model, n.ahead=1)
  error[i] <- yBreakTs[i+1] - prediction$pred 
}
standardDeviationError = sd(error, na.rm = TRUE)
errorNoNA = error[10:149]
cumulativeSum = cumsum(errorNoNA)
CUSUM = cumulativeSum/standardDeviationError 
plot(CUSUM, type ="l")
abline(h= 0, col="red")

# It seems that an AR(2) is not suitable. 
