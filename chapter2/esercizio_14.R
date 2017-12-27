##########################
#                        #
#     Excercise 14       #
#                        #   
##########################


# Load libraries
library(haven)
library(forecast)

# Import the dataset 
dataset <- read_sas("/your_path/quarterly.sas7bdat")

# Generate the spread as: r5 - Tbill
spread = dataset[,5] - dataset[,3]
spread <- ts(spread)

# Estimate an AR(7) and an ARMA(1,1)
arSeven <- Arima(spread, order = c(7,0,0), fixed=c(NA,NA,NA,NA,NA,NA,NA,NA))
arSeven   # AIC=289.32      BIC=319.53
armaOneOne <- Arima(spread, order = c(1,0,1), fixed=c(NA,NA,NA))
armaOneOne # AIC=293.5  BIC=306.92

# We estimate the AR(7) and the ARMA(1, 1) over the period 1960Q1–2000Q3.
# We use the rows 1:163
arSevenReduced <- Arima(spread[1:163,], order = c(7,0,0), fixed=c(NA,NA,NA,NA,NA,NA,NA,NA))
arSevenReduced   # AIC=235.74   BIC=263.59
armaOneOneReduced <- Arima(spread[1:163,], order = c(1,0,1), fixed=c(NA,NA,NA))
armaOneOneReduced # AIC=241.56   BIC=253.93


# We compute the out-of-sample forecast with rolling origin and we form the erroor forecast.

# AR(7)
error <- 0
forecastArSeven <- 0
for (i in 1:49) {
  model <- arima(window(spread, end=162+i),order=c(7,0,0),fixed=c(NA,NA,NA,NA,NA,NA,NA,NA))
  prediction <- predict(model, n.ahead=1)
  error[i] <- spread[163+i] - prediction$pred
  forecastArSeven[i] = prediction$pred
}
errorArSeven = error
mspeErrorArSeven = mean(errorArSeven^2, na.rm=TRUE) # MSPE: Mean Square Prediction Error


# AR(7): alternative method
k <- 163 # minimum size for training set
n <- length(spread) # total number of observations
e <- spread*NA # vector to record one-step forecast errors
for(i in 163:(n-1))
{
  train <- ts(spread[1:i],freq=1)
  fit <- arima(train, order=c(7,0,0),fixed=c(NA,NA,NA,NA,NA,NA,NA,NA))
  fc <- forecast(fit,h=1)$mean
  e[i] <- spread[i+1]-fc
}

# ARMA(1,1)
error <- 0
forecastArimaOneOne <- 0
for (i in 1:49) {
  model <- arima(window(spread, end=162+i),order=c(1,0,1),fixed=c(NA,NA,NA))
  prediction <- predict(mod,n.ahead=1)
  error[i] <- spread[163+i] - prediction$pred 
  forecastArimaOneOne[i] <- prediction$pred
}
errorArimaOneOne = error
mspeerrorArimaOneOne = mean(errorArimaOneOne^2,na.rm=TRUE)

# From the MSPE seems that the ARIMA(1,1) fit better the data.

# We check if the forecast are unbiased or not.
lrArSeven = lm(spread[164:212,] ~ forecastArSeven)
summary(lrArSeven)  
lrArimaOneOne = lm(spread[164:212,] ~ forecastArimaOneOne)
summary(lrArimaOneOne)

