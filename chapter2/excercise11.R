#######################
#                     #  
#    Excercise 10     #
#                     #
#######################

# Load libraries
library(haven)
library(forecast)

# Import the dataset and visualize it
dataset <- read_sas("/your_path/quarterly.sas7bdat")
View(dataset)

# We clean the data and take only what we need.
# The series PPI it's clearly not stationary.
dataset[8:19] <- list(NULL) 
dataset[2:6] <- list(NULL) 
plot(dataset, type="l")
time = dataset[,1]
PPI = dataset[,2]

# Because PPI is not stationary we need to differentiate it.
# We use the logaritmic difference:
#                                 dl(PPI_{t}) = log(PPI_{t}) − log(PPI_{t−1})
plot(cbind(time, PPI), type="l")

# We define PPI as a time series
PPIts <- ts(PPI)
plot(time, PPIts)

# We generate the "logaritmic series" and the differenced series.
# "diff" generate a series of which the first element is the difference between the second element and the first element
# of the series PPI, the second elemetn is the difference between the third element and the second element of the series PPI, 
# and so on.
logPPI = log(PPIts)
logDiffPPI = diff(logPPI, lag=1, differences = 1)
acf(logDiffPPI)
pacf(logDiffPPI)

# The logaritmic difference seems stationary.
plot(logdiff)

# We fit the data with three different models and we look at which one have the best in-sample fit, that is which one fit the 
# data better.

# We start estimating an Si AR(||1,3||), that is an AR(3) with the coefficient of the lag 2 forced to be 0 because from the 
# PACF it doesn't seems significant.
arOneThree <- Arima(logDiffPPI, order = c(3,0,0), fixed=c(NA,0,NA,NA), transform.pars = FALSE)
arOneThree # AIC = -1147.94, BIC= -1134.54

# We try now an AR(3) and an ARMA(1, 1).
# Since the two models have to be compared head-to-head, they need to be estimated over the same sample period.
# The estimation for the ARMA(1,1) is constrained to begin on 1961:1 (the first usable observation for the AR model with
# three lags).
arThree <- Arima(logDiffPPI, order = c(3,0,0), fixed=c(NA,NA,NA,NA), transform.pars = FALSE)
arThree # AIC = -1150.93, BIC= -1134.46
armaOneOne <- Arima(logDiffPPI[5:211,], order = c(1,0,1), fixed=c(NA,NA,NA), transform.pars = FALSE)
armaOneOne # AIC = -1125.39, BIC= -1112.06

# Looking at the BIC we choose the AR(||1,3||)

# Now we compare the performance of the out-of-sample fit between the AR(||1,3||) and the ARMA(1,1).
# In order to do that we fit the data in a time interval that exclude the last fifty observations. Than we use the model 
# estimated with the first observations to forecast all the values of the process in the fifty periods excluded.
# We check which of the two models forecast better, that is, which model generate values near to the sample values, using
# the forecast error function.
# We have 211 observations. We estimate the models with the first 160 observations.

# AR(||1,3||) 
for (i in 1:51) {
  model <- arima(window(logDiffPPI, end=159+i),order=c(3,0,0),fixed=c(NA,0,NA,NA), transform.pars = FALSE)
  prediction <- predict(model,n.ahead=1)
  error[i] <- prediction$pre - logDiffPPI[159+i+1]
}
errorAr = error

# ARMA(1,1)
for (i in 1:51) {
  model <- arima(logDiffPPI[5:211,], order = c(1,0,1), fixed=c(NA,NA,NA), transform.pars = FALSE)
  prediction <- predict(model,n.ahead=1)
  error[i] <- prediction$pre - logDiffPPI[159+i+1]
}
errorArima = error

# Alternative method to compute the value of the forecast error function (faster)
# ARMA(1,1)
arimaFor <- function(x, h){forecast(Arima(logDiffPPI, order=c(1,0,1)), fixed=c(NA,NA,NA), transform.pars = FALSE, h=1)}
errorArimaFor <- tsCV(logDiffPPI, arimaFor, h=1)
errorArimaFori = errorArimaFor[160:211,]

# AR(||1,3||) 
arFor <- function(x, h){forecast(Arima(logdiff, order=c(3,0,0)), fixed=c(NA,0,NA,NA), transform.pars = FALSE, h=1)}
errorArFor <- tsCV(logDiffPPI, arFor, h=1)
errorArFori = errorArFor[160:211,]

# We perform a Diebold-Mariano test.
# (Read the documentation with ?dm.test to see which are the alternative hyphotesis).
# The p-value > 0.05, then we don't reject the null that the accuracy of the two model is the same.
dm.test(errorAr, errorArima, alternative=c("two.sided"), power=1)

# Moreover
qt(0.975, 49) # = 2. Critical value t, 95% confidence, 2 sided, 49 df. Our test is 1,15 then we cannot reject the null.

# We try now an AR(5) and an ARMA(2,1)
# AR(5)
arFive <- Arima(logDiffPPI, order = c(5,0,0), fixed=c(NA,NA,NA,NA,NA,NA), transform.pars = FALSE)
arFive # AIC = -1148.25 BIC= -1124.79
# ARMA(2,1)
arimaTwoOne <- Arima(logDiffPPI, order = c(2,0,1), fixed=c(NA,NA,NA,NA), transform.pars = FALSE)
arimaTwoOne # AIC = -1148.87 BIC= -1132.11

# We test the forecast accuracy
# AR(5)
arFiveEst <- function(x, h){forecast(Arima(logdiff, order=c(5,0,0)), fixed=c(NA,NA,NA,NA,NA,NA), transform.pars = FALSE, h=1)}
errorArFiveEst <- tsCV(logDiffPPI, Ar5st, h=1)
errorArFiveEsti = errorArFiveEst[160:211,]

# ARIMA(2,1)
arimaTwoOneEst <- function(x, h){forecast(Arima(logdiff, order=c(2,0,1)), fixed=c(NA,NA,NA,NA), transform.pars = FALSE, h=1)}
errorArimaTwoOneEst <- tsCV(logDiffPPI, ARIMA_21, h=1)
errorArimaTwoOneEsti = errorArimaTwoOneEst[160:211,]

# We perform a Diebold-Mariano test to compare the AR(||1,3||) with the AR(5) and the ARIMA(2,1)
dm.test(errorArFori, errorArFiveEsti, alternative=c("two.sided"), power=1) # pvalue < 0.05 quindi si rigetta la nulla
dm.test(errorArFori, errorArimaTwoOneEsti, alternative=c("two.sided"), power=1) # pvalue < 0.05 quindi si rigetta la nulla
