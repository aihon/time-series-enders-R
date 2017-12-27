#################################
#                               #
#         Excercise 9           #
#                               #
#################################

# Load libraries
library(haven)
library(forecast)

# Import the dataset and visualize it
dataset <- read_sas("/your_path/sim_2.sas7bdat")
View(dataset)

# Extract the series Y2. The process seems stationary
Y2 = dataset$Y2
time = dataset$OBS
plot(time, Y2, type = "l")

# Plot the ACF and the PACF. It seems an AR(2) process. The ACF go to 0 oscillating. The PACF have only the first two spike
# significantly different from 0.
acf(Y2)
pacf(Y2)

# Estimate an AR(2)
arTwo <- Arima(Y2, order = c(2,0,0), include.mean = FALSE)

# Check the ACF of the residuals.
# (In alternative we can plot the standardized residuals against time and look if they stay into the +2,-2 band of cofidence).
residualsArTwo = residuals(ArTwo)
acf(residualsArTwo)

# We do a residual analysis using the Ljung-Box test: this method test the null hypothesis of independence of the residuals,
# that is, if residuals are white noise. If the p-value is "big" we "accept" the null otherwise we reject the null.
Box.test(residualsArTwo, lag = 8, type = "Ljung")
Box.test(residualsArTwo, lag = 24, type = "Ljung")

# We try to fit other two models: an AR(1) and an ARMA(1,1)
armaOneOne = Arima(Y2, order = c(1,0,1), include.mean = FALSE)
arOne = Arima(Y2, order = c(1,0,0), include.mean = FALSE)

# We look at residuals
residualsArmaOneOne = residuals(armaOneOne)
acf(residualsArmaOneOne)
residualsArOne = residuals(arOne)
acf(residualsArOne)

# We do the Ljung-Box test. The p-value of the two test for the AR(1) are such that we cannot accept the null of indepence. 
# Then we conclude that the AR(1) it's note te correct model for our DGP.
Box.test(residualsArOne, lag = 8, type = "Ljung")
Box.test(residualsArOne, lag = 24, type = "Ljung")
Box.test(residualsArmaOneOne, lag = 8, type = "Ljung")
Box.test(residualsArmaOneOne, lag = 24, type = "Ljung")

# We choose between AR(2) and ARMIMA(1,1) by looking at the AIC. 
# We select the ARIMA(1,1)
arTwo       # AIC=323 BIC=330
armaOneOne  # AIC=312 BIC=320


