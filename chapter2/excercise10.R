#################################
#                               #
#         Excercise 10          #
#                               #
#################################

# Load libraries
library(haven)
library(forecast)

# Import the dataset and visualize it
dataset <- read_sas("/your_path/sim_2.sas7bdat")
View(dataset)

# Extract the series Y3. The process seems stationary
Y3 = Dati.enders$Y3
time = Dati.enders$OBS
plot(time, Y3, type = "l")

# We plot the ACF and the PACF and Y3 seems a stationary process.
# Anyway we note a significantly lag at 16 in the ACF and in the PACF there are significantly lag at 14 and 17.  
acf = acf(Y3)
pacf = pacf(Y3)

# We estimate an AR(2)
arTwo <- Arima(Y3, order = c(2,0,0), include.mean = FALSE)
arTwo

# We check the ACF of residuals. They doesn't seems to be white noise, in fact there is a correlation different from 0 at 
# lag 16 in the ACF and in the PACF are significantly different from 0 the spikes at lag 6, 13, 16.
# (In alternative we can plot the standardized residuals against time and look if they stay into the +2,-2 band of cofidence).
residualsArTwo = residuals(arTwo)
acfResidualsArTwo = acf(residualsArTwo)
pacfResidualsArTwo = pacf(residualsArTwo)


# We do a residual analysis using the Ljung-Box test: this method test the null hypothesis of independence of the residuals,
# that is, if residuals are white noise. If the p-value is "big" we "accept" the null otherwise we reject the null.
# It's seems that there is correlation, then residuals are not white noise.
Box.test(residualsArTwo, lag = 16, type = "Ljung")

# We try an ARMA(1,16), all coefficients seems significant.
armaOneSixteen = Arima(Y3, order = c(2,0,16), fixed=c(NA,NA,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,NA,0))
armaOneSixteen

# We test if there is correlation in the residulas of the ARMA(1,16).
# It seems there isn't correlation.
residualsArmaOneSixteen = residuals(armaOneSixteen)
acfResidualsArmaOneSixteen = acf(residualsArmaOneSixteen)
pacfResidualsArmaOneSixteen = pacf(residualsArmaOneSixteen)

# We compare the BIC and the AIC of the AR(2) and the ARMA(1,16). Both criteria are in favour of the ARMA(1,16), but 
# from the excerices we know that the process is an AR(2).
arTwo           # BIC=53.86, AIC=46.3
armaOneSixteen  # BIC=50.53, AIC=40.11

# One way to "prove" that we are wrong is to separate the sample in two parts.
# A model have to fit well also a subsample if it fit well the sample. 
Y50 <- Y3[50:100]

# We look at the ACF and the PACF. It's clearly an AR(2) process.
acf(Y50)
pacf(Y50)

# We estimate an AR(2)
arTwoFifty <- Arima(Y50, order = c(2,0,0), include.mean = FALSE)
arTwoFifty

# We test the independence of residuals with a Ljung-Box test. Tere isn't autocorrelation between residuals.
# We choose an AR(2) model because the effect of the sixteenth MA coefficient should have been present also in this subsample.
residualsArTwoFifty = residuals(arTwoFifty)
Box.test(residualsArTwoFifty, lag = 8, type = "Ljung")
Box.test(residualsArTwoFifty, lag = 16, type = "Ljung")
Box.test(residualsArTwoFifty, lag = 24, type = "Ljung")





