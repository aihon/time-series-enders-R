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


# Si fa un'analisi dei residui col 
# Ljung-Box test: questo test testa la nulla
# di indipendenza dei residui, ovvero se
# sono o no white noise. Se il p-value è
# grande si accetto la nulla, se il p-value
# è piccolo rifiuto la nulla. 
# Viene fuori che c'è correlazione, non sono
# white noise i residui

Box.test(residui_AR_due, lag = 16, type = "Ljung")

# Si prova un ARMA(1,16), tutti i coefficienti
# sembrano significativi. 

ARMAunosedici = Arima(Y, order = c(2,0,16), fixed=c(NA,NA,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,NA,0))
ARMAunosedici

# Testiamo se c'è correlazione tra i residui.
# Sembra che non ci sia autocorrelazione

residui_ARMA_unosedici = residuals(ARMAunosedici)
acfresiduiarma= acf(residui_ARMA_unosedici)
pacfresiduiarma= pacf(residui_ARMA_unosedici)

# Confrontiamo BIC e AIC dell'AR e dell'ARMA.
# I due criteri sono a favore dell'ARMA.
# Dall'esercizio si sa però che il DGP è
# un AR(2)


AR_due # BIC=53.86, AIC=46.3
ARMAunosedici # BIC=50.53, AIC=40.11

# Un modo per provare che si è sbagliato
# è dividere il sample in due. Anche in un subsample
# deve valere lo stesso modello che vale nel sample

Y50 <- Y[50:100]

# ACF e PACF, ora è proprio un AR(2)

acf(Y50)
pacf(Y50)

# Si stima un AR(2)

AR_due50 <- Arima(Y50, order = c(2,0,0), include.mean = FALSE)
AR_due50

# Si analizzano i residui con il test di Ljung-Box
# e non emerge autocorrelazione significativa tra i residui.
# Dunque il modell che si sceglie è l'AR(2) perchè l'effetto del 
# 16esimo coefficinte MA sarebbe dovuto essere presente anche in
# questo subsample

residui_AR_due50 = residuals(AR_due50)
Box.test(residui_AR_due50, lag = 8, type = "Ljung")
Box.test(residui_AR_due50, lag = 16, type = "Ljung")
Box.test(residui_AR_due50, lag = 24, type = "Ljung")





