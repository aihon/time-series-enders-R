#################################
#                               #
#         Excercise 9           #
#                               #
#################################

# Load libraries
library(haven)

# Import data
dataset <- read_sas("/ypurpath/sim_2.sas7bdat")
View(Dati.enders)

# Si estrae la serie Y2. Il processo sembra stazionario

Y2 = Dati.enders$Y2
time = Dati.enders$OBS
plot(time,Y2, type = "l")

# Si disegnano l'ACF e la PACF

acf(Y2)
pacf(Y2)

# Sembra un AR(2). La ACF va a zero alternando 
# valori positivi e negativi. La PACF ha solo 
# i primi due spike significativi
# Si stima quindi un AR(2)

library(forecast)
AR_due <- Arima(Y2, order = c(2,0,0), include.mean = FALSE)

# Si controlla la ACF dei residui
# In alternativa usa i residui standardizzati, e plotta il grafico
# contro il tempo e vedi se stanno nell banda +2,-2 

residui_AR_due = residuals(AR_due)
acf(residui_AR_due)

# Si fa un'analisi dei residui col 
# Ljung-Box test: questo test testa la nulla
# di indipendenza dei residui, ovvero se
# sono o no white noise. Se il p-value è
# grande si accetto la nulla, se il p-value
# è piccolo rifiuto la nulla

Box.test(residui_AR_due, lag = 8, type = "Ljung")
Box.test(residui_AR_due, lag = 24, type = "Ljung")

# Si provano altri due modelli
# un AR(1) e un ARMA(1,1)

ARMA_unouno =Arima(Y2, order = c(1,0,1), include.mean = FALSE)
AR_uno = Arima(Y2, order = c(1,0,0), include.mean = FALSE)

# Si guardano i residui

residui_ARMA_unouno = residuals(ARMA_unouno)
acf(residui_ARMA_unouno)

residui_AR_uno = residuals(AR_uno)
acf(residui_AR_uno)

# Test di Ljung-Box.
# Come si vede il p-value dei test
# per l'AR(1) non fanno accettare la nulla
# di indipendeza. Quindi l'AR(1) non è il
# modello corretto per il nostro DGP

Box.test(residui_AR_uno, lag = 8, type = "Ljung")
Box.test(residui_AR_uno, lag = 24, type = "Ljung")


Box.test(residui_ARMA_unouno, lag = 8, type = "Ljung")
Box.test(residui_ARMA_unouno, lag = 24, type = "Ljung")

# Si sceglie tra l'AR(2) e l'ARIMA(1,1)
# guardando l'AIC

AR_due # AIC=323 BIC=330
ARMA_unouno # AIC=312 BIC=320


