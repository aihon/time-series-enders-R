#################################
#      Stima AR(2) con          #
#     dati da sim_2.sas7bdat    #
#                               #
#################################

# Clean

rm(list = ls(all=TRUE))

# Si importano i dati

library(haven)
Dati.enders <- read_sas("/home/giovanni/Scrivania/SASDatasets/sim_2.sas7bdat")
View(Dati.enders)

# Si estrae la serie Y3. Il processo sembra stazionario

Y = Dati.enders$Y3
time = Dati.enders$OBS
plot(time,Y, type = "l")

# Si disegnano l'ACF e la PACF,
# sembra un AR(2). Anche se c'è un
# lag significativo nell' ACF a 16
# e nella PACF ci sono i lag signfificativi
# 14 e 17

acf = acf(Y)
pacf = pacf(Y)

# Si stima quindi un AR(2)

library(forecast)
AR_due <- Arima(Y, order = c(2,0,0), include.mean = FALSE)
AR_due

# Si controlla la ACF dei residui
# Sembra non essere rumore bianco.
# C'è una correlazione diversa da zero al lag 16 
# nella ACF. Nella PACF sono significativamente
# diversi da zero gli spike ai lag: 6, 13, 16
# In alternativa usa i residui standardizzati, e plotta il grafico
# contro il tempo e vedi se stanno nell banda +2,-2 

residui_AR_due = residuals(AR_due)
acfresiduiar= acf(residui_AR_due)
pacfresiduiar= pacf(residui_AR_due)


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





