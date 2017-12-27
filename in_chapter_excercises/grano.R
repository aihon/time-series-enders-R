#####################
#  Esercizio grano  #
#      pag. 323     #
#####################

# Pulizia

rm(list = ls(all=TRUE))

# Pacchetti

library(forecast)
library(FinTS)
library(haven)
library(gdata)
library(lmtest)
library(qcc)
library(urca)
library(tseries)
library(plm)
library(quantmod)
library(mFilter)
library(ggplot2)
library(gridExtra)
library(reshape2)
library(CADFtest)
library(dynlm)
library(car)
library(TSA)

# Dati

Dati <- read.xls("/home/giovanni/Scrivania/SASDatasets/Enders_Holt.312103142.xls", header = TRUE)
View(Dati)


# Si seleziona il lag length del VAR

Datin <- Dati
Datin[,1] <- NULL
Datin <- Datin[-456,]
View(Datin)


VARselect(Datin, lag.max = 20) # AIC seleziona 4 lags. Enders nel libro 7

# SI stima il VAR

var1 = VAR(Datin, p=7)

# SI applicano poi restrizioni alla matrice B, che è quella che moltiplica
# gli errori. 

# Matrice delle restrizioni ceh vogliamo imporre. Più del necessario, ovvero 9.
# Exact identification in un VAR con 4 variabii richiede 4*(4-1)/2 rrestrizioni
# Questo tipo di restrizione si impone solo perch si vogliono studiare
# le cause del prezzo del grano, infatti si lascia diepndere gli errori
# del grano dagli shocks di tutte le variabili

B <-diag(4)
diag(B) <- NA
B[4,1] = B[4,2] = B[4,3] = NA
B

# Si stima il SVAR
svar1 <- SVAR(var1, estmethod = "scoring", Bmat = B, hessian = TRUE)
summary(svar1)
svar1$LR  # Chi^2 = 12.542, p-value = 0.005739
          # p < 0.05 allora rifiuto le restrizioni

# Siccome la correlazione tra e2 ed e3 è alta
# si provano le seguenti restrizioni

B1 <-diag(4)
diag(B1) <- NA
B1[4,1] = B1[4,2] = B1[4,3] = B1[2,3] = NA
B1

svar2 <- SVAR(var1, estmethod = "scoring", Bmat = B1, hessian = TRUE)
summary(svar2)
svar2$LR # Chi^2 = 3.7186, p-value = 0.1558
         # le restrizioni sono corrette

# Dunque: real grain prices are contemporaneously affected 
# by all variables, and the real exchange rate is contemporaneously
# affected by real interest rate shocks. The innovations in real
# energy prices and interest
# rates are due to their own pure shocks.


# Impulse response functions
irf2 = irf(svar2, boot = FALSE)
plot(irf2)

# Vediamo il grano pg come risponde
irf_pg = irf(svar2, response = "pg", boot = FALSE)
plot(irf_pg)

# Variance decomposition

vd = fevd(svar2, n.ahead = 20)
plot(vd)



