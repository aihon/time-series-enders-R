#################################
#
# Esercizio Terrorismo pag. 310
#
#################################


# Clean workspace

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
library(pracma)
library(Quandl)
library(vars)
library(dynlm)
library(tsDyn)
library(dse)


# Dati

Dati <- read.xls("/home/giovanni/Scrivania/SASDatasets/Terrorism.38140308.XLS")
View(Dati)

Datinew <- Dati[37:164,]
View(Datinew)

plot(Datinew$Domestic, type = "l")
plot(Datinew$Transnational, type = "l")

# Siccome vogliamo testare la Granger causality sia di dom
# su trans che di trans su dom, vogliamo prima vedere se 
# le due serie sono stazionarie. Per farlo facciamo un test
# di DF e un test di DF-GLS (ERS).
# Si deve prima selezionare il lag length nel modello,
# per farlo si può usare il general-to-specific method,
# oppure usare le funzioni AIC() e BIC() apploicate al
# modello coi vari lag e scegliere il modello con i valori
# più piccoli

# RIcontrollare i valori e capire come fare per selzeionare lag


# DF e DF-GLS su dom
df1 = ur.df(Datinew$Domestic, type = c("drift"),  lags = 2)          # -2.5144
summary(df1) # non rifiuto
ers1 = ur.ers(Datinew$Domestic, type = c("DF-GLS"), lag.max = 2)      # -2.4703 
summary(ers1) # rifiuto

# DF e DF-GLS su trans
df2 = ur.df(Datinew$Transnational, type = c("drift"), lags = 1)      # -2.6867
summary(df2) # rifuiuto
ers2 = ur.ers(Datinew$Transnational, type = c("DF-GLS"), lag.max = 1) # -2.5164 
summary(ers2) # rifiuto


# Ora si vuole testare la granger causality in entrambi i versi
# SI deve selezionare il lag length del VAR
# Per testare restrizioni sui coefficienti usare la funzione restrict()

Datinew[,1] <- NULL
View(Datinew)

dom = Datinew$Domestic
tra = Datinew$Transnational

VARselect(Datinew) # AIC seleziona 3 lags

var = VAR(Datinew, p = 3)
var

# causality.
# Voglio testare se dom GC o no tra.
# Voglio cioè vedere se A_21 = 0 o meno
causality(var, cause = "Domestic")$Granger       # F = 3.9797
                                                 # p = 0.008597 < 0.05 rifiuto 
                                                 # la nulla e quindi dom GC tra

# Voglio testare se transnational Granger cause
# Domestic, ovvero se A_12 = 0 o meno
causality(var, cause = "Transnational")$Granger  # F = 1.9627
                                                 # p = 0.1203 > 0.05 accetto la nulla
                                                 # che tra non GC dom




summary(var)
plot(var)

# Variance decomposition

graphics.off()
par(mar = rep(2, 4))

d12 = fevd(var, n.ahead = 12)
plot(d12)
d12

# Impulse response functions

irf1 = irf(var, n.ahead = 20)
plot(irf1)


