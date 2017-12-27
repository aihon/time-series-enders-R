#----------------#
#  ESERCIZIO 2
#----------------#

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

Dati <- read.xls("/home/giovanni/Scrivania/SASDatasets/Terrorism.38140308.XLS", header = TRUE)
View(Dati)

trans = Dati$Transnational
trans = ts(trans)

plot(Dati$Transnational, type = "l")

# Punto a). Per trovare il miglior modello che
# fitta i dati si usa l'intervallo pre o post
# intervention con più osservazioni. In questo 
# caso il pre intervention.
# (Puoi provare anche ad usare auto.arima()
# 

trans1 = ts(trans[0:112])     # 112 osservazioni
trans2 = ts(trans[113:164])   # 51 osservazioni

acf(trans1)
pacf(trans1) # sembra un AR(2)

plot(trans1)
test1 = ur.df(trans1, type = c("drift"))
summary(test1) # Rifiuto la nulla di non stazionarietà

AR_due = Arima(trans1, order = c(2,0,0))
plot(AR_due$residuals)
acf(AR_due$residuals)
pacf(AR_due$residuals)
Box.test(AR_due$residuals) # p-value = 0.9426 > 0.05 e quindi "accetto" la nulla
                           # di residui white noise

# Punto b)

dummy1 = rep(0,164)
dummy1[114:164]=1

modello1 = lm(trans ~ dummy1)
summary(modello1)

modello2 = dynlm(trans ~  L(trans, 1) + L(trans, 2) + dummy1)
summary(modello2)

AIC(modello1) # 1277.843
BIC(modello1) # 1287.142
plot(modello1$residuals, type ="l")
acf(modello1$residuals)
pacf(modello1$residuals)
Box.test(modello1$residuals)

AIC(modello2) # 1181.024
BIC(modello2) # 1196.462
plot(modello2$residuals, type ="l")
acf(modello2$residuals)
pacf(modello2$residuals)
Box.test(modello2$residuals)

# Si seleziona senza ombra di dubbio il modello 2

# Punto c) NON SO RISOLVERLO (oppuire la risposta è: guardando all'ACF)

plot(trans) # non sembra un processo stazionario, si prova 
            # il test di DF
acf(trans)  # Molta persistenza
pacf(trans)

# Magari poteva essere ceh ha stimato il processo con un AR(2)
# e gli veniva un valore di a1 vicino a 1 perchè c'è un break
# e quindi a valori alti seguono valori alti e a valori bassi seguono valori bassi
# Ma non pare stimi un valore di a1 vicino a 1.

AR_duetrasn = Arima(trans, order=c(2,0,0))

# Proviamo a fare un test di DF di radice unitaria.
# auto.arima() indica che non è un AR puro quindi si usa 
# un ADF

auto.arima(trans, stepwise=FALSE ,approximation=FALSE)

adftrans = ur.df(trans, type = c("drift"))
summary(adftrans)  # rifiuto la nulla di radice unitaria

# Punto d)


# L'effetto di lungo terine è c0/(1-a1)
# Per Jennifer è:  -5/(1-0.323)= -7.385524
# Per  Justin è:   -16.41/(1-0.87)= -126.2308
# Dunque per il modello di Justin gli attacchi 
# terroristici si stabilizzeranno su livelli più bassi
# rispetto a quelli di Jennifer

#Punto e)  Ma quale modello dovrei usare??

dummy2 = rep(0,164)
dummy2[90]=1







