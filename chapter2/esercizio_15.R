#-----------------------------#
#                             #
#        ESERCIZIO 16         #
#                             #
#-----------------------------#  


# Clean workspace

rm(list = ls(all=TRUE))

# Librerie

library(forecast)
library(haven)
library(gdata)
library(lmtest)
library(qcc)



# Dati

Dati <- read.xls("/home/giovanni/Scaricati/y_break.38134409.xls", sheet=1, header = TRUE)
View(Dati)


plot(Dati, type = "l")

# Proviamo a modellizzare la serie come un AR(1)

ybreak = Dati[,2]
View(ybreak)
ybreakts = ts(ybreak)
View(ybreakts)

# Le dummy variable ci consentono di catturare dei cambiamenti
# che si suppone ci siano stati nel processo e ceh quindi la descrizione/
# modellizzazione del processo deve essere diversa per tenere conto di
# questi cambiamenti. Le variabili dummy assumono valore 1 o 0.
# Nel nostro caso si crede ci sia stato un break nel periodo 101.
# Si testa quindi se c'è stato un intercept e/o slope break usando
# dummy variables. 
# Per testare un break nell'intercetta uso una dummy D_t che ha valore
# 0 prima di 101 e valore 1 da 101 in poi. Se il suo coefficiente è
# significativo vuol dire che vi è stato un cambiamento (vado a sommare il valore 
# a quello dell'intercetta).
# Per testare un brak nella slope uso la dummy D_t*y_t-1 e vale il ragionamento di sopra.

# Si replicano i dati del libro


AR1 = Arima(ybreakts, order = c(1,0,0), fixed = c(NA,NA))
AR1 # AIC=473.16   BIC=482.19

# Prima testo um break nell'intercetta
# PROVA A CERCARE INFO SUL CHOW TEST CON R

dummy = rep(1,150)
dummy[1:100]=0

breakint = Arima(ybreakts, order = c(1,0,0),  xreg=dummy)
breakint
coeftest(breakint)

# Si testa un break nell'intercetta e nell'AR coefficient
# del lag primo

nuova = lag(ybreakts)*dummy
B = matrix( c(dummy, nuova), nrow=length(dummy), ncol=2) 

breakintslo = Arima(ybreakts, order = c(1,0,0),  xreg=B)
breakintslo
coeftest(breakintslo)

# I coefficienti in entrambi i test sembrano
# significativi. Dunque c'è stato un break

# Estimate the series as an AR(2) process. In what sense does the AR(2) model perform
# better than the AR(1) model estimated in part a?

AR2 = Arima(ybreakts, order = c(2,0,0), fixed = c(NA,NA,NA))
AR2  # AIC=469.27   AICc=469.54   BIC=481.31

# L'AIC e il BIC sono inferiori per L'AR(2)

# Si plotta il valore del coefficiente ar1 stimando il modello
# come un AR ricorsivamente partendo da 1. Si vede che vi è un
# break 

coefficiente <- 0
for (i in 1:149) {
  mod <- arima(window(ybreakts, end=i+1),order=c(1,0,0),fixed=c(NA,NA))
  coefficiente[i] <-  mod$coef
}
plot(coefficiente, type="l")


# Si plotta il CUSUM di un AR(2)
# per vedere se è adeguato

error <- 0
for (i in 10:149) {
  mod <- arima(window(ybreakts, end=i),order=c(2,0,0),fixed=c(NA,NA,NA))
  pre <- predict(mod,n.ahead=1)
  error[i] <- ybreakts[i+1] - pre$pred 
}

sder = sd(error, na.rm = TRUE)

errornoNA = error[10:149]

sommacumu = cumsum(errornoNA)

CUSUM = sommacumu/sder 

plot(CUSUM, type ="l")
abline(h= 0, col="red")

# Sembra che anche un AR(2) non sia idoneo
  
  
  
  