#---------------------------
#
#  ESERCIZIO 4 CAP.4
#
#---------------------------


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


# Dati

Dati <- read_sas("/home/giovanni/Scrivania/SASDatasets/panel.sas7bdat")
View(Dati)

Aus = Dati[,2]
Aus = ts(Aus)

Can = Dati[,3]
Can = ts(Can)

Fra = Dati[,4]
Fra = ts(Fra)

Ger = Dati[,5]
Ger = ts(Ger)

Jap = Dati[,6]
Jap = ts(Jap)

Net = Dati[,7]
Net = ts(Net)

UK = Dati[,8]
UK = ts(UK)

US = Dati[,9]
US = ts(US)


# Si verifica il lag lenght, il valore di gamma
# e la statistica t riportate in tabella 4.8
# Si possono usare: adf.test(), CADFtest(),
# ur.df()
# |Per testare il length prova ad applicare
# |la funzione AIC() ai vari modelli stimati con
# |vari lag
# ALtra opzione è: siccome si hanno i valori dei vari
# lag si vede che i coefficienti del lag successivo 
# non sono significativi

# Aumentare i lag dimiuisce l'AIC, WHY!!!
#funzione = dynlm(d(Aus) ~ L(Aus,1) + L(d(Aus),1)+ L(d(Aus),2)+ L(d(Aus),3)+ L(d(Aus),4)+ L(d(Aus),5)
#                 + L(d(Aus),6)+ L(d(Aus),7)+ L(d(Aus),9))
#
#AIC(funzione)


# La regione di accettazione/rifuito è
# siccome abbiamo n=8 serie e T=133 osservazioni
# ad un livello di significatività 1% e 5%
# rispettivamente: -2.28 e -2.06 
# Quindi se la nostra statistica t cade alla sinistra 
# di questi valori si rifiuta la nulla di radice unitaria


#AUS
aus2 = ur.df(Aus, type = c("drift"), selectlags = c("BIC"))     # ACCETTO
summary(aus2) #gamma = -0.03386, t-stat = -1.3083    

# CAN
can1 = ur.df(Can, type = c("drift"), selectlags = c("BIC"))     # ACCETTO
summary(can1) #gamma = -0.03044, t-stat = -1.6858      

#FRA
fra1 = ur.df(Fra, type = c("drift"), selectlags = c("BIC"))     # RIFIUTO
summary(fra1) #gamma = -0.08171, t-stat = -3.1075 

#GER
ger = ur.df(Ger, type = c("drift"), selectlags = c("BIC"))      # RIFIUTO
summary(ger) #gamma = -0.06767, t-stat = -2.6693

#JAP
jap = ur.df(Jap, type = c("drift"), selectlags = c("BIC"))      # RIFIUTO
summary(jap) #gamma = -0.05951, t-stat = -2.3281

#NET
net = ur.df(Net, type = c("drift"), selectlags = c("BIC"))      # RIFIUTO
summary(net) #gamma = -0.11126, t-stat = -3.5194

#UK
uk = ur.df(UK, type = c("drift"), selectlags = c("BIC"))        # RIFIUTO
summary(uk) #gamma = -0.07873, t-stat = -2.7497

#US
us = ur.df(US, type = c("drift"), selectlags = c("BIC"))        # ACCETTO
summary(us) #gamma = -0.03626, t-stat = -1.8237

# Si confrontano i risultati ottenuti con l'ADF test
# con quelli che si ottengono con un DF-GLS test
# Qui i valori critici sono all'1 e al 5 % rispettivamente:
# -2.58 e -1.94
# NON TORNA NULLAAAAAAAAAAAAAAAAAAAAAA
#AUS
dfgls_Aus = ur.ers(Aus, type = c("DF-GLS"), model = c("constant")) # ACCETTO
summary(dfgls_Aus) # gamma = -0.02668, t = -1.0753 

#CAN
dfgls_Can = ur.ers(Can, type = c("DF-GLS"), model = c("constant")) # ACCETTO
summary(dfgls_Can) # gamma = -0.02930, t = -1.57 

#FRA
dfgls_Fra = ur.ers(Fra, type = c("DF-GLS"), model = c("constant")) # ACCETTO
summary(dfgls_Fra) # gamma = -0.005329, t = -0.4191 

#GER
dfgls_Ger = ur.ers(Ger, type = c("DF-GLS"), model = c("constant")) # ACCETTO
summary(dfgls_Ger) # gamma = -0.013419, t = -0.9849 

#JAP
dfgls_Fra = ur.ers(Fra, type = c("DF-GLS"), model = c("constant")) # ACCETTO
summary(dfgls_Fra) # gamma = -0.013419, t = -0.9849 

#NET
dfgls_Net = ur.ers(Net, type = c("DF-GLS"), model = c("constant")) 
summary(dfgls_Net) # gamma = -0.02668, t = -1.0753 

#UK
dfgls_UK = ur.ers(UK, type = c("DF-GLS"), model = c("constant")) 
summary(dfgls_UK) # gamma = -0.02668, t = -1.0753 

#US
dfgls_US = ur.ers(US, type = c("DF-GLS"), model = c("constant")) 
summary(dfgls_US) # gamma = -0.02668, t = -1.0753 


# PER IL PUNTO c) VEDI LA CARTELLA dfgls enders

# IL PUNTO d) HA A CHE FARE CON SEASONAL QUINDI SALTO

