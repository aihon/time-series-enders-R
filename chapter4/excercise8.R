#-----------------#
#   ESERCIZIO 8   #
#-----------------#

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
library(dynlm)


# Dati

Dati <- read_sas("/home/giovanni/Scrivania/SASDatasets/quarterly.sas7bdat")
View(Dati)

# Si forma lo spread

s = Dati[,5] - Dati[,3]
s = ts(s)
as = acf(s)
as

# Si stima il modello usato ep rfare DF test e si guarda
# quanti lags ci indicano i vari criteri

scadfa = CADFtest(s, type=c("drift"), max.lag.y=12, criterion=c("AIC"))
summary(scadfa) # 9 lags

scadfb = CADFtest(s, type=c("drift"), max.lag.y=12, criterion=c("BIC"))
summary(scadfb) # 1 lag

estimated <- dynlm(d(s) ~ L(s, 1) + L(d(s), 1) + L(d(s), 2) +  L(d(s), 3) + L(d(s), 4)
                     + L(d(s), 5) + L(d(s), 6) + L(d(s), 7) + L(d(s), 8)) 
summary(estimated)    # il gts method seleziona 8 lags. il nono è buono ad un alpha 0.1


# Proviamo a fare l'adf test usando i lag
# selezionati dai vari metodi per vedere se
# conta il lag length nel DF test

test1 = ur.df(s, type = c("drift"), lags = 1)  # 1 lag, t = -4.7505 

test2 = ur.df(s, type = c("drift"), lags = 9) # 9 lags, t = -4.7026 

test3 = ur.df(s, type = c("drift"), lags = 8) # 8 lags, t = -4.3657

summary(test1)

# I valori critici si ricavano da
# summary(test1). All' 1, 5 e 10 sono 
# rispettivamente: -3.46, -2.88 e -2.57
# e quindi in tutti i casi si rifiuta la nulla
# di radice unitaria e quindi non conta
# il lag length

# Punto b)

summary(test3) # tornano tutti i valori
               # Si rifiuta la nulla e quindi 
               # lo spread è stazionario


# Punto c)

r5= Dati[,5]
r5 = ts(r5)

testr5 = ur.df(r5, type = c("drift"), lags = 7)   
summary(testr5) # t =  -0.7849 r5 non è quindi stazionario

# Punto d)

bill= Dati[,3]
bill = ts(bill)

testbill = ur.df(bill, type = c("drift"), lags = 11)   
summary(testbill) # t =  -1.3443 bill non è quindi stazionario

# Punto e)

# Un'ipotesi può essere che il tbill dipende
# dal r5 o si muove insieme e varia "in ritardo" e quindi
# lo spread è come la differenze prima di uno stesso
# processo

# ALtra ipotesi è che la parte non stazionaria
# dei due processi è simile e quindi sottraendoli si 
# elimina la non stazionarietà


