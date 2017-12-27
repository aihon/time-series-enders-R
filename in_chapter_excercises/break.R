#-----------------------------#
#                             #
#        ESERCIZIO            #
#      PERRON's TEST          #
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
library(urca)
library(tseries)


# Dati

Dati <- read_sas("/home/giovanni/Scrivania/SASDatasets/break.sas7bdat")
View(Dati)

y1 = Dati[,2]
y1 = ts(y1)

plot(y1)

acf(y1) # sembra proprio un unit root process
        # ma sappiamo che non lo è

dy1= diff(y1)
plot(dy1)
acf(dy1)

# DF test. sembrano tutte serie diffference stationary, ovvero
# che sono integrate di ordine 1, ovvero che hanno una radice unitaria.
# sappiamo che non lo sono ma sappiamo anche che il DF test è 
# riluttante a rigettare la nulla di unit root in presenza di break.
# Il DF test ci fa in tutti e tre i casi non rigettare 
# la nulla di unit root. Confronta le t-statistic
# con i valori critici

# Random walk
test = ur.df(y1, type = c("none"), selectlags = c("BIC")) #BIC seleziona 1 lag

# Random wlak plus drift
test = ur.df(y1, type = c("drift"), selectlags = c("BIC")) #BIC seleziona 1 lag

# Random walk plus drift e linear time trend
adf.test(y1, alternative = c("s"), k = 1)

# Vediamo se servono più lags nei modelli specificati
# per il rw puro

ols = lm(diff(y1) ~ lag(y1)[1:99,] -1)
summary(ols)

# I residui sembrano normali quindi lag length ok
plot(density(resid(ols)))
qqnorm(resid(ols))
qqline(resid(ols))


# Vediamo se servono più lags nei modelli specificati
# per il rw plus drift e linear time trend

y1t = y1[1:99,]
olsdt = lm(diff(y1) ~ lag(y1)[1:99,] + seq_along(y1t))
summary(olsdt)


plot(density(resid(olsdt)))
qqnorm(resid(olsdt))
qqline(resid(olsdt))

# Siccome il DF test non funge:
# Si fa il test di Perron (1989) 
# di radice unitaria con presenza di break.
# La ipotesi nulla è che ci sia radice unitaria 
# in presenza di break, l'alternativa è che ci sia un trend stazionario
# e un break.
# SI testa ora che ci sia stato un cambiamento sia
# nell'ntercetta 

# NON TORNAAAAAAAAAAAAAAAAAAAAAAA
# vedi valori dummies


dummy1 = rep(1,100)
dummy1[1:50]=0

dummy2 = rep(0,100)
dummy2[51]=2.5

olstest = lm(y1 ~ lag(y1) + seq_along(y1) + dummy1 + dummy2)
summary(olstest)

