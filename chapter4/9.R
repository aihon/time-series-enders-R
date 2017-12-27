#----------------#
#  ESERCIZIO 9
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

# Dati

Dati <- read_sas("/home/giovanni/Scrivania/SASDatasets/quarterly.sas7bdat")
View(Dati)

# Punto c)

unemp = Dati$Unemp
unemp = ts(unemp)

stimaunemp = dynlm(d(unemp) ~ L(unemp, 1) + L(d(unemp), 1)) 
summary(stimaunemp)

attributes(stimaunemp)
residui = stimaunemp$residuals
acfresidui = acf(residui)

acfresidui

# Punto d)

indprod = Dati$IndProd
m1nsa = Dati$M1NSA

regression = lm(indprod ~ m1nsa)
summary(regression)

residui_reg = regression$residuals

scatterplot(indprod, m1nsa)


