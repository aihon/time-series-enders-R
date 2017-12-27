#----------------#
# ESERCIZIO 11
#----------------#


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


# Dati

Dati <- read_sas("/home/giovanni/Scrivania/SASDatasets/quarterly.sas7bdat")
View(Dati)

# Punto a)

logrgdp = log(Dati$RGDP)
logrgdp = ts(logrgdp)

detrended = residuals(lm(logrgdp ~ Dati$Date))

dat <- data.frame("Time"=Dati$Date,"Linearly.Detrended"=detrended)
ggplot(dat,aes(x=Time,y=Linearly.Detrended)) + geom_hline(yintercept=0,colour="grey80") + geom_line(size=.5) + theme_classic() + labs(title="Linearly Detrended",y="")

acfdet = acf(detrended)

# Punto b)

dftest = ur.df(detrended, type = c("trend"), lags = 0)
summary(dftest) # tau3 = -0.6172, phi3 = 4.2959 

# phi3 = 4.2959 < valore critico (10%) = 5.47
# dunque accetto la nulla che il modello vero
# è quello ristretto con a2 e gamma uguale a zero

# Punto c)

diffGDP = Dati$Potent - Dati$RGDP

adf.test(diffGDP, alternative = c("stationary")) # t = -2.3533
                                                 # rifiuto nulla di radice unitaria al 5%
# Punto d) 

ur.ers(Dati$RGDP, type = c("DF-GLS")) # t-statistic = 2.6471
ur.ers(Dati$Potent, type = c("DF-GLS")) # t-statistic = 0.3197




