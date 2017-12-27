###############################
#
#
#     Esercizo 14 
#
#
###############################

# Si pulisce il workspace

rm(list = ls(all=TRUE))

# Librerie

library(forecast)
library(haven)


# Si importano i dati

Dati <- read_sas("/home/giovanni/Scrivania/SASDatasets/quarterly.sas7bdat")
View(Dati)


# Si forma lo spread come r5 - Tbill

spread = Dati[,5] - Dati[,3]
spread <- ts(spread)

# Si stima un AR(7) e un ARMA(1,1)

AR_7 <- Arima(spread, order = c(7,0,0), fixed=c(NA,NA,NA,NA,NA,NA,NA,NA))
AR_7   # AIC=289.32      BIC=319.53

ARMA_11<- Arima(spread, order = c(1,0,1), fixed=c(NA,NA,NA))
ARMA_11 # AIC=293.5  BIC=306.92

# Si stimano l' AR(7) e l' ARMA(1, 1) over the period 1960Q1–2000Q3.
# Si usano quindi le righe 1:163

AR_7red <- Arima(spread[1:163,], order = c(7,0,0), fixed=c(NA,NA,NA,NA,NA,NA,NA,NA))
AR_7red   # AIC=235.74   BIC=263.59


ARMA_11red <- Arima(spread[1:163,], order = c(1,0,1), fixed=c(NA,NA,NA))
ARMA_11red # AIC=241.56   BIC=253.93


# Si fa out-of-sample forecast con rolling origin
# e si forma il forecast error

# AR(7)

error <- 0
forecastar <- 0
for (i in 1:49) {
  mod <- arima(window(spread, end=162+i),order=c(7,0,0),fixed=c(NA,NA,NA,NA,NA,NA,NA,NA))
  pre <- predict(mod,n.ahead=1)
  error[i] <- spread[163+i] - pre$pred 
  forecastar[i] = pre$pred
}
ear = error

MSPEar = mean(ear^2,na.rm=TRUE)


# AR(7): Metodo alternativo

k <- 163 # minimum size for training set
n <- length(spread) # Total number of observations
e <- spread*NA # Vector to record one-step forecast errors
for(i in 163:(n-1))
{
  train <- ts(spread[1:i],freq=1)
  fit <- arima(train, order=c(7,0,0),fixed=c(NA,NA,NA,NA,NA,NA,NA,NA))
  fc <- forecast(fit,h=1)$mean
  e[i] <- spread[i+1]-fc
}

# ARMA(1,1)

error <- 0
forecastarima <- 0
for (i in 1:49) {
  mod <- arima(window(spread, end=162+i),order=c(1,0,1),fixed=c(NA,NA,NA))
  pre <- predict(mod,n.ahead=1)
  error[i] <- spread[163+i] - pre$pred 
  forecastarima[i] <- pre$pred
}
earima = error

MSPEarima = mean(earima^2,na.rm=TRUE)

# Dal MSPE pare che l'arima sia migliore

# Si guarda se i forecast sono unbiased o no

lrar = lm(spread[164:212,] ~ forecastar)
summary(lrar)  

lrarima = lm(spread[164:212,] ~ forecastarima)
summary(lrarima)

# Ora si devono testare le restrizioni 
# i) intercetta = 0 ii) slope = 1 per
# vedere se modello unbiased o no
# ma non so come fare


# Si calcola il DM test usando come
# loss function il MAE (mean absolute error)

MAEar <- mean(abs(ear),na.rm=TRUE)
MAEarima <- mean(abs(earima),na.rm=TRUE)

# Non sono sicuro sia corretto

# newvalu = abs(earts) - abs(earimats)
# earts = ts(ear)
# earimats = ts(earima)
# meanloss = mean(newvalu)
# varloss = var(newvalu)
# DMtest = meanloss/varloss

# Fare il Granger-Newbold test

# DA FARE

# Construct the ACF and PACF of the first difference of the spread. What type of model is
# suggested?

spreaddiff = diff(spread)
View(spreaddiff)

acf(spreaddiff)
pacf(spreaddiff)

bettermodeldiffspread = auto.arima(spreaddiff)
bettermodeldiffspread

# Show that an ARMA(2,||3,8||) has a better fit than any of the
# models reported in the text. What do you think about such a model?

ARMA_238 <- Arima(spread, order = c(2,0,8), fixed=c(NA,NA,0,0,NA,0,0,0,0,NA,NA))
ARMA_238 # AIC=279.44   BIC=299.58

bettermodeldspread = auto.arima(spread)
bettermodeldspread # AIC=290.82   BIC=314.32
 
