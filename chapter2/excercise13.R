##########################
#                        #  
#     Excercise 13       #  
#                        #  
##########################

# Load libraries
library(haven)
library(forecast)

# Import the dataset and visualize it.
dataset <- read_sas("/your_path/quarterly.sas7bdat")
View(dataset)

# We take the data we need.
indProd <- dataset[,15] 
View(indProd)

# We generate the growth rate of the series as:
#                                 indProd_{t} = log(indProd_{t}) − log(indprod_{t−1})

indprod <- ts(indprod)
plot(indprod)

indprodlog = log(indprod)
plot(indprodlog)

logdiff = diff(indprodlog, lag = 1,  differences = 1)
plot(logdiff)

acf(logdiff)

# Si stima un AR(1) con intercetta

AR_1 <- Arima(logdiff, order = c(1,0,0), fixed=c(NA,NA))
AR_1 # AIC=-1237.08    BIC=-1227.02
coeftest(AR_1)

# Si stima ora un AR(||1,8||), e dalla aic e
# dalla bic si vede che fitta meglio

AR_18 <- Arima(logdiff, order = c(8,0,0), fixed=c(NA,0,0,0,0,0,0,NA,NA))
AR_18 # AIC=-1241.39  BIC=-1227.98

# Si usa unemp

urate <- Dati[,14]
urate <- ts(urate)

plot(urate)  # La media potrebbe non essere costante 
acf(urate)   # Cade a zero dopo un po' 
             # Ci sta quindi che la ts non sia stazionaria

# Si stima come un AR(2) con intercetta

AR_2 <- Arima(urate, order = c(2,0,0), fixed=c(NA,NA,NA))
AR_2 # AIC=19.08   BIC=32.51


# Per il punto iii) si definisce l'eq omogenea.
# Si trovano poi le radici caratteristichje che 
# vengono complesse coniugate.
# Poi siccome (a_2)^0.5 è minore di 1 il processo 
# converge


# Si modella la differenza prima del processo
# come un AR(1). 

uratediff = diff(urate, lag=1, differences = 1)
plot(uratediff)

AR_1_diff = Arima(urate, order = c(1,0,0), fixed=c(NA,NA))
AR_1_diff # AIC=147.92   BIC=157.99

# Si usa CPICORE

cpicore = Dati[,10]
cpicore = ts(cpicore)

# Si forma dly_t = log(cpicore_t) − log(cpicore_t−1)

cpicorelog = log(cpicore)
cpicorediff = diff(cpicorelog, lag=1, differences = 1)

plot(cpicorediff)
acf(cpicorediff)   
pacf(cpicorediff)

# La ACF non cade a 0. C'è quindi il caso che  la serie 
# sia non stazionaria. Si prova a ridifferenziare

cpicorediff2 = diff(cpicorediff, lag=1, differences = 1)

plot(cpicorediff2)
acf(cpicorediff2) # Ora è stazionaria la serie diff2
pacf(cpicorediff2)

# Si guarda se per diff2 fitta meglio un AR(1) o un MA(1)

AR_1_cpidiff2 = Arima(cpicorediff2, order = c(1,0,0), fixed=c(NA,NA))
AR_1_cpidiff2 # AIC=-1999.41    BIC=-1989.37

MA_1_cpidiff2 = Arima(cpicorediff2, order = c(0,0,1), fixed=c(NA,NA))
MA_1_cpidiff2 # AIC=-2000.39   BIC=-1990.35

# Dalla aic e della bic si vede ce l'in-sample forecast è
# più accurato quello dell'MA(1)

# Si stima cpicorediff come un AR(2)

AR_2_cpidiff = Arima(cpicorediff, order = c(2,0,0), fixed=c(NA,NA,NA))
AR_2_cpidiff

# Le seguenti non sono le forecast h=12 per cpicore
# vanno trasformate ma non so come fare

FORECASTAR = forecast(AR_2_cpidiff,h=12)$mean
FORECASTMA = forecast(MA_1_cpidiff2,h=12)$mean 









