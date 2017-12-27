#######################
#
# Esercizio 11 Enders
#
#######################

# Si pulisce l'ambiente di lavoro

rm(list = ls(all=TRUE))

# Si leggono i dati

Dati.enders <- read_sas("/home/giovanni/Scrivania/SASDatasets/quarterly.sas7bdat")

# Si puliscono i dati eliminando ciò che non ci serve

Dati.enders[8:19] <- list(NULL) 
Dati.enders[2:6] <- list(NULL) 
plot(Dati.enders, type="l")

#Si estraggono i dati

time = Dati.enders[,1]
PPI = Dati.enders[,2]

# La serie è evidentemente non stazionaria
# va quindi differenziata.
# Si usa la differenza logaritmica:
# dly_t = log(ppi_t) − log(ppi_t−1)

plot(cbind(time, PPI), type="l")

# Si definisce come una serie storica

ppits <- ts(PPI)
plot(time, ppits)

# SI creano i logaritmi e la differenza
# come richiede l'esercizio
# diff mi fa la differenza tra il secondo valore
# della serie e il primo, tra il terzo e il secondo
# e così via

logppi = log(ppits)
logdiff = diff(logppi, lag=1, differences = 1)

acf(logdiff)
pacf(logdiff)

# La differenza logaritmica sembra essere 
# stazionaria

plot(logdiff)

# Si fittano i dati 
# con 3 modelli e si guarda quale ha
# l'in-sample fit migliore, ovvero quale
# fitta meglio i dati.
# Si parte stimando un AR(||1,3||),
# ovvero un AR(3) con il coefficiente del
# lag 2 forzato a zero dato che dalla PACF
# non sembra significativo


library(forecast)
AR_13 <- Arima(logdiff, order = c(3,0,0), fixed=c(NA,0,NA,NA), transform.pars = FALSE)
AR_13 # AIC = -1147.94 BIC= -1134.54

# Si prova ora a fittare i dati
# con un AR(3) e con un  ARMA(1, 1)
# Next, estimate the series as an ARMA(1, 1) process. Since the two models are
# to be compared head-to-head, they need to be estimated over the same sample
# period. The estimation for the ARMA(1,1) is constrained to begin on 1961:1
# (the first usable observation for the AR model with three lags).

AR_3 <- Arima(logdiff, order = c(3,0,0), fixed=c(NA,NA,NA,NA), transform.pars = FALSE)
AR_3 # AIC = -1150.93   BIC= -1134.46

ARMA_11 <- Arima(logdiff[5:211,], order = c(1,0,1), fixed=c(NA,NA,NA), transform.pars = FALSE)
ARMA_11 # AIC = -1125.39  BIC= -1112.06

# La BIC mi fa scelgliere l'AR(||1,3||)

# Adesso si confronta la performance dell'out-of-sample
# fit tra l'AR(||1,3||) e l'ARMA(1,1).
# Per fare questo si fittano i dati per un intervallo di tempo 
# che esclude le ultime 50 osservazioni.
# Poi si usa il modello stimato con le prime osservazioni
# per fare forecast per tutti i 50 periodi avanti.
# Si controlla poi quale dei due modelli fa forecast 
# che si discostano meno dai dati del sample usando la 
# forecast error function

# Abbiamo 211 osservazioni. SI stimano i modelli
# usando le prime 160 osservazioni

# AR(||1,3||) FORSE PRIMA DEL CICLO DEVI DEFINIRE: error <- 0

for (i in 1:51) {
  mod <- arima(window(logdiff, end=159+i),order=c(3,0,0),fixed=c(NA,0,NA,NA), transform.pars = FALSE)
  pre <- predict(mod,n.ahead=1)
  error[i] <- pre$pred - logdiff[159+i+1]
}
ear = error

# ARMA(1,1)

for (i in 1:51) {
  mod <- arima(logdiff[5:211,], order = c(1,0,1), fixed=c(NA,NA,NA), transform.pars = FALSE)
  pre <- predict(mod,n.ahead=1)
  error[i] <- pre$pred - logdiff[159+i+1]
}
earima = error

# Metodo alternativo (più breve) 

Arimast <- function(x, h){forecast(Arima(logdiff, order=c(1,0,1)), fixed=c(NA,NA,NA), transform.pars = FALSE, h=1)}
errorarima <- tsCV(logdiff, Arimast, h=1)

eredarima = errorarima[160:211,]

ARst <- function(x, h){forecast(Arima(logdiff, order=c(3,0,0)), fixed=c(NA,0,NA,NA), transform.pars = FALSE, h=1)}
errorar <- tsCV(logdiff, ARst, h=1)

eredar = errorar[160:211,]

# Diebold-Mariano test 
# leggi la docu con ?dm.test
# per vedere le ipotesi alternative

dm.test(ear, earima, alternative=c("two.sided"), power=1)

# p-value > 0.05 quindi non si rigetta la nulla che
# l'accuratezza dei due modelli sia uguale
# Altresì

qt(0.975, 49) # = 2. critical value t, 95% confidence, 2 sided, 49 df

# mentre il nostro test è 1,15. quindi non si 
# può rifiutare la nulla


# Si provano ora un AR(5) e un ARMA(2,1)

AR_5 <- Arima(logdiff, order = c(5,0,0), fixed=c(NA,NA,NA,NA,NA,NA), transform.pars = FALSE)
AR_5 # AIC = -1148.25 BIC= -1124.79

ARIMA_21 <- Arima(logdiff, order = c(2,0,1), fixed=c(NA,NA,NA,NA), transform.pars = FALSE)
ARIMA_21 # AIC = -1148.87 BIC= -1132.11

# Si testa la forecast accuracy

# AR5
Ar5st <- function(x, h){forecast(Arima(logdiff, order=c(5,0,0)), fixed=c(NA,NA,NA,NA,NA,NA), transform.pars = FALSE, h=1)}
errorar5 <- tsCV(logdiff, Ar5st, h=1)

eredar5 = errorar5[160:211,]

# ARIMA21

ARIMA_21 <- function(x, h){forecast(Arima(logdiff, order=c(2,0,1)), fixed=c(NA,NA,NA,NA), transform.pars = FALSE, h=1)}
errorarima21 <- tsCV(logdiff, ARIMA_21, h=1)

eredarima21 = errorarima21[160:211,]


# DM test confronto con AR(||1,3||)

dm.test(eredar, eredar5, alternative=c("two.sided"), power=1) # pvalue < 0.05 quindi si rigetta la nulla

dm.test(eredar, eredarima21, alternative=c("two.sided"), power=1) # pvalue < 0.05 quindi si rigetta la nulla













