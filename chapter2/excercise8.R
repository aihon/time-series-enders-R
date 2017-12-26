#####################################
#            Excercise 8            #
#                                   #        
#    (Box - Jenkins methodology)    # 
#                                   #    
#####################################  

# Load libraries
library(haven)

# Import the dataset and visualize it
Dataset <- read_sas("/your_path/sim_2.sas7bdat")
View(Dataset)

# Take only the data you need
Dataset[3:4] <- list(NULL) 

# Plot the sequence Y1 against time. It seems  weakly stationary. In fact it fluctuates around a constant mean and the 
# variability appears to be regular.
Y1 <- Dataset[, 2]
time <- Dataset[, 1]
plot(time,Y1, type = "l")

# Try to spot the stationarity of Y1 by looking at the ACF and PACF. Both go to 0 very quickly. This tells us that the series
# can be stationary. (The blue lines give the values beyond which the autocorrelations are statistically significantly different
# from zero. These two lines gives the values of the confidence interval of Box-Jenkins statistics). 
# From the sample ACF and PACF you can also guess that the DGP is an AR(1). That's because in the ACF the autocorrelations go to
# 0 slowly and the first autocorreltions are significantly different from 0.
# In the PACF only the first partial autocorrelation is signifacntly different from 0. 
acf(x = Y1, lag.max = 30)
pacf(x = Y1, lag.max = 30)

# Now we look if an AR(1) model fit the data well or not. We estimate an AR(1) without intercept: 
#                                              y_(t) = a_(1) * y_(t-1) * e_(t)
# and one AR(1) with intercept:
#                                              y_(t) = a_(0) * y_(t-1) * e_(t).
# The s.e. of the intercept is big. Moreover, the AIC of the AR(1) without intercept is lower than the AIC of the AR(1) with
# the intercept, then we "choose" the AR(1) without intercept.

library(forecast)
ARwoi <- Arima(x, c(1,0,0), include.mean = FALSE) #senza intercetta
ARwi <- Arima(x, c(1,0,0))
ARwoi
ARwi


# Si scarta l'AR(1) con intercetta.
# Si procede ad una analisi dei residui
# per vedere se è rimasta in giro dell'informazione
# non catturata dal modello, ovvero se i residui sono white noise.
# Dalla ACF i residui sembrano whithe noise, quindi il modello
# fitta bene i dati. Perchè i residui devono essere withe noise/
#l'acf deve mostrare incorrelazione se un modello è ben specificato?:
# "It means you managed to extract all the information there is to extract.
# What's left is just random and not representable by a model. 
# If there is correlation in the residuals this regularity 
# can be further used to extract some more information."
# In alternativa usa i residui standardizzati, e plotta il grafico
# contro il tempo e vedi se stanno nell banda +2,-2 

Residui_woi <- residuals(ARwoi) 
acf(Residui_woi) 


# Si usa il Ljung-Box test per testare
# se un certo numero di lag non sono differenti da zero,
# ovvero non c'è autocorrelazione. 

Box.test(resid(ARwoi),type="Ljung",lag=8)
Box.test(resid(ARwoi),type="Ljung",lag=16)
Box.test(resid(ARwoi),type="Ljung",lag=24)


# Nella PACF sembra esserci diretta
# correlazione di x_t con il lag 12.
# Si prova quindi ad usare un AR(1)
# e a catturare questa correlazione con un coefficiente 
# MA. y_t = a_1*y_(t-) + e_t + \beta_(12)e_(t-12)
# Il valore stimato di \beta_(12) sta dentro
# la regione diaccettazione, quindi si accetta la nulla
# che è uguale a 0. Per calcolare il t-test
# si fa il valore di beta stimato meno zero diviso lo standard error.
# Il valore stimato sta dentro l'intervallo + o - la statistica t.

ARMA = arima(x , order=c(1,0,12), fixed=c(NA,0,0,0,0,0,0,0,0,0,0,0,0,NA))

# Si vede che l'AIC del modello
# AR(1) è inferiore a questo ARMA, quindi
# si sceglie l'AR(1)

ARMA
ARwoi

# PUNTO c)

AR_due = Arima (x, order=c(2,0,0), include.mean= FALSE)
resisui_ardue = residuals(AR_due)
Box.test(resid(AR_due),type="Ljung",lag=8)
Box.test(resid(AR_due),type="Ljung",lag=16)
Box.test(resid(AR_due),type="Ljung",lag=24)

# L'AIC dell'ARwoi è più piccolo
# di quello dell' AR_due

ARwoi
AR_due

# PUNTO d)

ARMA_unouno = Arima(x, order = c(1,0,1), include.mean = FALSE)
residui_armaunouno = residuals(AR_due)
acf(residui_armaunouno)
pacf(residui_armaunouno)



