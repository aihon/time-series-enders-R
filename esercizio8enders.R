###################################
#                                 #    
#                                 #        
#    Box - Jenkins methodology    #
#                                 #
# data from: sim_2 da Enders      #
#                                 #    
#                                 #
###################################  

# Si importano i dati dal file sim_2.sas7bdat 
# che è un formato per sas. Poi si visualizza

library(haven)
Dati.enders <- read_sas("/home/giovanni/Scaricati/SASDatasets/sim_2.sas7bdat")
View(Dati.enders)

# Nell'esercizio 8 pag 114 di Enders ci serve solo la prima serie,
# quindi si eliminano i dati in più (potremmo anche evitare, ma per esercizio)
# e si controlla se è giusto

Dati.enders[3:4] <- list(NULL) 
View(Dati.enders)

# Si plotta la sequenza Y1 contro il tempo
# e sembra stazionaria in senso debole because 
#it fluctuates around a constant mean and the variability appears to
#be regular.

x <- Dati.enders[, 2]
time <- Dati.enders[, 1]
plot(time,x, type = "l")


# Si controlla la stazionarietà dall ACF
#e dalla PACF
#Entrambe vanno a zero veloce, cosa che suggerisce
#la stazionarietà della serie.
# (The blue lines give the values beyond which the autocorrelations
#are (statistically) significantly different from zero. 
#Your ACF seems to indicate seasonality. Credo che le 
# linee blu trattegiate siano i valori dell'intervalllo
#  di confidenza della statistica test di Box and Jenkins
# Dalla sample ACF e PACF si può anche ipotizzare
# che il DGP sia un AR(1), dato che nella ACF
# la correlazione tra i lag va a zero e le prime 
# autocorrelazioni sono significativamente diverse da 0.
# Nella pacf c'è solo la prima
# autocorrelazione parziale significativa

acf(x = x, lag.max = 30)
pacf(x = x, lag.max = 30)

# Vediamo se un modello AR(1) fitta bene i dati.
# Si stima quindi un AR(1): y_t = a_1*y_t-1 + e_t
# e uno con intercetta Lo s.e. dell'intercetta è
# alto e anche l'AIC di quello senza intercetta
# è più basso, quindi si "sceglie" quello senza.

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



