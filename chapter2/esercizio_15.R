###############################
#                             #
#        Excercise 16         #
#                             #
###############################  

# Load libraries
library(forecast)
library(haven)
library(gdata)
library(lmtest)
library(qcc)

# We import the dataset and we take only what we need.
dataset <- read.xls("/your_path/y_break.38134409.xls", sheet=1, header = TRUE)
yBreak = dataset[,2]
yBreakTs = ts(yBreak)
plot(yBreakTs, type="l")

# We try to fit an AR(1) model.
arOne = Arima(yBreakTs, order = c(1,0,0), fixed = c(NA,NA))
arOne # AIC=473.16   BIC=482.19

# Dummy variables allow us to capture changes that we suppose have occurred in the process. If these changes are really occurred
# the modelling of the process have to consider it.
# Dummy variablese can assume values 0 or 1.
# In our case we think that a break have occurred in the period 101.
# We test if this break is an intercept and/or a slope break using dummy variables.
# In order to test a break in the intercept we use a dummy d_{t} that have value 0 before period 101 and value 101 from 101 
# onwards.
# If the coefficient of d_{t} is signficant it means that a break occurred (we sum the value of the coefficient of d_{t}
# to the value of the intercept).
# Instead, in order to test a break in the slope we use a dummy the d_{t} multiplied by y_{t-1}. If the coefficient of d_{t} is 
# signficant it means that a break in the slope occurred (we sum the value of the coefficient of d_{t} to the value of 
# coefficient y_{t-1}).

# Prima testo um break nell'intercetta

dummy = rep(1,150)
dummy[1:100]=0

breakint = Arima(ybreakts, order = c(1,0,0),  xreg=dummy)
breakint
coeftest(breakint)

# Si testa un break nell'intercetta e nell'AR coefficient
# del lag primo

nuova = lag(ybreakts)*dummy
B = matrix( c(dummy, nuova), nrow=length(dummy), ncol=2) 

breakintslo = Arima(ybreakts, order = c(1,0,0),  xreg=B)
breakintslo
coeftest(breakintslo)

# I coefficienti in entrambi i test sembrano
# significativi. Dunque c'è stato un break

# Estimate the series as an AR(2) process. In what sense does the AR(2) model perform
# better than the AR(1) model estimated in part a?

AR2 = Arima(ybreakts, order = c(2,0,0), fixed = c(NA,NA,NA))
AR2  # AIC=469.27   AICc=469.54   BIC=481.31

# L'AIC e il BIC sono inferiori per L'AR(2)

# Si plotta il valore del coefficiente ar1 stimando il modello
# come un AR ricorsivamente partendo da 1. Si vede che vi è un
# break 

coefficiente <- 0
for (i in 1:149) {
  mod <- arima(window(ybreakts, end=i+1),order=c(1,0,0),fixed=c(NA,NA))
  coefficiente[i] <-  mod$coef
}
plot(coefficiente, type="l")


# Si plotta il CUSUM di un AR(2)
# per vedere se è adeguato

error <- 0
for (i in 10:149) {
  mod <- arima(window(ybreakts, end=i),order=c(2,0,0),fixed=c(NA,NA,NA))
  pre <- predict(mod,n.ahead=1)
  error[i] <- ybreakts[i+1] - pre$pred 
}

sder = sd(error, na.rm = TRUE)

errornoNA = error[10:149]

sommacumu = cumsum(errornoNA)

CUSUM = sommacumu/sder 

plot(CUSUM, type ="l")
abline(h= 0, col="red")

# Sembra che anche un AR(2) non sia idoneo
  
  
  
  
