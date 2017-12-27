#########################
#                       #
#     Excercise 8       #
#                       #
######################### 

# Load libraries
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

# Import the dataset.
dataset <- read_sas("/your_path/quarterly.sas7bdat")

# Form the spread.
spread = dataset[,5] - dataset[,3]
spread = ts(spread)
acfSpread = acf(spread)
acfSpread

# We use the model estimated to perform the Dickey-Fuller test by the "CADFtest" function to check how many lags indicates the
# AIC and the BIC
cadfAic = CADFtest(spread, type=c("drift"), max.lag.y=12, criterion=c("AIC"))
summary(cadfAic) # 9 lags
cadfBic = CADFtest(spread, type=c("drift"), max.lag.y=12, criterion=c("BIC"))
summary(cadfBic) # 1 lag

# We check how many lags select the general-to-specifc (GTS) method.
estimatedModel <- dynlm(d(s) ~ L(s, 1) + L(d(s), 1) + L(d(s), 2) +  L(d(s), 3) + L(d(s), 4)
                        + L(d(s), 5) + L(d(s), 6) + L(d(s), 7) + L(d(s), 8)) 
summary(estimated)    # the GTS method select 8 lags. 


# We try to perform the ADF (Augmented Dickey-Fuller) using the number of lags selected by the various methods in order to see
#if the lag lenght is relevant in the Dickey-Fuller test
adfOne = ur.df(spread, type = c("drift"), lags = 1)    # 1 lag,  t = -4.7505 
adfNine = ur.df(spread, type = c("drift"), lags = 9)   # 9 lags, t = -4.7026 
adfEight = ur.df(spread, type = c("drift"), lags = 8)  # 8 lags, t = -4.3657

# From summary(adfOne) we get the critical values. At the significance levels 1, 5 and 10 the critical values are, respectively:
# -3.46, -2.88 e -2.57. Hence, in all cases we reject the null of unit root and so the lag lenght it's not relevant.
summary(adfOne)

# Point (b)
summary(adfEight) # All values are in accords to the values of the book hence we reject the null of unit root and we can conclude 
                  # that the spread is stationary

# Point (c)
r5= Dati[,5]
r5 = ts(r5)
adfR5 = ur.df(r5, type = c("drift"), lags = 7)   
summary(adfR5) # t =  -0.7849 hence r5 is not stationary

# Punto (d)
bill= Dati[,3]
bill = ts(bill)
adfBill = ur.df(bill, type = c("drift"), lags = 11)   
summary(adfBill) # t =  -1.3443 hence bill is not stationary 
