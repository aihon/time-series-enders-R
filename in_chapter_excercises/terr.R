#################################
#                               #
# Excercise terrorism pag. 310  #
#                               #
#################################

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
library(pracma)
library(Quandl)
library(vars)
library(dynlm)
library(tsDyn)
library(dse)


# Import the dataset and organize it
dataset <- read.xls("/your_path/Terrorism.38140308.XLS")
newDataset <- Dati[37:164,]
View(newDataset)
plot(newDataset$Domestic, type = "l")
plot(newDataset$Transnational, type = "l")

# Since we want to test Granger causality both of "domestic" on "transnational" and "transnational" on "domestic" we want first
# see if the two series are stationary. In order to do that we perform a DF (Dickey-Fuller) test and a ERS (from Elliott, 
# Rothenberg, and Stock [1996]) test generally known as Dickey–Fuller generalized least squares (DF-GLS) test.
# First we have to select the lag lenght of the model and to do that we can use the general-to-specific method or the
# function AIC() and BIC() applied at the models estimated with different lag lenght and choose the model that have the smaller
# value of taht functions.

# DF and DF-GLS for Domestic
dfDom = ur.df(newDataset$Domestic, type = c("drift"),  lags = 2)            # -2.5144
summary(dfDom) # "accept"
ersDom = ur.ers(newDataset$Domestic, type = c("DF-GLS"), lag.max = 2)       # -2.4703 
summary(ersDom) # reject

# DF and DF-GLS on Trans
dfTrans = ur.df(newDataset$Transnational, type = c("drift"), lags = 1)       # -2.6867
summary(dfTrans) # reject
ersTrans = ur.ers(newDataset$Transnational, type = c("DF-GLS"), lag.max = 1) # -2.5164 
summary(ersTrans) # reject

# Now we want to test the Granger causality in both directions.
# To do that we have to select the lag lenght of the VAR.
# (To test restriction on coefficients use the function restrict())
nnewDataset[,1] <- NULL
View(nnewDataset)
dom = nnewDataset$Domestic
tran = nnewDataset$Transnational
VARselect(nnewDataset) # AIC select 3 lags
var = VAR(nnewDataset, p = 3)
var

# We want to test if "dom" Granger cause "tran", that is, if if A_{21} = 0
causality(var, cause = "Domestic")$Granger       # F = 3.9797
                                                 # p = 0.008597 < 0.05 reject the null hence "dom" Granger cause "tran" 
                                                 # la nulla e quindi dom GC tra

# We want to test if "tran" Granger cause "com", that is, if if A_{12} = 0
causality(var, cause = "Transnational")$Granger  # F = 1.9627
                                                 # p = 0.1203 > 0.05 accept the null hence "tran" don't Granger cause "dom"
                                                

# Variance decomposition
graphics.off()
par(mar = rep(2, 4))
d12 = fevd(var, n.ahead = 12)
plot(d12)
d12

# Impulse response functions
irfOne = irf(var, n.ahead = 20)
plot(irfOne)
