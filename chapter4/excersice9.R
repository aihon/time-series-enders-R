##################
#                # 
#  Excersice 9   # 
#                # 
##################

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
library(car)

# Import the dataset 
dataset <- read_sas("/your_path/quarterly.sas7bdat")

# Point (c)
unemp = dataset$Unemp
unemp = ts(unemp)
estimateUnemp = dynlm(d(unemp) ~ L(unemp, 1) + L(d(unemp), 1)) 
summary(estimateUnemp)
attributes(estimateUnemp)
residualsEstimateUnemp = estimateUnemp$residuals
acfResiduals = acf(residuals)
acfResiduals

# Point (d)
indprod = dataset$IndProd
m1nsa = dataset$M1NSA
regressionIndprodM1nsa = lm(indprod ~ m1nsa)
summary(regressionIndprodM1nsa)
residuiRegressionIndprodM1nsa = regressionIndprodM1nsa$residuals
scatterplot(indprod, m1nsa)
