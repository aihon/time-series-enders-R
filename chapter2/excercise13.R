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

indProd <- ts(indProd)
indProdLog = log(indProd)
diffIndProdLog = diff(indProdLog, lag = 1,  differences = 1)

# We look at the ACF of the differencieted series. It seems an AR(1) process.
acf(diffIndProdLog)

# We estimate an AR(1) with intercept.
arOne <- Arima(diffIndProdLog, order = c(1,0,0), fixed=c(NA,NA))
arOne # AIC=-1237.08    BIC=-1227.02
coeftest(arOne)

# We estimate an AR(||1,8||). From the AIC and the BIC we see that the AR(||1,8||) fit better than the AR(1).
arOneEight <- Arima(diffIndProdLog, order = c(8,0,0), fixed=c(NA,0,0,0,0,0,0,NA,NA))
arOneEight # AIC=-1241.39  BIC=-1227.98

# We extract the unemployment rate
unempRate <- dataset[,14]
unempRate <- ts(urate)
plot(unempRate)  # the mean may not be constat 
acf(unempRate)   # Go to 0 not immediatley, then unempRate may not be stationary  

# We try to fit unempRate with an AR(2) with intercept.
arTwo <- Arima(unempRate, order = c(2,0,0), fixed=c(NA,NA,NA))
arTwo # AIC=19.08   BIC=32.51


# We take the first difference of unempRate to make it stationary. Then we model it as an AR(1).
diffUnempRate = diff(unempRate, lag=1, differences = 1)
plot(diffUnempRate)
arOneDiff = Arima(unempRate, order = c(1,0,0), fixed=c(NA,NA))
arOneDiff # AIC=147.92   BIC=157.99

# We extract CPICORE
cpicore = dataset[,10]
cpicore = ts(cpicore)

# We form:
#                     dl(cpicore_{t}) = log(cpicore_{t}) − log(cpicore_{t−1})
# The ACF of the log difference doesn't go to 0 quickly. Then unempRate may not be first difference stationary.
cpicoreLog = log(cpicore)
cpicoreDiff = diff(cpicoreLog, lag=1, differences = 1)
plot(cpicoreDiff)
acf(cpicoreDiff)   
pacf(cpicoreDiff)

# We try to take the second difference.
cpicoreDiffTwo = diff(cpicoreDiff, lag=1, differences = 1)
plot(cpicoreDiffTwo)
acf(cpicoreDiffTwo) # unempRate is second difference stationary 
pacf(cpicoreDiffTwo)

# We check if, for cpicoreDiffTwo, fit better an AR(1) or an MA(1).
# From the AIC and the BIC we see that the in-sample forecast is more accurate the one of MA(1).
arOneCpicoreDiffTwo = Arima(cpicorediff2, order = c(1,0,0), fixed=c(NA,NA))
arOneCpicoreDiffTwo # AIC=-1999.41    BIC=-1989.37
maOneCpicoreDiffTwo = Arima(cpicorediff2, order = c(0,0,1), fixed=c(NA,NA))
maOneCpicoreDiffTwo # AIC=-2000.39   BIC=-1990.35

# We estimate fit an AR(2) for cpicoreDiff 
arTwoCpicoreDiff  = Arima(cpicorediff, order = c(2,0,0), fixed=c(NA,NA,NA))











