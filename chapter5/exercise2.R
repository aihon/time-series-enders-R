###########################
#                         #
#      Excercise 2        #
#                         #
###########################

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
library(TSA)

# Import the dataset, take what is reqeusted from the excercise and visualize it
dataset <- read.xls("/your_path/Terrorism.38140308.XLS", header = TRUE)
View(dataset)
trans = dataset$Transnational
trans = ts(trans)
plot(trans, type = "l")

# Point (a)
# In order to find the model for "trans" with the best fit we use the pre or post intervention interval with more observations.
# In this case the period with more observations is the pre-intervention period.
transPre = ts(trans[0:112])      # 112 osservazioni
transPost = ts(trans[113:164])   # 51 osservazioni
acf(transPre)
pacf(transPre) # It seems an AR(2) process
plot(transPre)
unitRootTransPre = ur.df(transPre, type = c("drift"))
summary(unitRootTransPre) # We reject the null of non stationarety 
arTwo = Arima(trans1, order = c(2,0,0))
plot(arTwo$residuals)
acf(arTwo$residuals)
pacf(arTwo$residuals)
Box.test(arTwo$residuals)  # p-value = 0.9426 > 0.05 hence we "accept" the null that residuals are white noise

# Point (b)
dummyOne = rep(0,164)
dummyOne[114:164]=1
modelOne = lm(trans ~ dummyOne)
summary(modelOne)
modelTwo = dynlm(trans ~  L(trans, 1) + L(trans, 2) + dummyOne)
summary(modello2)
AIC(modelOne) # 1277.843
BIC(modelOne) # 1287.142
plot(modelOne$residuals, type ="l")
acf(modelOne$residuals)
pacf(modelOne$residuals)
Box.test(modelOne$residuals)
AIC(modelTwo) # 1181.024
BIC(modelTwo) # 1196.462
plot(modelTwo$residuals, type ="l")
acf(modelTwo$residuals)
pacf(modelTwo$residuals)
Box.test(modelTwo$residuals)

# We select the model two 
