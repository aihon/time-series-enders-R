###########################
#                         #
#      Excercise 11       #
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
library(pracma)
library(Quandl)

# Import the dataset 
dataset <- read_sas("/your_path/quarterly.sas7bdat")

# Point (a)
logRgdp = log(dataset$RGDP)
logRgdp = ts(logRgdp)
detrendedLogRgdp = residuals(lm(logRgdp ~ dataset$Date))
dat <- data.frame("Time"=dataset$Date,"Linearly.Detrended"=detrended)
ggplot(dat,aes(x=Time,y=Linearly.Detrended)) + geom_hline(yintercept=0,colour="grey80") + geom_line(size=.5) + theme_classic() 
                                             + labs(title="Linearly Detrended",y="")
acfDetrended = acf(detrended)

# Point (b)
dfTest = ur.df(detrended, type = c("trend"), lags = 0)
summary(dfTest) # tau3 = -0.6172 
                # phi3 = 4.2959 < valore critico (10%) = 5.47 hence I accept the null that the true modelis the one restricted
                # with a_{2} and \gamma set equal to 0  

# Point (c)
diffGDP = dataset$Potent - dataset$RGDP
adf.test(diffGDP, alternative = c("stationary")) # t = -2.353 hence I refuse the null of unit root at the 5% level of 
                                                 # signifiancy

# Point (d) 

ur.ers(dataset$RGDP, type = c("DF-GLS"))   # t-statistic = 2.6471
ur.ers(dataset$Potent, type = c("DF-GLS")) # t-statistic = 0.3197

