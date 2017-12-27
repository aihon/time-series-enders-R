#####################################
#            Excercise 8            #
#                                   #        
#    (Box - Jenkins methodology)    # 
#                                   #    
#####################################  

# Load libraries
library(haven)
library(forecast)

# Import the dataset and visualize it
dataset <- read_sas("/your_path/sim_2.sas7bdat")
View(dataset)

# Take only the data you need
dataset[3:4] <- list(NULL) 

# Plot the sequence Y1 against time. It seems  weakly stationary. In fact it fluctuates around a constant mean and the 
# variability appears to be regular.
y1 <- dataset[, 2]
time <- dataset[, 1]
plot(time,Y1,type = "l")

# Try to spot the stationarity of Y1 by looking at the ACF and PACF. Both go to 0 very quickly. This tells us that the series
# can be stationary. (The blue lines give the values beyond which the autocorrelations are statistically significantly different
# from zero. These two lines gives the values of the confidence interval of Box-Jenkins statistics). 
# From the sample ACF and PACF you can also guess that the DGP is an AR(1). That's because in the ACF the autocorrelations go to
# 0 slowly and the first autocorreltions are significantly different from 0.
# In the PACF only the first partial autocorrelation is signifacntly different from 0. 
acf(x = Y1,lag.max = 30)
pacf(x = Y1,lag.max = 30)

# Now we look if an AR(1) model fit the data well or not. We estimate an AR(1) without intercept: 
#                                              y_{t} = a_{1} * y_{t-1} * e_{t}
# and one AR(1) with intercept:
#                                              y_{t} = a_{0} * y_{t-1} * e_{t}.
# The s.e. of the intercept is big. Moreover, the AIC of the AR(1) without intercept is lower than the AIC of the AR(1) with
# the intercept, then we "choose" the AR(1) without intercept.

ARwoi  <- Arima(Y1,c(1,0,0),include.mean = FALSE) # AR(1) without intercept
ARi <- Arima(Y1,c(1,0,0))
ARwoi
ARi

# We procede to do residual analysis to check if some information of the process is not captured by our model, that is,
# we check if the residuals are white noise (we hope yes).
# From ACF of residuals they seems white noise and so we can assert that our model fit well the data.
# The following test is an explanation of why we want residuals to be white noise:
# "It means you managed to extract all the information there is to extract. What's left is just random and not representable
# by a model. If there is correlation in the residuals this regularity can be further used to extract some more information."
# (In alternative we can plot the standardized residuals and look if they stay into the +2,-2 band of cofidence.
residualsARwoi <- residuals(ARwoi) 
acf(Residui_woi) 

# We use the Ljung-Box test to test if a certain number of lags are different from 0, that is, if there isn't correlation 
#between lags.
Box.test(resid(ARwoi),type="Ljung",lag=8)
Box.test(resid(ARwoi),type="Ljung",lag=16)
Box.test(resid(ARwoi),type="Ljung",lag=24)

# In the PACF it's seems that that the partial autocorrelation at lag 12 is significantly different from 0. We try to capture
# such correlation introducing an MA coefficient in the AR(1):
#                                           y_{t} = a_{1} * y_{t-1} + e_{t} + beta_{12} * e_{t-12}
# The estimated value of beta_{12} lie inside the acceptation region, then we accept the null hypothesis that it's equal to 0.
# (The t-statistics is: t_{\hat{\beta_{12}} = \frac{\hat{beta_{12}} - 0}{s.e.(\hat{beta_{12}}}. The estimated value lie inside
# the interval [-t_{\hat{\beta_{12}};+t_{\hat{\beta_{12}}.]
ARMA = arima(Y1,order=c(1,0,12),fixed=c(NA,0,0,0,0,0,0,0,0,0,0,0,0,NA))

# The AIC of the ARwoi is less than the AIC of the ARMA, then we select the AR(1)
ARMA
ARwoi

# Solution of point (c)
ARwoi_two = Arima(Y1,order=c(2,0,0),include.mean= FALSE)
residualsARwoi_two = residuals(ARwoi_two)
Box.test(resid(ARwoi_two),type="Ljung",lag=8)
Box.test(resid(ARwoi_two),type="Ljung",lag=16)
Box.test(resid(ARwoi_two),type="Ljung",lag=24)

# The AIC of the ARwoi is less than the AIC of the ARwoi_two. The we select the ARwoi
ARwoi
ARwoi_two

# Solution of point (d)
ARMA_oneone = Arima(Y1,order = c(1,0,1),include.mean = FALSE)
residualsARMA_oneone = residuals(ARMA_oneone)
acf(residualsARMA_oneone)
pacf(residualsARMA_oneone)



