#######################
#  Example  pag. 323  #
#######################

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

# Import the dataset 
dataset <- read.xls("/your_path/Enders_Holt.312103142.xls", header = TRUE)

# We select the lag lenght of the VAR
datasetM <- dataset
datasetM[,1] <- NULL
datasetM <- datasetM[-456,]
View(datasetM)
VARselect(datasetM, lag.max = 20) # AIC select 4 lags. Enders in the book use 7 lags.

# We estmate the VAR
varOne = VAR(datasetM, p=7)

# We apply restrictions to the matrix B, the matrix that multiplies the errors.
B <-diag(4)
diag(B) <- NA
B[4,1] = B[4,2] = B[4,3] = NA

# We estimate the structural VAR (SVAR)
sVarOne <- SVAR(varOne, estmethod = "scoring", Bmat = B, hessian = TRUE)
summary(sVarOne)
sVarOne$LR  # Chi^2 = 12.542, p-value = 0.005739
            # p < 0.05 hence we reject the restrictions

# Since the correlation between e_{2} and e_{3} is high we try the following restrictions
BOne <-diag(4)
diag(B1) <- NA
B1[4,1] = B1[4,2] = B1[4,3] = B1[2,3] = NA
BOne
sVarTwo <- SVAR(varOne, estmethod = "scoring", Bmat = BOne, hessian = TRUE)
summary(sVarTwo)
sVarTwo$LR # Chi^2 = 3.7186, p-value = 0.1558 hence restrictions are correct

# Conclusion: real grain prices are contemporaneously affected by all variables, and the real exchange rate is contemporaneously
# affected by real interest rate shocks. The innovations in real energy prices and interest rates are due to their own pure 
# shocks.

# Impulse response functions
irfSVarTwo = irf(sVarTwo, boot = FALSE)
plot(irfSVarTwo)

# Let's see how grain pg how respond
irfPg = irf(sVarTwo, response = "pg", boot = FALSE)
plot(irfPg)

# Variance decomposition
varDec = fevd(irfPg, n.ahead = 20)
plot(varDec)
