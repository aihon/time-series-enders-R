####################
##
##  SIMULATE VAR 
##
####################


# Libraries

library(dynlm)
library(tsDyn)
library(vars)
library(dse)
library(ggplot2)


# Metodo 1. Mio tentativo, non so se è giusto

e1 = rnorm(100)
e2 = rnorm(100)

y = rep(NA,100)
y[1] = 0

z = rep(NA,100)
z[1] = 0


for(i in 2:100){
  y[i] = 0.7*y[i-1] + 0.2*z[i-1] + e1[i]
  z[i] = 0.7*y[i-1] + 0.2*z[i-1] + e2[i]
}

plot(y, type = "l")
plot(z, type = "l")

ccf(y,z, type = c("correlation"), lag.max = 50)

# Metodo 2

Apoly <- array(c(1, -0.7, 0, -0.2, 0, -0.2, 1, -0.7), dim = c(2,2,2))
Apoly

B <- diag(2)

TRD <- c(0,0)

var <- ARMA(A=Apoly, B=B, TREND = TRD)

varsim <- simulate(var)
varsim

vardat <- matrix(varsim$output, nrow = 100, ncol = 2)
colnames(vardat) <- c("y1","z1")

plot.ts(vardat, main = "", xlab = "")

y1 = vardat[,1]
plot(y1, type="l")

z1 = vardat[,2]
plot(z1, type="l")

ccf(y1, z1, type = c("correlation"), lag.max = 50)

# Plots

#1
plot(y1, type = "l")
lines(z1, col = "red")

#2
plot.zoo(cbind(y1, z1), 
         plot.type = "single", 
         col = c("red", "blue"))


