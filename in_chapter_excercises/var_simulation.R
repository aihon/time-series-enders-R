#######################
#                     #
# Simulation of a VAR #
#                     #  
#######################

# Load libraries
library(dynlm)
library(tsDyn)
library(vars)
library(dse)
library(ggplot2)

#VAR simulation
Apoly <- array(c(1, -0.7, 0, -0.2, 0, -0.2, 1, -0.7), dim = c(2,2,2))
B <- diag(2)
TRD <- c(0,0)
var <- ARMA(A=Apoly, B=B, TREND = TRD)
varsim <- simulate(var)
vardat <- matrix(varsim$output, nrow = 100, ncol = 2)
colnames(vardat) <- c("y1","z1")
plot.ts(vardat, main = "", xlab = "")
y1 = vardat[,1]
plot(y1, type="l")
z1 = vardat[,2]
plot(z1, type="l")
ccf(y1, z1, type = c("correlation"), lag.max = 50)

# Plots

# Method one
plot(y1, type = "l")
lines(z1, col = "red")

# Method two
plot.zoo(cbind(y1, z1), 
         plot.type = "single", 
         col = c("red", "blue"))
