# SVAR
#SVAR

#Load Packages
library(vars)
library(tseries)
library(tidyverse)
library(readxl)
library(zoo)
library(iRF)

#Load Data
germany <- read_excel("C:/Data/germany.xlsx")
View(germany)

#Set Time as Quarterly
germany$Quarter<-as.yearqtr(germany$Quarter)

#Rename
names(germany) <- c("Quarter", "GDP_Ger", "U_Ger")

#Take the Log
germany$yt_ger <- log(germany$GDP_Ger)

#Take First Difference
germany$xt_ger<-c(0,diff(germany$yt_ger,1))

#Unit Root Test - Augmented Dickey-Fuller Test
#Log
adf.test(germany$yt_ger) #stacionárius#Stationary

#Lag
adf.test(germany$xt_ger) #Stationary

#Deal with Unit Root (if needed)
france$xt_fr<-c(0,diff(france$xt_fr,1))
adf.test(france$xt_fr) #Stationary

#Unemployment
adf.test(germany$U_Ger) #Stationary

#New table with only necessary data
keeps<-c("U_Ger", "xt_ger")
germany=germany[keeps]

#Optimal Lag Length
VARselect(germany) #1

#Vector autoregression (VAR)
germany_var<-VAR(germany, p=10, type="none")
summary(germany_var) #correlation matrix of residuals is diagonal

a.mat <- diag(2)
diag(a.mat) <- NA
a.mat[2, 1] <- NA
print(a.mat)
b.mat <- diag(2)
diag(b.mat) <- NA
print(b.mat)

#Germany
svar.germany <- SVAR(germany_var, Amat = a.mat, Bmat = b.mat)

#(1) Delta(y) response to delta(y) shock
plot(irf(svar.germany, response = "xt_ger", impulse = "xt_ger", 
               n.ahead = 3, ortho = TRUE, boot = TRUE))
#(2) Delta(y) response to Ut shock
plot(irf(svar.germany, response = "xt_ger", impulse = "U_Ger", 
               n.ahead = 3, ortho = TRUE, boot = TRUE))

#(3) Ut response to delta(y) shock
plot(irf(svar.germany, response = "U_Ger", impulse = "xt_ger", 
               n.ahead = 3, ortho = TRUE, boot = TRUE))
#(4) Ut response to Ut shock
plot(irf(svar.germany, response = "U_Ger", impulse = "U_Ger", 
         n.ahead = 3, ortho = TRUE, boot = TRUE))

#Germany
model_Germany <- BQ(germany_var)
summary(model_Germany)
irf.dyt <- irf(model_Germany, impulse = "xt_ger", boot = FALSE, n.ahead = 3)
irf.unt <- irf(model_Germany, impulse = "U_Ger", boot = FALSE, n.ahead = 3)
gdp_Germany <- cbind(cumsum(irf.dyt$irf$xt_ger[, 1]), irf.dyt$irf$xt_ger[, 2])
unemp_Germany <- cbind(-1 * cumsum(irf.unt$irf$U_Ger[, 1]), -1 * irf.unt$irf$U_Ger[, 2])

#Supply shock
plot.ts(gdp_Germany[, 1], col = "black", lwd = 2, ylab = "", 
        xlab = "", xlim = c(1, 4), ylim = c(-0.4, 
                                                                    0.4))
lines(gdp_Germany[, 2], col = "blue", lwd = 2)
abline(h = 0)
legend(x = "bottomright", c("Output response", "Unemployment response"), 
       col = c("black", "blue"), lwd = 2, bty = "n")

#Demand shock
plot.ts(unemp_Germany[, 1], col = "black", lwd = 2, ylab = "", 
        xlab = "", xlim = c(1, 4), ylim = c(-0.8,
                                                                    0.8))
lines(unemp_Germany[, 2], col = "blue", lwd = 2)
abline(h = 0)
legend(x = "topright", c("Output response", "Unemployment response"), 
       col = c("black", "blue"), lwd = 2, bty = "n")
