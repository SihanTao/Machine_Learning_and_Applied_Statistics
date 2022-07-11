library(forecast)
library(ghyp)
GOOG <- read.csv("GOOG.csv", header = TRUE, sep = ",", dec = ".")
GOOG_prices <-GOOG$Close #daily closing prices
#Plot the prices
plot(GOOG_prices,type="l")
#Detour: What kind of ARMA model could describe the prices?
#We see that prices could be modelled by a random walk:
auto.arima(GOOG_prices)

GOOG_logreturns <-diff(log(GOOG_prices))
#Compute the losses
GOOG_losses <- -GOOG_logreturns
plot(GOOG_losses, ylab="Google losses",type="l")

#Next, we compare the model fit for various models from the GH class and then safe the parameters for both the NIG and the Gaussian model.
a <- stepAIC.ghyp(GOOG_losses,silent=TRUE)
a

#Fit a VG distribution:
GOOG_VGfit <- fit.VGuv(GOOG_losses, silent = TRUE)
#Fit an NIG distribution:
GOOG_NIGfit <- fit.NIGuv(GOOG_losses, silent = TRUE)
#Fit a Gaussian distribution:
GOOG_gaussfit <- fit.gaussuv(GOOG_losses)

#Alternatively, you can get the fitted model parameters from the auto.arima function
#For the asymmetric VG, use:
a$all.models[[4]]
#For the asymmetric NIG, use:
a$all.models[[3]]
#For the Gaussian model, use:
a$all.models[[11]]

#Next, we compute the VaR for various confidence levels for both the 
#Gaussian and the NIG model and compare them:
 
###Compute Value at Risk
VaRlevels <- 1-c(0.1, 0.05, 0.01, 0.001)
#Compute the NIG and Gaussian quantiles:
GOOG_VG_VaR <- qghyp(VaRlevels,GOOG_VGfit)
GOOG_NIG_VaR <-qghyp(VaRlevels,GOOG_NIGfit)
GOOG_gauss_VaR <-qnorm(VaRlevels, mean=coef(GOOG_gaussfit)$mu, sd=coef(GOOG_gaussfit)$sigma)
#Plot the Gaussian, NIG  and VG quantiles:
barplot(rbind(GOOG_gauss_VaR, GOOG_NIG_VaR, GOOG_VG_VaR), beside = TRUE,
        names.arg = paste(100 * VaRlevels, "percent"), density=c(10,20,30) , angle=c(0,45,90), col = c("red", "blue", "green"),
        ylab = "VaR (loss distribution)", xlab = "Level")
legend("topleft", c("Gaussian", "NIG", "VG"), density=c(10,20,30) , angle=c(0,45,90),fill=c("red","blue", "green"))

#We do the same analysis for the corresponding expected shortfalls:
#Confidence levels for expected shortfalls:
ESlevels <- VaRlevels
GOOG_VG_ES <- ESghyp(ESlevels, GOOG_VGfit, distr="loss")
GOOG_NIG_ES <- ESghyp(ESlevels, GOOG_NIGfit, distr="loss")
GOOG_gauss_ES <- ESghyp(ESlevels, GOOG_gaussfit, distr="loss")

barplot(rbind(GOOG_gauss_ES, GOOG_NIG_ES, GOOG_VG_ES), beside = TRUE,
        names.arg = paste(100 * VaRlevels, "percent"), density=c(10,20,30) , angle=c(0,45,90), col = c("red", "blue", "green"),
        ylab = "ES (loss distribution)", xlab = "Level")
legend("topleft", c("Gaussian", "NIG", "VG"), density=c(10,20,30) , angle=c(0,45,90), fill=c("red","blue", "green"))

#Finally, we explore various graphics to display and compare the VaR and ES for our two models:
 
#You can graphically compare the levels of the VaR and ES for the three different fitted distributions using:
par(mfrow=c(2,1))
barplot(rbind(GOOG_gauss_VaR, GOOG_NIG_VaR, GOOG_VG_VaR), beside = TRUE,
        names.arg = paste(100 * VaRlevels, "percent"), density=c(10,20,30) , angle=c(0,45,90), col = c("red", "blue", "green"),
        ylab = "VaR (loss distribution)", xlab = "Level")
legend("topleft", c("Gaussian", "NIG", "VG"),density=c(10,20,30) , angle=c(0,45,90), fill=c("red","blue", "green"))
barplot(rbind(GOOG_gauss_ES, GOOG_NIG_ES, GOOG_VG_ES), beside = TRUE,
        names.arg = paste(100 * VaRlevels, "percent"), density=c(10,20,30) , angle=c(0,45,90), col = c("red", "blue", "green"),
        ylab = "ES (loss distribution)", xlab = "Level")
legend("topleft", c("Gaussian", "NIG", "VG"),density=c(10,20,30) , angle=c(0,45,90), fill=c("red","blue", "green"))

#Next we plot the VaR and ES for the VG and Gaussian distributions in one picture:
par(mfrow=c(1,1))
barplot(rbind(GOOG_gauss_VaR, GOOG_gauss_ES, GOOG_VG_VaR, GOOG_VG_ES), beside = TRUE,
        names.arg = paste(100 * VaRlevels, "percent"), density=c(10,20, 30, 40) , angle=c(0,45,90,135), col = c("red", "blue", "green", "grey"),
        ylab = "VaR and ES (loss distribution)", xlab = "Level")
legend("topleft", c("Gaussian VaR", "Gaussian ES", "VG VaR", "VG ES"),density=c(10,20, 30, 40) , angle=c(0,45,90,135), fill=c("red","blue", "green", "grey"))

#Finally we indicate the VaR and ES levels w.r.t the empirical density:
library(MASS)
truehist(GOOG_losses, main="Empirical density and true histogram of the losses", xlab="")
lines(density(GOOG_losses))
abline(v=GOOG_gauss_VaR[3],col="red",lwd=2)
text(GOOG_gauss_VaR[3], 30, "Gaussian VaR 99%", col = "red") 
abline(v=GOOG_NIG_VaR[3],col="green",lwd=2)
text(GOOG_NIG_VaR[3], 20, "NIG VaR 99%", col = "green") 
abline(v=GOOG_VG_VaR[3],col="grey",lwd=3)
text(GOOG_VG_VaR[3], 20, "VG VaR 99%", col = "grey") 

