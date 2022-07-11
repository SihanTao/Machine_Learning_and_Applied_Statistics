library(forecast)
#Set the length of the time series
n <- 400
#Simulate an ARMA(1,1) process with standard normal white noise and phi=0.5, theta=0.3
set.seed(100)
# Simulate an ARMA(1,1) process with N(0,5^2) strict white noise
x <- arima.sim(model = list(ar = 0.5, ma = 0.3), n = n, sd=5)  
plot(x, type="l")
acf(x)

#Split the sample into a training and a test sample
length_test <- 30
length_training <- n-length_test
x.train <- window(x,start=1, end=length_training)
x.test <- window(x, start=length_training+1, end=n)

###Fit an AR(1), MA(1) and ARMA(1,1) process and check the residual
fit_ar1 <- Arima(x.train, order=c(1,0,0))
fit_ma1 <- Arima(x.train, order=c(0,0,1))
fit_arma11 <- Arima(x.train, order=c(1,0,1))
checkresiduals(fit_ar1)
checkresiduals(fit_ma1)
checkresiduals(fit_arma11)

#Note that you can plot the acf of the residuals also using the following command:
par(mfrow=c(1,1))
plot(acf(fit_arma11$residuals,lag=100,plot=F)[1:100])

###Predict from the three models
modelar1.pred <- predict(fit_ar1, n.ahead = length_test)$pred
modelma1.pred <- predict(fit_ma1, n.ahead = length_test)$pred 
modelarma11.pred <- predict(fit_arma11, n.ahead = length_test)$pred 

plot(modelar1.pred,col = "blue", lty = 2,lwd=2, ylab="Predictions")
lines(modelma1.pred, col = "black", lty = 3,lwd=2)
lines(modelarma11.pred, col = "red", lty = 1,lwd=2)
legend(x="bottomright", legend=c("AR(1)", "MA(1)", "ARMA(1,1)"),
       col=c("blue", "black", "red"), lty=c(2,3,1), lwd=2, cex=1.1)

#Plot the forecast from the ARMA(1,1) process including the prediction intervals (up to 99\% level)
plot(forecast(fit_arma11,h=length_test,fan=TRUE))
lines(x, col="black",lwd=1)

#Alternatively, you can specify the level of the prediction interval
plot(forecast(fit_arma11,h=length_test,level=0.95))
lines(x, col="black",lwd=1)

