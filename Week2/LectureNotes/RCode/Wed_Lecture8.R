#Read in the population data
MyData <- read.csv("UK-Population.csv", header = TRUE, sep = ",", dec=".", skip=7)
population <- ts(MyData[,2],start=1971, end=2017, frequency =1)
par(mfrow=c(1,3))
plot(population, type = "l", pch=19)
plot(diff(population),type = "l", pch=19)
plot(diff(population,differences=2),type = "l", pch=19)

#Load the electricity load data
library(opera)
attach(electric_load)
LoadTS <-ts(Load,start=1996, frequency =52)

plot(stl(LoadTS, s.window="periodic"))

par(mfrow=c(2,3))
plot(LoadTS,type ="l", ylab="Weekly load in MW") #Plot the original time series
plot(diff(LoadTS,lag=52))#Plot the seasonal differenced time series
plot(diff(diff(LoadTS,lag=52)))#Plot the series after applying the lag-1 and lag-52 difference operators
#Plot the corresponding empirical autocorrelation functions:
acf(LoadTS)
acf(diff(LoadTS,lag=52))
acf(diff(diff(LoadTS,lag=52)))
detach(electric_load)
