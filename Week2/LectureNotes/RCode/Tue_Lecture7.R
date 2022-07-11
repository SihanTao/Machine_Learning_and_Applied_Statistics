#First we fix the seed of R's random number generator 
#so that we can generate a simulation which can be reproduced
set.seed(1) 
#Simulate 100 i.i.d N(0,4) variables
MyData <- rnorm(100,mean=0,sd=2)

mean(MyData) #Compute the sample mean
var(MyData) #Compute the sample variance (Recall the different scaling factor!)
summary(MyData) #Compute the summary statistics of the data

#Visualise the data:
par(mfrow=c(2,3))
#Plot the data
plot(MyData) 
boxplot(MyData,main="Boxplot")
library(vioplot)
vioplot(MyData,col="gold", main="Violin plot")
#Plot the histogram which depicts the number of times an
#observation falls into each of the bins
hist(MyData, main="Histogram")
#The following function scales the classical histogram
#such that it approximates a probability density
library(MASS)
truehist(MyData,main="Scaled/true histogram and kernel density")
#Add the so-called empirical density to the plot
lines(density(MyData),col="red",lwd=3)
library(car)
qqPlot(MyData, main="Quantile-quantile plot")

#Plot the sample autocorrelation of the data
par(mfrow=c(1,2))
acf(MyData)
#Remove the lag 0
plot(acf(MyData,plot=F)[1:20])

