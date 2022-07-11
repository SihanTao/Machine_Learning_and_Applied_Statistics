###Examples of time series data
#Set the working directory to the folder where your datasets are stored
###
#Example 1: Exchange rate data from the Bank of England
#https://www.bankofengland.co.uk/boeapps/database/Rates.asp?Travel=NIxAZx&into=GBP

#Read in the data:
MyData <- read.csv("EUR-GBP-Rates.csv", header = TRUE, sep = ",", dec = ".",fileEncoding="UTF-8-BOM")

#View the data
View(MyData)

#Print the first part of the data
head(MyData)

#Print the last part of the data
tail(MyData)

#Convert the dates into the correct date format
MyDates <- as.Date(MyData$Date, "%d/%m/%Y")

#Plot the data
#type: whether the plot is line or point etc
plot(MyDates,MyData$GBPtoEUR,type="l", xlab="Time",ylab="EUR/GBP exchange rate")
plot(MyDates,MyData$EURtoGBP,type="l", xlab="Time",ylab="GBP/EUR exchange rate")

#You can also check the length of your dataset using
length(MyData$GBPtoEUR)

#EXample 2: UK population 1971 -2020
# Downloaded from:
#https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationestimates/timeseries/ukpop/pop

#Read in the data (ignoring the first seven rows which include the data description)
MyData <- read.csv("UK-Population.csv", header = TRUE, sep = ",", dec=".", skip=7)

#View the data
View(MyData)

#Print the first part of the data
head(MyData)

#Print the last part of the data
tail(MyData)

#Briefly check the structure of the data
str(MyData) #outcome: a data.frame containing integers

#Convert the data into a time series using function ts
population <- ts(MyData[,2],start=1971, end=2020, frequency =1)

#Plot the data
plot(population, type = "p", pch=19)


#Example 3: Electricity load
#Install the package opera (this only needs to be done once!)
#install.packages("opera")
#Load the package opera
library(opera)

#Load the data set
attach(electric_load)

#Convert the data into a time series using the function ts
LoadTS <-ts(Load,start=1996, frequency =52)

#Check the length of the time series
length(LoadTS)

#Plot the data
plot(LoadTS,type ="l", ylab="Weekly load in MW")

detach(electric_load)

#Example 4: Bitcoin prices
#Download the data from Yahoo Finance
#https://finance.yahoo.com/quote/BTC-USD/history?period1=1410912000&period2=1655856000&interval=1d&filter=history&frequency=1d&includeAdjustedClose=true


#Read in the data:
MyData <- read.csv("BTC-USD.csv", header = TRUE, sep = ",", dec = ".")

#View the data
View(MyData)

#Print the first part of the data
head(MyData)

 #Print the last part of the data
tail(MyData)

#Convert the dates into the correct date format
MyDates <- as.Date(MyData$Date, "%d/%m/%Y")

#Plot the data
plot(MyDates,MyData$Close,type="l", xlab="Time",ylab="Bitcoin price is USD")



