# Read in the data:
MyData <- read.csv("EUR-GBP-Rates.csv", header = TRUE, sep = ",",
                     dec = ".",fileEncoding="UTF-8-BOM")

# View the data
View(MyData)

# Determine the number of observations in MyData
nrow(MyData)

str(MyData)

# print all the values of the GBPtoEUR
MyData$GBPtoEUR
MyData[,3]

plot(MyData$GBPtoEUR, type = 'l', col = 'blue', main = "EUR/GBP exchange rate",
     xlab = "Time", ylab = "GBP per EUR", cex.main = 1.25, cex.lab = 1.5, 
     cex.axis = 2)

# Transfer Data
MyDates = as.Date(MyData$Date, "%d/%m/%Y")
plot(MyDate, MyData$GBPtoEUR, type = 'l')

# Now we can compare the initial plot with the modified plot
par(mfrow=c(1, 2))
plot(MyData$GBPtoEUR)
plot(MyDates, MyData$GBPtoEUR, type="l",col="blue", main="EUR/GBP exchange rate", xlab="Time", ylab="GBP per one EUR", cex.main
     =1.25, cex.lab=1.5, cex.axis=2)
par(mfrow=c(1,1))

