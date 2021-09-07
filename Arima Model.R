setwd('C:\\Users\\Manish Bajpai\\Desktop\\Practise')
## Importing the dataset
Register_new <- read.csv('NewHouseRegistrations_Ireland.csv',fileEncoding="UTF-8-BOM")
NewHouseRegistration<-ts(Register_new, start=1978, frequency = 1)
autoplot(NewHouseRegistration ,ylab="New House Registrations",xlab="Years")
##NewHouseRegistration_1 <-NewHouseRegistration[,2]
##View(NewHouseRegistration_1)

# set training data, test data
train<-window(NewHouseRegistration,start=c(1978,1), end=c(2006,1))
View(train)
##test period
test<-window(NewHouseRegistration,start=c(2007,1), end=c(2019,1), frequency=1)
View(test)

## finding the no of diffrencing
ndiffs(train)
diffrencing<- diff(train)

## checking stattionary
install.packages("aTSA")
library(aTSA)
adf.test(diffrencing)
## Plotting the Graph
autoplot(diff(train),ylab="New House Registrations",xlab="Years")
## removing varinace by log
autoplot(diff(log10(train)),ylab="New House Registrations",xlab="Years")


#### Plot ACF and PACF to identify potential AR and MA model
par(mfrow = c(1,1))
acf(diffrencing,main='regoster')
pacf(diffrencing,main='register')

###Applying auto arima function to know the model type
arima_optimal_1 = auto.arima((train),stepwise = FALSE)
##New Model
arimaModel_1=arima(train, order=c(0,1,1))

## look at the parameters
print(arimaModel_1)
## forecast
forecast1=predict(arimaModel_1, 3)
## 
install.packages('DMwR')
library(DMwR)
