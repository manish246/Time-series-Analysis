setwd('C:\\Users\\Manish Bajpai\\Desktop\\Practise')
## Importing the dataset
Register <- read.csv('NewHouseRegistrations_Ireland.csv',fileEncoding="UTF-8-BOM")
Register<-ts(Register, start=c(1978,1), frequency = 1)
plot(Register)
Register_1 <-Register[,2]
View(Register_1)
# set training data, test data
train<-window(Register_1,start=c(1978,1), end=c(2006,1))
View(train)
##test period
test<-window(Register_1,start=c(2007,1), end=c(2019,1), frequency=1)
View(test)

########## 
## Applying Naive bayes Model
library(fpp2)
naive_mod <- naive(train, h = 3)
summary(naive_mod)
autoplot(naive_mod)
## accuracy of naive model
accuracy(naive_mod,test)
