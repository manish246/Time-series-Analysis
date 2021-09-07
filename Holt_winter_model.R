setwd('C:\\Users\\Manish Bajpai\\Desktop\\Practise')
## Importing the dataset
Timseseries <- read.csv('OverseasTrips.csv',fileEncoding="UTF-8-BOM")
Trips<-ts(Timseseries, start=c(2012,1), frequency = 4)
plot(Trips)
Trips_1 <-Trips[,2]
View(Trips)
# set training data, test data
train<-window(Trips_1,start=c(2012,1), end=c(2017,4))
View(train)
##test period
test<-window(Trips_1,start=c(2018,1), end=c(2019,4), frequency=4)
View(test)

##applying holt-winters 
# method on qcement
autoplot(decompose(Trips_1))


##Forming Model
Ets_model_1 <- ets(train,
                 model = "ZZZ")
autoplot(forecast(Ets_model_1))

Ets_Predict<- forecast(Ets_model_1,
                       h = 3)

## Getting the Optimum value for gamma
gamma <- seq(0.01, 0.85, 0.01)
RMSE <- NA

for(i in seq_along(gamma)) {
  Ets_gamma <- ets(train, 
                 "ZZZ", 
                 gamma = gamma[i])
  future <- forecast(Ets_gamma, 
                     h = 3)
  RMSE[i] = accuracy(future, 
                     test)[2,2]
}
error <- data_frame(gamma, RMSE)
minimum <- filter(error, 
                  RMSE == min(RMSE))

##ploting Graph 
ggplot(error, aes(gamma, RMSE)) +
  geom_line() +
  geom_point(data = minimum, 
             color = "blue", size = 2) +
  ggtitle("gamma's impact on 
            forecast errors",
          subtitle = "gamma = 0.63 minimizes RMSE")

##Finding the accuracy

accuracy(Ets_Predict, test)



# new model with 
# optimal gamma parameter
Ets_Model2 <- ets(train,
                   model = "ZZZ", 
                   gamma = 0.63)
Ets_Predict_new <- forecast(Ets_Model2, 
                       h = 3)
accuracy(Ets_Predict_new, test)

# predicted values
Ets_Predict_new
autoplot(Ets_Predict_new)
