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
install.packages("fpp2")
library(fpp2) 

## applying Model

holt_Model <- holt(train,
                  h = 3)
autoplot(holt_Model)

##checking the value of smoothening parameter alpha and beta

holt_Model$model

# identify optimal alpha parameter
beta <- seq(.0001, .5, by = .001)
RMSE <- NA
for(i in seq_along(beta)) {
  fit <- holt(train,
              beta = beta[i], 
              h = 3)
  RMSE[i] <- accuracy(fit, 
                      test)[2,2]
}



# convert to a data frame and
# idenitify min alpha value
beta.fit <- tibble(beta, RMSE)
beta.min <- filter(beta.fit, 
                   RMSE == min(RMSE))

# plot RMSE vs. alpha
ggplot(beta.fit, aes(beta, RMSE)) +
  geom_line() +
  geom_point(data = beta.min, 
             aes(beta, RMSE), 
             size = 2, color = "red")

##reapllying the model with minimin b obtained from above

holt_Model_corrected <- holt(train,
                   h = 3,
                  beta = 0.0341)

# accuracy of first model
accuracy(holt_Model, test)

# accuracy of new optimal model
accuracy(holt_Model_corrected, test)


##Plotting

p1 <- autoplot(holt_Model) +
  ggtitle("Original Holt's Model")

p2 <- autoplot(holt_Model_corrected) +
  ggtitle("Optimal Holt's Model")

gridExtra::grid.arrange(p1, p2, 
                        nrow = 1)

