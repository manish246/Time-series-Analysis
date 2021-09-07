
## setting the working directory

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

#######################################################################
#Plotting

##Observing Raw Dataset
install.packages("fpp")
library(fpp)

plot(Trips_1,main="Number Of Trips", xlab="year", ylab="Trips from abroad",lwd=3)
lines(ma(Trips_1,9),col="orange",lwd=4)

##Seasonality visualization

seasonplot(Trips_1,main="Seasonal plot: Trips from abroad",
           year.labels = TRUE, year.labels.left = TRUE,
           col=1:20, pch=19,lwd=2)

## Ploting series subplot
ggsubseriesplot(Trips_1,main="Seasonal plot: Trips from abroad",
                year.labels = TRUE, year.labels.left = TRUE,
                col=1:20, pch=19,lwd=2)
##Polar Plot
ggseasonplot(Trips_1,main="Seasonal plot: Trips from abroad",
                year.labels = TRUE, year.labels.left = TRUE,polar=TRUE,
                col=1:20, pch=19,lwd=2)
###########################################################################

##Simple Exponential Smoothing (SES)

##Removing Trend
ndiffs(train)
Trips_diff <- diff(train)
autoplot(Trips_diff)

##reapplying SES on the filtered data

Trips_diff1 <- ses(Trips_diff,
                    alpha = .2, 
                    h = 3)
autoplot(Trips_diff1)

# removing trend from test set
ndiffs(test)
Trips_diff_test <- diff(test)
autoplot(Trips_diff_test)
accuracy(Trips_diff1, Trips_diff_test)


# comparing our model
alpha <- seq(.01, .99, by = .01)
RMSE <- NA
for(i in seq_along(alpha)) {
  fit <- ses(Trips_diff, alpha = alpha[i],
             h =3)
  RMSE[i] <- accuracy(fit, 
                      Trips_diff_test)[2,2]
}
  
# convert to a data frame and 
# idenitify min alpha value
library(tidyverse)
alpha.fit <- tibble(alpha, RMSE)
alpha.min <- filter(alpha.fit, 
                      RMSE == min(RMSE))


# plot RMSE vs. alpha
ggplot(alpha.fit, aes(alpha, RMSE)) +
  geom_line() +
  geom_point(data = alpha.min,
             aes(alpha, RMSE), 
             size = 2, color = "red")


##again plotting the model with min alpha calulcated from above


Trips_diff2 <- ses(Trips_diff,
                   alpha = 0.01, 
                   h = 3)
autoplot(Trips_diff2)

##Performance evaluation
accuracy(Trips_diff2, Trips_diff_test)



