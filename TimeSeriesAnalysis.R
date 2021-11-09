################Time series analysis ############

install.packages("tseries")
install.packages("forecast")
install.packages("zoo")
library(tseries)
library(forecast)
library(zoo)

############Importing the datset###################
dataset <- read.csv(choose.files())
dataset
str(dataset)
dim(dataset)
summary(dataset)

######### We have to change the dataset as orderyearmonth is in chr variable have to convert into date format #################

date <- as.yearmon(dataset$OrderYearMonth, "%Y/%m")
dataset <-data.frame(date,dataset$Quantity)
dataset <-ts(dataset[,-1],frequency =12,start=c(2014,01),end=c(2017,12))
str(dataset)
##########3converted into time series now we can done further analysis############

plot(dataset, ylab = "Qunatity")
abline(reg=lm(dataset~time(dataset)))

plot(aggregate(dataset, FUN = mean))
boxplot(dataset~cycle(dataset))
###########Seasonality dataset####################

######### Now we have to check if the dataset is stationery or not##############
###### By the use of  augmented dickey fuller test#################

adf.test(diff(log(dataset)), alternative = "stationary",k=0) 
#Augmented Dickey-Fuller Test
#data:  diff(log(dataset))
#Dickey-Fuller = -8.6935, Lag order = 0, p-value = 0.01
#alternative hypothesis: stationary

##########Acf and pacf
acf(diff(log(dataset))) 
pacf(diff(log(dataset))) 


############Model building###########################
model <- arima(log(dataset),c(0,1,1),seasonal = list(order=c(0,1,1), period=12))
model 
# aic = -22.16

############Prediction######3

pred <- predict(model, n.ahead = 5*12)
# Prediction for  5 years
ts.plot(data, 2.7^pred$pred, log='y',lty=c(1,3)) 

########Predicted the dataset##############















