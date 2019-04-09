#Time series not use when value is constant
#Time series use when data is stationary condtion for stationary is mean is constant 
#with time and varience and co-varience.
end(AirPassengers)
start(AirPassengers)
plot(AirPassengers)
time(AirPassengers)

abline(reg =lm(AirPassengers~time(AirPassengers)))
#Fit the line:-view the mean and varience

summary(AirPassengers)
#Component of Time Series :- general Trends,Sesonal,Irregular Fluctuation
#General trends show where the data is increasing or decreasing
plot(aggregate(AirPassengers,FUN=mean))#it will show the trend of data where data is increasing or decreasing
aggregate(AirPassengers,FUN=mean)#Yearly mean of data
#Sesonal data show where there is any fluctuation in perticular data or not
#i.e how many pick in data
#Irregular Fluctuation :- the data give irrelavent pick that is not predictable

class(AirPassengers)
frequency(AirPassengers)#12 Monthes

boxplot(AirPassengers~cycle(AirPassengers))#shows where the is any seasonal attributes are not

input <- mtcars[,c('mpg','cyl')]
print(head(input))
boxplot(mpg ~ cyl, data = mtcars, xlab = "Number of Cylinders",
        ylab = "Miles Per Gallon", main = "Mileage Data")

#Data Serielazation process
#using log fuction we make varience constant
log(AirPassengers)
plot(log(AirPassengers))
#2)constant the mean  using diff() function
plot(diff(log(AirPassengers)))

#Time series analysis using AR I MA model(auto regression integration and moving average)
#AR-P ,I-d ,MA-q
help(acf)

#detemining q value using acf check which first line go down word and take one previous of that line and start numbering from 0
acf(diff(log(AirPassengers)))#here q is 1
pacf(diff(log(AirPassengers)))#here p value is 0 
#and d value is get by how much time we use diff function here is 1
fit <- arima(log(AirPassengers), c(0, 1, 1),seasonal = list(order = c(0, 1, 1), period = 12))
fit
pred<-predict(fit,n.ahead = 10*12)#10 year and 12 month prediction we have calculated
pred1<-2.718^pred$pred#Converting log value to decimal value we use e value


ts.plot(AirPassengers,pred1)



#Testing the model 
dataw<-ts(AirPassengers,frequency = 12,start = c(1949,1),end=c(1959,12))
fit<-arima(log(dataw),c(0,1,1),seasonal = list(order=c(0,1,1),period=12))#we don't have to take difference because we pass d value 1 have it automaticaly does it
pred1<-predict(fit,n.ahead = 10*12)
pred1<-2.718^pred1$pred
data1<-head(pred1,12)
predict_1960=round(data1,digits = 0)
original_1960=tail(AirPassengers,12)
