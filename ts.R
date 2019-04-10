x <- ts (1:20, frequency = 4, start = c(1959,1)) # frequency 4 => Quarterly Data
x <-ts (1:12, frequency = 12, start = c(1959,2)) # freq 12 => Monthly data. 
ts (1:20, frequency = 1, start = c(1959), end=c(1962))# Freq 1 - yearly data
plot(x)

#==

library(tseries)
adf.test(AirPassengers) # Augmented Dickey-Fuller Test p-value < 0.05 indicates the TS is stationary

data(AirPassengers) # loads a dataset
class(AirPassengers) # identifies class 
#class(mtcars)

start(AirPassengers)
end(AirPassengers)
frequency(AirPassengers)

summary(AirPassengers)

plot(AirPassengers) # plots year VS number of passangers

abline(reg=lm(AirPassengers~time(AirPassengers))) # fits line

cycle(AirPassengers) #This will print the cycle across years.

#This will aggregate the cycles and display a year on year trend
plot(aggregate(AirPassengers,FUN=mean))

#Box plot across months will give us a sense on seasonal effect
boxplot(AirPassengers~cycle(AirPassengers))
#Imp Deductions
#1. The year on year trend clearly shows increase in number of passangers
#2. The variance and the mean value in July and August is much higher than other months
#3. Variation is small otherwise, there is a seasonal effect



acf(log(AirPassengers))
acf(diff(log(AirPassengers)))
(fit <- arima(log(AirPassengers), c(0, 1,1),seasonal = list(order = c(0, 1, 1), period = 12)))
pred <- predict(fit, n.ahead = 10*12)
summary(pred)
plot(pred)
pred
ts.plot(AirPassengers,2.718^pred$pred, log = "y", lty = c(1,3))

