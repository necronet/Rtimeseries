# Simlated Time series from TSA 4 Chapter 3 ARIMA models
library(astsa)
set.seed(101010)


##AR(1) Example
par(mfrow=c(2,1))
plot(arima.sim(list(order=c(1,0,0), ar=.9), n=100),
     ylab="x",main=(expression(AR(1)~~~phi==+.9)))      # ~ is a space and == is equal        
plot(arima.sim(list(order=c(1,0,0), ar=-.9), n=100),
     ylab="x",main=(expression(AR(1)~~~phi==-.9)))

##MA(1) Example

par(mfrow=c(2,1))                                   
plot(arima.sim(list(order=c(0,0,1), ma=.5), n=100), 
     ylab="x",main=(expression(MA(1)~~~theta==+.5)))    
plot(arima.sim(list(order=c(0,0,1), ma=-.5), n=100),
     ylab="x",main=(expression(MA(1)~~~theta==-.5)))    