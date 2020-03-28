# Exponential smoothing
# based on: https://otexts.com/fpp2/ses.html

library(forecast)
library(fpp2)
library(ggplot2)

autoplot(oil) 
oildata <- window(oil)
autoplot(oildata)

fc <- ses(oildata, h=5)
round(accuracy(fc),2)
summary(fc)

autoplot(fc) + autolayer(fitted(fc), series = "Model SES")+ ylab("Oil (millions of tonnes") + xlab("Year")

# Holt linear method
air <- window(ausair, start=1990)
autoplot(air)
fc <- holt(air, h=5)
autoplot(air) + autolayer(fc)

autoplot(fc) + autolayer(fitted(fc), series = "Model Linear Holt") + xlab("Year")

# Trying different values of alpha and comparing aic 
# holt(air, h=5, alpha = .3)$model$aic
# holt(air, h=5, alpha = .2)$model$aic
# holt(air, h=5, alpha = .9)$model$aic
# holt(air, h=5, alpha = .6)$model$aic
# holt(air, h=5)$model$aic

# Damped parameters to avoid overshoot the forecast 
fc <- holt(air, h=15)
fc2 <- holt(air, damped=TRUE, phi = 0.9, h=15)
autoplot(air) +
  autolayer(fc, series="Holt's method", PI=FALSE) +
  autolayer(fc2, series="Damped Holt's method", PI=FALSE) +
  ggtitle("Forecasts from Holt's method") + xlab("Year") +
  ylab("Air passengers in Australia (millions)") +
  guides(colour=guide_legend(title="Forecast"))

# Holt's winter method

aust <- window(austourists,start=2005)
fit1 <- hw(aust,seasonal="additive")
fit2 <- hw(aust,seasonal="multiplicative")
autoplot(aust) +
  autolayer(fit1, series="HW additive forecasts", PI=FALSE) +
  autolayer(fit2, series="HW multiplicative forecasts",
            PI=FALSE) +
  xlab("Year") +
  ylab("Visitor nights (millions)") +
  ggtitle("International visitors nights in Australia") +
  guides(colour=guide_legend(title="Forecast"))