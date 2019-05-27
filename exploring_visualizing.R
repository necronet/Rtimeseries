library(fpp2)
library(datasets)

# Autoplotting
autoplot(a10)

#seasonal plot 
ggseasonplot(window(a10,start=2000))

#Seasonal plot with polar
ggseasonplot(a10, polar = TRUE)


#Restricting beer dataset

beer <- window(ausbeer, start=1992)

autoplot(beer)
ggseasonplot(beer)
ggsubseriesplot(beer)


# Autocorrelation of non-seasonal time series
autoplot(oil)
gglagplot(oil,lags=9)

ggAcf(oil,lags=9)


autoplot(sunspot.year)
ggAcf(sunspot.year)

autoplot(hyndsight, xlab="Weeks")
ggAcf(hyndsight)


# White noise 
autoplot(goog)

autoplot(diff(goog))

ggAcf(diff(goog))

Box.test(diff(goog), lag = 10, type = "Ljung")

