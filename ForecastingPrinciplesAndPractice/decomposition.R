# Decompositions of time series
# Based on: https://otexts.com/fpp2/moving-averages.html

# Moving average smoothing (which is different than moving average regression model)
library(forecast)
library(fpp2)
library(ggplot2)
library(stlplus)

ma_series <- ma(elecsales, 5)
[!is.na(x)]


# Smoothness of data
autoplot(elecsales, series="Data") + 
  autolayer(ma(elecsales, 5), series="5-MA") +
  autolayer(ma(elecsales, 3), series="3-MA") +
  autolayer(ma(elecsales, 7), series="7-MA") +
  xlab("Year") + 
  ylab("GWh") + ggtitle("Annual electricity sales: South Australia") +
  scale_colour_manual(values=c("Data"="grey50","5-MA"="red","3-MA"="green","7-MA"="blue"),
                      breaks=c("Data","5-MA","3-MA","7-MA"))



beer <- window(ausbeer,start=1992)
beer_ma2 <- ma(beer, 2)
beer_ma4 <- ma(beer, order=4, centre=FALSE)
beer_ma2x4<- ma(beer, order=4, centre=TRUE)

autoplot(ausbeer) + autolayer(beer) + autolayer(beer_ma4) + autolayer(beer_ma2x4) + autolayer(beer_ma2)



autoplot(elecequip, series="Data") +
  autolayer(ma(elecequip, 12), series="12-MA") +
  xlab("Year") + ylab("New orders index") +
  ggtitle("Electrical equipment manufacturing (Euro area)") +
  scale_colour_manual(values=c("Data"="grey","12-MA"="red"),
                      breaks=c("Data","12-MA"))


elecequip %>% decompose(type="multiplicative") %>%
  autoplot() + xlab("Year") +
  ggtitle("Classical multiplicative decomposition
    of electrical equipment index")

#Diagnositing the s.window parameter through seasonal diagnostic plot
elecequip %>% stlplus(t.window = 13, s.window = "periodic") %>% plot_seasonal


fit <- stl(elecequip, t.window=13, s.window="periodic",
           robust=TRUE)
fit %>% seasadj() %>% naive() %>%
  autoplot() + ylab("New orders index") +
  ggtitle("Naive forecasts of seasonally adjusted data")

# Readding the seasonality component
fit %>% forecast(method="naive") %>%
  autoplot() + ylab("New orders index")

