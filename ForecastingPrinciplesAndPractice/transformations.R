# Forecast toolbox
library(forecast)
library(readr)
library(dplyr)
library(fpp2)

monthly_milk <- ts(read.csv("ForecastingPrinciplesAndPractice/data/monthly_milk.csv"),
                   frequency=12,
                   start=c(1962,1,1))

# Showing a smoother production by removing variation between month
dframe <- cbind(Monthly = monthly_milk[,'milk_prod_per_cow_kg'], DailyAvg = monthly_milk[,'milk_prod_per_cow_kg']/monthdays(monthly_milk))

dframe %>% autoplot(facet = TRUE) + xlab("Years") + ylab("Pounds") + ggtitle("Milk production per cow")

lambda <- BoxCox.lambda(fpp2::elecdemand)

autoplot(elec)
autoplot(BoxCox(elec,lambda))

fc <- rwf(eggs, drift=TRUE, lambda=0, h=50, level=80)
fc2 <- rwf(eggs, drift=TRUE, lambda=0, h=50, level=80,
           biasadj=TRUE)
fc3 <- rwf(eggs, drift=TRUE, h=50, level=80)
autoplot(eggs) +
  autolayer(fc, series="Simple back transformation") +
  autolayer(fc2, series="Bias adjusted", PI=FALSE) +
  autolayer(fc3, series="No transformation", PI=FALSE) +
  guides(colour=guide_legend(title="Forecast"))





