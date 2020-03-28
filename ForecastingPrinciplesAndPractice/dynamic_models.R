# Dynamic regression models
library(forecast)
library(ggplot2)
library(fpp2)

autoplot(uschange[,1:2], facets=TRUE) +
  xlab("Year") + ylab("") +
  ggtitle("Quarterly changes in US consumption
    and personal income")

fit <- auto.arima(uschange[,"Consumption"], xreg=uschange[,"Income"])

cbind("Regression Errors" = residuals(fit, type="regression"),
      "ARIMA errors" = residuals(fit, type="innovation")) %>%
  autoplot(facets=TRUE)

checkresiduals(fit)

# Forecasting
fcast <- forecast(fit, xreg=rep(mean(uschange[,"Income"]),8))
autoplot(fcast) + xlab("Year") +
  ylab("Percentage change")

as.data.frame(elecdaily) %>% ggplot(aes(x = Temperature, y = Demand)) + geom_point()

xreg <- cbind(MaxTemp = elecdaily[, "Temperature"],
              MaxTempSq = elecdaily[, "Temperature"]^2,
              Workday = elecdaily[, "WorkDay"])
fit <- auto.arima(elecdaily[, "Demand"], xreg = xreg)

checkresiduals(fit)

fcast <- forecast(fit, xreg = cbind(MaxTemp=rep(26,14), MaxTempSq=rep(26^2,14), Workday=c(0,1,0,0,1,1,1,1,1,0,0,1,1,1)))
autoplot(fcast) + ylab("Electricity demand (GW)")


# Stochastic and deterministic models

autoplot(austa) + xlab("Year") +
  ylab("millions of people") +
  ggtitle("Total annual international visitors to Australia")

trend <- seq_along(austa)
(fit1 <- auto.arima(austa, d=0, xreg=trend))

(fit2 <- auto.arima(austa, d=1))



fc1 <- forecast(fit1,
                xreg = length(austa) + 1:10)
fc2 <- forecast(fit2, h=10)
autoplot(austa) +
  autolayer(fc2, series="Stochastic trend") +
  autolayer(fc1, series="Deterministic trend") +
  ggtitle("Forecasts from trend models") +
  xlab("Year") + ylab("Visitors to Australia (millions)") +
  guides(colour=guide_legend(title="Forecast"))



# Dynamic regression models

cafe04 <- window(auscafe, start=2004)
plots <- list()
for (i in seq(6)) {
  fit <- auto.arima(cafe04, xreg = fourier(cafe04, K = i),
                    seasonal = FALSE, lambda = 0)
  plots[[i]] <- autoplot(forecast(fit,
                                  xreg=fourier(cafe04, K=i, h=24))) +
    xlab(paste("K=",i,"   AICC=",round(fit[["aicc"]],2))) +
    ylab("") + ylim(1.5,4.7)
}
gridExtra::grid.arrange(
  plots[[1]],plots[[2]],plots[[3]],
  plots[[4]],plots[[5]],plots[[6]], nrow=3)



