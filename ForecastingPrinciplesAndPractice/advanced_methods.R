p1 <- autoplot(calls) +
  ylab("Call volume") + xlab("Weeks") +
  scale_x_continuous(breaks=seq(1,33,by=2))
p2 <- autoplot(window(calls, end=4)) +
  ylab("Call volume") + xlab("Weeks") +
  scale_x_continuous(minor_breaks = seq(1,4,by=0.1))
gridExtra::grid.arrange(p1,p2)


calls %>% mstl() %>% autoplot() + xlab("Week")

calls %>%  stlf() %>% autoplot() + xlab("Week")



fit <- auto.arima(calls, seasonal=FALSE, lambda=0,
                  xreg=fourier(calls, K=c(10,10)))
fit %>%
  forecast(xreg=fourier(calls, K=c(10,10), h=2*169)) %>%
  autoplot(include=5*169) +
  ylab("Call volume") + xlab("Weeks")


calls %>%
  subset(start=length(calls)-2000) %>%
  tbats() -> fit2
fc2 <- forecast(fit2, h=2*169)
autoplot(fc2, include=5*169) +
  ylab("Call volume") + xlab("Weeks")




# Dynamic harmonic regression
cooling <- pmax(elecdemand[,"Temperature"], 18)
fit <- auto.arima(elecdemand[,"Demand"],
                  xreg = cbind(fourier(elecdemand, c(10,10,0)),
                               heating=elecdemand[,"Temperature"],
                               cooling=cooling))


temps <- subset(elecdemand[,"Temperature"],
                start=NROW(elecdemand)-2*48+1)
fc <- forecast(fit,
               xreg=cbind(fourier(temps, c(10,10,0)),
                          heating=temps, cooling=pmax(temps,18)))
autoplot(fc, include=14*48)

checkresiduals(fc)