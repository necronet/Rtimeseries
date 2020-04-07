library(fpp2)
library(forecast)
library(vars)

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


# Vector autoregressive model

VARselect(uschange[,1:2], lag.max=8, type="const")[["selection"]]

var1 <- VAR(uschange[,1:2], p=1, type="const")
serial.test(var1, lags.pt=10, type="PT.asymptotic")
var2 <- VAR(uschange[,1:2], p=2, type="const")
serial.test(var2, lags.pt=10, type="PT.asymptotic")

var3 <- VAR(uschange[,1:2], p=3, type="const")
serial.test(var3, lags.pt=10, type="PT.asymptotic")

forecast(var3) %>% autoplot() + xlab("Year")


fit <- nnetar(sunspotarea, lambda=0)
autoplot(forecast(fit,h=30, PI=TRUE))


sim <- ts(matrix(0, nrow=30L, ncol=9L),
          start=end(sunspotarea)[1L]+1L)
for(i in seq(9))
  sim[,i] <- simulate(fit, nsim=30L)

autoplot(sunspotarea) + autolayer(sim)





df <- data.frame(runif(3), runif(3))
names(df) <- c(1, 2)
df$`3` = df$`1`+ df$`2`


bootseries <- bld.mbb.bootstrap(debitcards, 10) %>%
  as.data.frame() %>% ts(start=2000, frequency=12)
autoplot(debitcards) +
  autolayer(bootseries, colour=TRUE) +
  autolayer(debitcards, colour=FALSE) +
  ylab("Bootstrapped series") + guides(colour="none")
