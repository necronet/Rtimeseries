# Arima Models chapter 3 on Time series analysis and applications
library(astsa)


dev.new()
n = 6
par(mfrow=c(n,1), mar=c(1,1,1,1))
ars = seq(from=-.9, to=.9, length.out = n)
for (i in 1:n) {
  plot( arima.sim(list(order=c(1,0,0), ar=ars[i]), n =100), ylab="X", main=(bquote("AR(1) "~phi==~.(ars[i]))))
}

# As per docs order refers to:
# A specification of the non-seasonal part of the ARIMA model: 
# the three integer components (p, d, q) are the AR order, the degree of differencing, and the MA order.
par(mfrow=c(2,1))
plot( arima.sim(list(order=c(0,0,1), ma=.9), n =100), ylab="X", main= expression(MA(1)~~~theta==+.9) )
plot( arima.sim(list(order=c(0,0,1), ma=-.9), n =100), ylab="X", main= expression(MA(1)~~~theta==-.9) )


# AR(2) with complex roots

z = c(1, -1.5, .75)
(a = polyroot(z)[1])

arg = Arg(a)/(2*pi)
1/arg

set.seed(8675309)
ar2 = arima.sim(list(order=c(2,0,0), ar=c(1.5,-.75)),n =144)
plot(ar2, axes=FALSE, xlab="Time")
axis(2); axis(1, at=seq(0,144, by=12)); box()
abline(v=seq(0,144, by=12), lty=2)

ACF = ARMAacf(ar=c(1.5,-.75), ma=0, 50)
plot(ACF, type="h", xlab="lag")
abline(h=0)

# ON ACF and PACF proving how PACF cuts once it stop having an influence based on order
dev.off()
ACF = ARMAacf(ar=c(1.5, -.1, -.75), ma=0, 24)[-1]
PACF = ARMAacf(ar=c(1.5, -.1, -.75), ma=0, 24, pacf = TRUE)

par(mfrow=c(1,2))
plot(ACF, type="h", xlab="lag", ylim =c(-.8, 1)); abline(h=0)
plot(PACF, type="h", xlab="lag", ylim =c(-.8, 1)); abline(h=0)


# Forecasting with time Series
dev.off()
regr = ar.ols(rec, order = 2, demean=FALSE, intercept = TRUE)
forecast = predict(regr, n.ahead=24)
ts.plot(rec, forecast$pred, col=1:2, xlim=c(1980,1990), ylab="Recruitment")

# upper and lower bound for forecasting
U = forecast$pred + forecast$se; L = forecast$pred-forecast$se
xx = c(time(U), rev(time(U))); yy = c(L, rev(U))
polygon(xx, yy, border=8, col = gray(.6, alpha=.2))
lines(forecast$pred, type='p', col=2)
abline(h=mean(rec), b=.2, lty=2)


dev.off()
regr = ar.ols(rec, order = 3, demean=FALSE, intercept = TRUE)
forecast = predict(regr, n.ahead=24)
ts.plot(rec, forecast$pred, col=1:2, xlim=c(1980,1990), ylab="Recruitment")

# upper and lower bound for forecasting
U = forecast$pred + forecast$se; L = forecast$pred-forecast$se
xx = c(time(U), rev(time(U))); yy = c(L, rev(U))
polygon(xx, yy, border=8, col = gray(.6, alpha=.2))
lines(forecast$pred, type='p', col=2)
abline(h=mean(rec), b=.2, lty=2)


# Backcasting on R with ARMA model
x = arima.sim(list(order= c(1,0,1), ar=.9, ma=.5 ), n = 100)
xr = rev(x)
# Prediction for backcasting
pxr = predict(arima(xr, order = c(1,0,1)), 10)
pxrp = rev(pxr$pred)
pxrse = rev(pxr$se)
nx = ts(c(pxrp, x), start=-9)

plot(nx, ylab=expression(X[~t]), main = "Backcasting")

# upper and lower bounds for backcasting
U = nx[1:10] + pxrse; L = nx[1:10] - pxrse
xx = c(-9:0,0:-9); yy = c(L, rev(U))
polygon(xx, yy, border=8, col = gray(.6, alpha=.2))
lines(-9:0, nx[1:10], type='o', col=2)

