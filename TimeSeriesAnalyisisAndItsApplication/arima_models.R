# Arima Models chapter 3 on Time series analysis and applications

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



