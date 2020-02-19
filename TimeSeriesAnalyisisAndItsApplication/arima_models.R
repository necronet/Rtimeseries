# Arima Models chapter 3 on Time series analysis and applications

dev.new()
n = 6
par(mfrow=c(n,1), mar=c(1,1,1,1))
ars = seq(from=-.9, to=.9, length.out = n)
for (i in 1:n) {
  plot( arima.sim(list(order=c(1,0,0), ar=ars[i]), n =100), ylab="X", main=(bquote("AR(1) "~phi==~.(ars[i]))))
}


