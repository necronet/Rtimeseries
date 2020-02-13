# This folder will follow up on Time series analysis and its application book fourth edition
library(astsa)


# Three example of time series data
plot(jj, type="o", ylab="Quaterly Earning per share")
plot(globtemp, type="o", ylab="Global temperature deviations")
plot(speech)

# Moving average is referred generically as filtered series
w = rnorm(500, 0 ,1)
#Side value of 2 means is going to be centered around lag 0 
v = filter(w, sides = 2, filter=rep(1/3,3))
par(mfrow=c(2,1))
plot.ts(w, main="White Noise")
plot.ts(v, main="Moving average(3)")

# For autoregressive still use filter function but with method recursive

w = rnorm(550, 0, 1)
x = filter(w, filter=c(1,-.9), method = "recursive")[-(1:50)]
plot.ts(x, main= "autoregression")

# For random walk with drift
set.seed(154)
w = rnorm(200)
x = cumsum(w)
wd = w + 0.2
xd = cumsum(wd)
plot.ts(xd, ylim = c(-5,55), main= "Random walk", ylab = '')
lines(x, col=4); abline(h=0, col=4, lty=2); abline(a=0, b=.2, lty=2)

# Signal in Noise

freq_oscilation = 1/30
t = 1:500*freq_oscilation # w oscilation frequency
phi = 50*pi #phase shift
A = 5 #amplitude
cs = A*cos(2*pi*t + phi); w = rnorm(500.0,1) # aditive noise

par(mfrow=c(3,1), mar=c(3,2,2,1), cex.main=1.5)
plot.ts(cs, main=expression(2*cos(2*pi*t/50+.6*pi)))
plot.ts(cs + w, main=expression(2*cos(2*pi*t/50+.6*pi) + N(0,1)))
plot.ts(cs + 5*w, main=expression(2*cos(2*pi*t/50+.6*pi +N(0,25))))
