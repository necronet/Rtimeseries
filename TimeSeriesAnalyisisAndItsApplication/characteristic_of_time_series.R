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


# Sample ACF and scatterplots

r = round(acf(soi, 12, plot=FALSE)$acf[-1],3)
par(mfrow=c(1,3))
plot(lag(soi,-1), soi); legend('topleft', legend=r[1])
plot(lag(soi,-6), soi); legend('topleft', legend=r[6])
plot(lag(soi,-12), soi); legend('topleft', legend=r[12])

# Simulating time series

set.seed(101010)
x1 = 2*rbinom(11, 1, .5) - 1
x2 = 2*rbinom(101, 1, .5) - 1
x3 = 2*rbinom(1001, 1, .5) - 1
y1 = 5 + filter(x1, sides = 1, filter=c(1, -.7))[-1]
y2 = 5 + filter(x2, sides = 1, filter=c(1, -.7))[-1]
y3 = 5 + filter(x3, sides = 1, filter=c(1, -.7))[-1]
plot.ts(y1, type='s'); plot.ts(y2, type='s'); plot.ts(y3, type='s')
c(mean(y1), mean(y2), mean(y3))

acf(y1, lag.max=4, plot=FALSE)
acf(y2, lag.max=4, plot=FALSE)
acf(y3, lag.max=4, plot=FALSE)

# Manual calculation of ACF per lag 2, 1, 0
( (sum( (y1[c(-1,-2)]-mean(y1)) * (y1[c(-10,-9)]-mean(y1)) ))/9 )/var(y1)
( (sum( (y1[-1]-mean(y1)) * (y1[-10]-mean(y1)) ))/9 )/var(y1)
(sum( (y1-mean(y1))^2 )/9 )/var(y1)







