# Classical regression in the context of time series
library(astsa)

dev.off()
par(mfrow=c(3,1))
plot(cmort, main="Cardiovascular Mortality", xlab = "", ylab = "")
plot(tempr, main="Temperature", xlab = "", ylab = "")
plot(part, main="Particles", xlab = "", ylab = "")

# New Graph
dev.new()
ts.plot(cmort, tempr, part, col=1:3)

dev.new()
pairs(cbind(Mortality=cmort, Temperature=tempr, Particulates=part), lower.panel = NULL)

temp = tempr-mean(tempr)
temp2 = temp^2
trend = time(cmort)
fit = lm(cmort ~ trend + temp + temp2 + part, na.action=NULL)
summary(fit)
summary(aov(fit))
summary(aov(lm(cmort~cbind(trend,temp,temp2,part))))
num = length(cmort)

AIC(fit)/num-log(2*pi)
BIC(fit)/num-log(2*pi)

AICc = log(sum(resid(fit)^2)/num) + (num+5)/(num-3)

fish = ts.intersect(rec, soiL6=lag(soi,-6), dframe=TRUE)
summary( fit1 <- lm(rec~soiL6, data=fish, na.action=NULL))


# Detrending and Differencing a time series

fit = lm(chicken~time(chicken), na.action = NULL)
par(mfrow=c(3,1))
plot(chicken, type="o", main="Detrend")
plot(resid(fit), type="o", main="Detrend")
plot(diff(chicken), type="o",main="First difference")
dev.new()
par(mfrow=c(3,1))
acf(chicken, 48, main="Chicken")
acf(resid(fit), 48, main="Detrended")
acf(diff(chicken), 48, main="Differencing")

dev.new()
par(mfrow=c(3,1))
plot(globtemp, type="o")
plot(diff(globtemp), type="o")
mean(diff(globtemp))
acf(diff(gtemp), 48)

# Using linear regression to discover a signal in noise

set.seed(90210)
x = 2*cos(2*pi*1:500/50 + .6*pi) + rnorm(500,0,5)
z1 = cos(2*pi*1:500/50)
z2 = sin(2*pi*1:500/50)
summary(fit<- lm(x~0+z1+z2))

dev.off()
plot.ts(x, col=8, ylab=expression(hat(x)))
lines(fitted(fit), col=2)


# Smoothing in the context of Linear regression
dev.off()
windows()
wgts <- c(.5, rep(1,11), .5)/12
soif <- filter(soi, sides=2, filter=wgts)
plot(soi)
lines(soif, lwd=2, col=4)
par(fig=c(.75, 1, .75, 1), new=TRUE)
nwgts = c(rep(0,20), wgts, rep(0,20))
plot(nwgts, type="l", ylim=c(-.02, .1), xaxt='n', yaxt='n', ann=FALSE)

# Kernel smoothing for time sereis Nadayara kernel
plot(soi)
lines(ksmooth(time(soi), soi, "normal", bandwidth = 1), lwd=2, col=4)
par(fig=c(.50,1,.50,1), new=TRUE)
gauss <- function(x) { 1/sqrt(2*pi) * exp( -(x^2)/2) }
x = seq(from=-3, to=3, by=0.001)
plot(x, gauss(x), type='l', ylim = c(-.02, .45), xaxt='n', yaxt='n', ann=FALSE)


# Lowess smoother for el Nino
plot(soi)
lines(lowess(soi, f=.05), lwd=2, col=4)
lines(lowess(soi), lty=2, lwd=2, col=2)

