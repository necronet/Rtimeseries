# Simlated Time series from TSA 4 
library(astsa)
set.seed(101010)


x1 = 2*rbinom(11, 1, 0.5) - 1
x2 = 2*rbinom(101, 1, 0.5) - 1

y1 = 5 + filter(x1, sides=1, filter=c(1,-.7))[-1]
y2 = 5 + filter(x2, sides=1, filter=c(1,-.7))[-1]

plot.ts(y1, type='s')
plot.ts(y2, type='s')

c(mean(y1), mean(y2))

acf(y1, lag.max=4, plot=FALSE)

acf(y2, lag.max=4, plot=FALSE)

acf(speech, 250)

par(mfrow=c(3, 1))
acf(soi, 48, main="SOI")
acf(rec, 48, main="Recruitment")
ccf(soi, rec, 48, main="Soi vs Recruitment")

lag1.plot(soi, 12)

lag2.plot(soi, rec, 8)


