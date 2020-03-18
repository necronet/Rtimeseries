library(astsa)

Xs = 1:100
T <- function(t, L) t/L
A <- 2
PS = 2
plot.ts(A*cos(2*pi*Xs*T(8, length(Xs) + PS)))

# Spectral analysis of time series 
x1 = 2*cos(2*pi*1:100*6/100) + 3*sin(2*pi*1:100*6/100)
x2 = 4*cos(2*pi*1:100*10/100) + 5*sin(2*pi*1:100*10/100)
x3 = 6*cos(2*pi*1:100*40/100) + 7*sin(2*pi*1:100*40/100)


par(mfrow=c(2,2))
plot.ts(x1, ylim=c(-10,10), main=expression(omega==6/100~~~A^2==13))
plot.ts(x2, ylim=c(-10,10), main=expression(omega==10/100~~~A^2==41))
plot.ts(x3, ylim=c(-10,10), main=expression(omega==40/100~~~A^2==85))
plot.ts((x1+x2+x3), ylim=c(-10,10), main="Sum")

x = x1 + x2 + x3
# Cyclical Behaviour and Periodicity 

# on simulated data for scaled periodogram

P = Mod(2*fft(x)/100)^2; Fr = 0:99/100
plot(Fr, P, type="o",xlab= " Frequency", ylab="Scaled periodogram")

# Another simulation

t = 1:200
x <- 2 * cos(2*pi * .2*t) * cos(2*pi*0.01*t )
plot.ts(x)
lines(cos(2*pi *.19*t) + cos(2*pi *.21*t), col=2)

Px = Mod(fft(x))^2; plot(0:199/200, Px, type="o")

# Star magnitude and part of the corresponding periodogram

n = length(star)
par(mfrow = c(2,1))

plot(star, ylab = " Star magnitude", xlab = "day")
Per = Mod(fft(star-mean(star)))^2/n
Freq = (1:n-1)/n

plot(Freq[1:50], Per[1:50], type="h", lwd=3, ylab = "Periodogram", xlab="Frequency")
u = which.max(Per[1:50])
uu = which.max(Per[1:50][-u])
1/Freq[u]; 1/Freq[uu+1]
text(.05, 7000, "24 day cycle"); text(.027, 9000, "29 day cycle")
y = cbind(1:50, Freq[1:50], Per[1:50]); y[order(y[, 3]),]


# Periodogram and spectral density function

# testing on DFT with R
# example of complex number: 1 + 1i
dft = fft(1:4)/sqrt(4)
idft = fft(dft, inverse = TRUE)/sqrt(4)
Re(idft)


# Example of a 2D ACF plot graph

fs = Mod(fft(soiltemp - mean(soiltemp)))^2/(64*36)
cs = Re(fft(fs, inverse=TRUE)/sqrt(64*36))
rs = cs/cs[1,1]

rs2 = cbind(rs[1:41, 21:2], rs[1:41, 1:21])
rs3 = rbind(rs2[41:2,], rs2)
par(mar = c(1, 2.5, 0, 0)+.1)
persp(-40:40, -20:20, rs3, phi=30, theta = 30, 
      expand = 30, scale=FALSE, ticktype = "detailed", 
      xlab="row lags", ylab = "Colums lags", zlab = "ACF")


# Spectral ANOVA example

x = c(1,2,3,2,1)
c1 = cos(2*pi*1:5*1/5); s1 = sin(2*pi*1:5*1/5)
c2 = cos(2*pi*1:5*2/5); s2 = sin(2*pi*1:5*2/5)

omega1 = cbind(c1, s1)
omega2 = cbind(c2, s2)

anova(lm(x~omega1+omega2))

Mod(fft(x))^2/5









