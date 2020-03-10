
x <- arima.sim(model = list(ar=.5), n = 100)
ts.plot(x)

# Simulate an AR model with 0.9 slope
y <- arima.sim(model = list(ar=.9), n = 100)
ts.plot(y)

# Simulate an AR model with -0.75 slope
z <- arima.sim(model = list(ar=-.75), n = 100)
ts.plot(z)


# Calculate the ACF for x
acf(x)

# Calculate the ACF for y
acf(y)

# Calculate the ACF for z
acf(z)

# Simulate and plot AR model with slope 0.9 
x <- arima.sim(model = list(ar=.9), n = 200)
ts.plot(x)
acf(x)

# Simulate and plot AR model with slope 0.98
y <- arima.sim(model = list(ar=.98), n = 200)
ts.plot(y)
acf(y)

# Simulate and plot RW model
z <- arima.sim(model = list(order=c(0,1,0)), n = 200)
ts.plot(z)
acf(z)



# Testing moving average
# Generate MA model with slope 0.5
x <- arima.sim(model = list(ma=.5), n = 100)

# Generate MA model with slope 0.9
y <- arima.sim(model = list(ma=.9), n = 100)

# Generate MA model with slope -0.5
z <- arima.sim(model = list(ma=-.5), n = 100)

# Plot all three models together
plot.ts(cbind(x, y, z))




