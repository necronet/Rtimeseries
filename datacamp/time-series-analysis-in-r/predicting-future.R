# Datacamp example time series on predicting the future

# Simulate white noise with 100 observations
white_noise <- arima.sim(model = list(order=c(0,0,0)), n =100)

# Plot your white_noise data
ts.plot(white_noise)

# Simulate from the WN model with: mean = 100, sd = 10
white_noise_2 <- arima.sim(model = list(order=c(0,0,0)), n = 100, mean = 100, sd = 10)

# Plot your white_noise_2 data
ts.plot(white_noise_2)
arima(white_noise, order = c(0, 0, 0))

# Generate a RW model using arima.sim
random_walk <- arima.sim(model = list(order = c(0,1,0)), n = 100)

# Plot random_walk
ts.plot(random_walk)

# Calculate the first difference series
random_walk_diff <- diff(random_walk)

# Plot random_walk_diff
ts.plot(random_walk_diff)

# Generate a RW model with a drift uing arima.sim
rw_drift <- arima.sim(model = list(order=c(0,1,0)), n = 100, mean = 1)

# Plot rw_drift
ts.plot(rw_drift)

# Calculate the first difference series
rw_drift_diff <- diff(rw_drift)

# Plot rw_drift_diff
ts.plot(rw_drift_diff)


random_walk <- arima.sim(model = list(order = c(0,1,0)), n = 100, mean=1)
# Difference your random_walk data
rw_diff <- diff(random_walk)

# Plot rw_diff
ts.plot(rw_diff)

# Now fit the WN model to the differenced data
model_wn <-arima(order = c(0,0,0), x = rw_diff)

# Store the value of the estimated time trend (intercept)
int_wn <- model_wn$coef

# Plot the original random_walk data
ts.plot(random_walk)

# Use abline(0, ...) to add time trend to the figure
abline(0, int_wn)




