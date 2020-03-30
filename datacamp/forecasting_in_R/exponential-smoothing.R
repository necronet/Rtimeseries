# Based on: https://campus.datacamp.com/courses/forecasting-in-r/exponential-smoothing?ex=2
# Exponential Smoothing 

autoplot(marathon)

# Use ses() to forecast the next 10 years of winning times
fc <- ses(marathon, h = 10)

# Use summary() to see the model parameters
summary(fc)

# Use autoplot() to plot the forecasts
autoplot(fc)

# Add the one-step forecasts for the training data to the plot
autoplot(fc) + autolayer(fitted(fc))

# Create a training set using subset()
train <- subset(marathon, end = length(marathon) - 20)

# Compute SES and naive forecasts, save to fcses and fcnaive
fcses <- ses(train, h = 20)
fcnaive <- naive(train, h = 20)

# Calculate forecast accuracy measures
accuracy(fcses, marathon)
accuracy(fcnaive, marathon)

# Save the best forecasts as fcbest
fcbest <- fcnaive

fcholt <- holt(austa, h = 10)
summary(fcholt)
autoplot(fcholt)
checkresiduals(fcholt)


# Holt's winter method
# Plot the data
autoplot(a10)

# Produce 3 year forecasts
fc <- hw(a10, seasonal = "multiplicative", h = 36)

# Check if residuals look like white noise
checkresiduals(fc)
whitenoise <- FALSE

# Plot forecasts
autoplot(fc)

# Holt-Winters method with daily data

# Create training data with subset()
train <- subset(hyndsight, end = 337)

# Holt-Winters additive forecasts as fchw
fchw <- hw(train, seasonal = "additive", h = 7*4)

# Seasonal naive forecasts as fcsn
fcsn <- snaive(train, h=7*4)

# Find better forecasts with accuracy()
accuracy(fchw, hyndsight)
accuracy(fcsn, hyndsight)

# Plot the better forecasts
autoplot(fchw)
