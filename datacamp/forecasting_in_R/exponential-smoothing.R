library(dplyr)
library(forecast)
library(fpp2)

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

# Automatic model selection with ETS

# Fit ETS model to austa in fitaus
fitaus <- ets(austa)

# Check residuals
checkresiduals(fitaus)

# Plot forecasts
autoplot(forecast(fitaus))

# Repeat for hyndsight data in fiths
fiths <- ets(hyndsight)
checkresiduals(fiths)
autoplot(forecast(fiths))

# Which model(s) fails test? (TRUE or FALSE)
fitausfail <- FALSE
fithsfail <- TRUE

# Function to return ETS forecasts
fets <- function(y, h) {
  forecast(ets(y), h = h)
}


# Apply tsCV() for both methods
e1 <- tsCV(qcement, fets, h = 4)
e2 <- tsCV(qcement, snaive, h = 4)

# Compute MSE of resulting errors (watch out for missing values)
mean(e1^2, na.rm = TRUE)
mean(e2^2, na.rm = TRUE)



# When does ETS does not work well
autoplot(lynx)
fit <- ets(lynx)
summary(fit)
fit %>% forecast(h = 20) %>% autoplot()

