library(forecast)
library(fpp2)

# Time plot of both variables
autoplot(advert, facet = TRUE)

# Fit ARIMA model
fit <- auto.arima(advert[, "sales"], xreg = advert[, "advert"], stationary = TRUE)

# Check model. Increase in sales for each unit increase in advertising
salesincrease <- coefficients(fit)["xreg"]

# Forecast fit as fc
fc <- forecast(fit, xreg = rep(10, 6))

# Plot fc with x and y labels
autoplot(fc) + xlab("Month") + ylab("Sales")


# Time plots of demand and temperatures
autoplot(elec[, c("Demand","Temperature")], facets = TRUE)

# Matrix of regressors
xreg <- cbind(MaxTemp = elec[, "Temperature"], 
              MaxTempSq = elec[, "Temperature"]^2, 
              Workday = elec[, "WorkDay"])

# Fit model
fit <- auto.arima(elec[, "Demand"], xreg = xreg)

# Forecast fit one day ahead
forecast(fit, xreg = cbind(MaxTemp=20, MaxTempSq=20^2, Workday=1), h =1)


# using harmonic dynamic regression

# Set up harmonic regressors of order 13
harmonics <- fourier(gasoline, K = 13)


# Fit regression model with ARIMA errors
fit <- auto.arima(gasoline, xreg = harmonics, seasonal = FALSE)

# Forecasts next 3 years
newharmonics <- fourier(gasoline, K = 13, h = 3*52)
fc <- forecast(fit, xreg = newharmonics)

# Plot forecasts fc
autoplot(fc)


# Fit a harmonic regression using order 10 for each type of seasonality
fit <- tslm(taylor ~ fourier(taylor, K = c(10, 10)))

# Forecast 20 working days ahead
fc <- forecast(fit, newdata = data.frame(fourier(taylor, K = c(10,10), h = 48*20)))

# Plot the forecasts
autoplot(fc)

# Check the residuals of fit
checkresiduals(fit)


# Plot the calls data
autoplot(calls)

# Set up the xreg matrix
xreg <- fourier(calls, K = c(10,0))

# Fit a dynamic regression model
fit <- auto.arima(calls, xreg = xreg, seasonal = FALSE, stationary = TRUE)

# Check the residuals
checkresiduals(fit)

# Plot forecasts for 10 working days ahead
fc <- forecast(fit, xreg =  fourier(calls, c(10, 0), h = 1690))
autoplot(fc)


# TBATS

# Plot the gas data
autoplot(gas)

# Fit a TBATS model to the gas data
fit <- tbats(gas)

# Forecast the series for the next 5 years
fc <- forecast(fit, h = 5*12)

# Plot the forecasts
autoplot(fc)

# Record the Box-Cox parameter and the order of the Fourier terms
lambda <- 0.0852
K <- 5


