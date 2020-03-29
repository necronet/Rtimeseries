library(here)
library(readxl)
library(fpp2)
# Read the data from Excel into R
mydata <- read_excel(here("datacamp","forecasting_in_R","exercise1.xlsx"))

# Look at the first few lines of mydata
head(mydata)

# Create a ts object called myts
myts <- ts(mydata[,2:4], start = c(1981, 1), frequency = 4)


# Plot the data with facetting
autoplot(myts, facets = TRUE)

# Plot the data without facetting
autoplot(myts, facets = FALSE)

# Plot the three series
autoplot(gold)
autoplot(woolyrnq)
autoplot(gas)

# Find the outlier in the gold series
goldoutlier <- which.max(gold)

# Look at the seasonal frequencies of the three series
frequency(gold)
frequency(woolyrnq)
frequency(gas)

# Using fpp2 data

autoplot(a10)
ggseasonplot(a10)
ggseasonplot(a10, polar = TRUE)

beer <- window(ausbeer, start=1992)

# Make plots of the beer data
autoplot(beer)
ggsubseriesplot(beer)

# Looking inside what ggsubsereriesplot does
data <- data.frame(y = as.numeric(beer), year = trunc(time(beer)), season = as.numeric(cycle(beer)))
seasonwidth <- (max(data$year) - min(data$year)) * 1.05
data$time <- data$season + 0.025 + (data$year - min(data$year)) / seasonwidth
avgLines <- stats::aggregate(data$y, by = list(data$season), FUN = mean)
colnames(avgLines) <- c("season", "avg")
data <- merge(data, avgLines, by = "season")


# Seasonal trend and cycles.

# Create an autoplot of the oil data
autoplot(oil)

# Create a lag plot of the oil data
gglagplot(oil)

# Create an ACF plot of the oil data
ggAcf(oil)


# Plot the annual sunspot numbers
autoplot(sunspot.year)
ggAcf(sunspot.year)

# Save the lag corresponding to maximum autocorrelation
maxlag_sunspot <- 1

# Plot the traffic on the Hyndsight blog
autoplot(hyndsight)
ggAcf(hyndsight)

# Save the lag corresponding to maximum autocorrelation
maxlag_hyndsight <- 7

# White noise test
autoplot(goog)

autoplot(diff(goog))

ggAcf(diff(goog))

Box.test(diff(goog), lag = 10, type = "Ljung")






