eu_stocks <- EuStockMarkets
# Plot eu_stocks
plot(EuStockMarkets)

# Use this code to convert prices to returns
returns <- eu_stocks[-1,] / eu_stocks[-1860,] - 1

# Convert returns to ts
returns <- ts(returns, start = c(1991, 130), frequency = 260)

# Plot returns
plot(returns)

# Use this code to convert prices to log returns
logreturns <- diff(log(eu_stocks))

# Plot logreturns
plot(logreturns)

DAX <- EuStockMarkets[,"DAX"]
FTSE <- EuStockMarkets[,"FTSE"]

plot(DAX, FTSE)

pairs(eu_stocks)

logreturns <- diff(log(eu_stocks))

plot(logreturns)

pairs(logreturns)









