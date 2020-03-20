# Aditional Time Domain Topics Chapter 5
library(astsa)
# ARFIMA
acf(varve, 100)
acf(log(varve), 100)
acf(cumsum(rnorm(1000)), 100)