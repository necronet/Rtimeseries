library(here)
library(readxl)
# Read the data from Excel into R
mydata <- read_excel(here("datacamp","forecasting_in_R","exercise1.xlsx"))

# Look at the first few lines of mydata
head(mydata)

# Create a ts object called myts
myts <- ts(mydata[,2:4], start = c(1981, 1), frequency = 4)
