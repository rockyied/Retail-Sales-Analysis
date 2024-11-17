#Clear directory
rm(list=ls())

#Load dataset
data <- read.csv("C:/Users/Desktop/train.csv", header=TRUE, stringsAsFactors=FALSE)

#Sumary of dataset
summary(data)

#Number of rows and columns
dim(data)

#Converting to date format
data$Order.Date <- as.Date(data$Order.Date, format = "%d/%m/%Y")
data$Ship.Date <- as.Date(data$Ship.Date, format = "%d/%m/%Y")
str(data)

library(dplyr)
library(tidyr)

# Extract year and quarter from Order.Date
sales_by_year_quarter <- data %>%
  mutate(Year = lubridate::year(Order.Date),
         Quarter = lubridate::quarter(Order.Date)) %>%
  group_by(Year, Quarter) %>%
  summarize(Revenue = sum(Sales)) %>%
  ungroup()

# Print the resulting dataset
print(sales_by_year_quarter)


library(forecast)
# Create a time series object
newData <- ts(sales_by_year_quarter$Revenue, start = c(2015, 1), end = c(2018, 4), frequency = 4)

# Holdout partitioning
TData <- window(newData, end = c(2016, 4))
VData <- window(newData, start = c(2017, 1))

# Estimate linear model
Reg1 <- tslm(TData ~ trend + season)

# Estimate quadratic model
Reg2 <- tslm(TData ~ trend + I(trend^2) + season)

# Find number of observations in training set
nV <- length(VData)

# Make forecasts for the validation set for each model
fReg1 <- forecast(Reg1, h = nV)
fReg2 <- forecast(Reg2, h = nV)

# View performance measures
accuracy(fReg1, VData)
accuracy(fReg2, VData)

# Now estimate a model on the entire dataset
RegFin <- tslm(newData ~ trend + season)
forecast(RegFin, h = 4)
