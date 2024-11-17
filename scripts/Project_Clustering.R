#Clear directory
rm(list=ls())

library(cluster)

library(dplyr)

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
summary(data$Order.Date)

# Checking for missing values in each column
which(is.na(data))
missing_rows <- data[rowSums(is.na(data)) > 0, ]
missing_rows

#Checking for number of instances where city is burlington and state is vermont
length(which(data$City=='Burlington' & data$State=='Vermont'))

# imputing the missing values in postal.code column
data$Postal.Code[is.na(data$Postal.Code)] <- "05402"


# Group sales by postal code and calculate total sales for each postal code
sales_by_zip <- data %>%
  group_by(Postal.Code) %>%
  summarise(Total_Sales = sum(Sales, na.rm = TRUE))

# View the first few rows of the sales by zip data
head(sales_by_zip)

numeric_data <- sales_by_zip

# Scale the numerical columns
str(numeric_data)
numeric_data$Postal.Code=as.numeric(numeric_data$Postal.Code)
scaled_data <- scale(numeric_data)

#Euclidean method
clustering <- dist(scaled_data, method = "euclidean")


aResult <- agnes(clustering, diss = TRUE, method = "ward")
##print the result
aResult

##produce the banner plot and dendrogram
plot(aResult)

##where 'k' = 5
aClusters <- cutree(aResult, k = 5)

##append the result to the original data frame
myData<- data.frame(sales_by_zip, aClusters)
View(myData)

##we can now obtain summary statistics for each cluster
summary(subset(myData, aClusters == 1))
summary(subset(myData, aClusters == 2))
summary(subset(myData, aClusters == 3))
summary(subset(myData, aClusters == 4))
summary(subset(myData, aClusters == 5))

##to identify the number of observations in each cluster type
summary(as.factor(aClusters))
