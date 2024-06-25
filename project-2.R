# Load necessary libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(scatterplot3d)

# Define the data
data <- data.frame(
  StoreID = 1:30,
  Region = c('North', 'South', 'East', 'West', 'North', 'South', 'East', 'West', 'North', 'South', 'East', 'West', 'North', 'South', 'East', 'West', 'North', 'South', 'East', 'West', 'North', 'South', 'East', 'West', 'North', 'South', 'East', 'West', 'North', 'South'),
  Month = c('January', 'January', 'January', 'January', 'February', 'February', 'February', 'February', 'March', 'March', 'March', 'March', 'April', 'April', 'April', 'April', 'May', 'May', 'May', 'May', 'June', 'June', 'June', 'June', 'July', 'July', 'July', 'July', 'August', 'August'),
  TotalSales = c(10000, 15000, 12000, 13000, 9000, 16000, 14000, 11000, 11000, 17000, 13000, 15000, 12000, 18000, 16000, 14000, 13000, 19000, 15000, 16000, 14000, 20000, 17000, 15000, 15000, 21000, 16000, 14000, 16000, 22000),
  NumberOfCustomers = c(500, 700, 600, 650, 450, 800, 700, 550, 550, 850, 650, 750, 600, 900, 800, 700, 650, 950, 750, 800, 700, 1000, 850, 750, 750, 1050, 800, 700, 800, 1100),
  AverageTransactionValue = c(20, 21.43, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20)
)

# Data Cleaning
# Check for missing values
sapply(data, function(x) sum(is.na(x)))

# Check data types
str(data)

# Ensure the data types are correct
data$Region <- as.factor(data$Region)
data$Month <- as.factor(data$Month)
data$TotalSales <- as.numeric(data$TotalSales)
data$NumberOfCustomers <- as.numeric(data$NumberOfCustomers)
data$AverageTransactionValue <- as.numeric(data$AverageTransactionValue)

# Data Processing
# Summary statistics
summary(data)

# Group by Region and Month
grouped_data <- data %>%
  group_by(Region, Month) %>%
  summarise(
    TotalSales = sum(TotalSales),
    AverageCustomers = mean(NumberOfCustomers),
    AverageTransactionValue = mean(AverageTransactionValue)
  )

# Sales Analysis
# Summary plots
boxplot(TotalSales ~ Region, data = data, main = "Total Sales by Region", ylab = "Total Sales")
boxplot(TotalSales ~ Month, data = data, main = "Total Sales by Month", ylab = "Total Sales")

# Visualization – 15 plots

# 2D Visualization
# Scatter plot of Number of Customers vs. Total Sales colored by Region
ggplot(data, aes(x = NumberOfCustomers, y = TotalSales, color = Region)) + 
  geom_point() + 
  labs(title = "Number of Customers vs. Total Sales by Region")

# Scatter plot of Number of Customers vs. Total Sales colored by Month
ggplot(data, aes(x = NumberOfCustomers, y = TotalSales, color = Month)) + 
  geom_point() + 
  labs(title = "Number of Customers vs. Total Sales by Month")

# Bar plot of Total Sales by Region
ggplot(data, aes(x = Region, y = TotalSales, fill = Region)) + 
  geom_bar(stat = "identity") + 
  labs(title = "Total Sales by Region")

# Bar plot of Total Sales by Month
ggplot(data, aes(x = Month, y = TotalSales, fill = Month)) + 
  geom_bar(stat = "identity") + 
  labs(title = "Total Sales by Month")

# Histogram of Total Sales
ggplot(data, aes(x = TotalSales)) + 
  geom_histogram(binwidth = 1000, fill = "blue", color = "black") + 
  labs(title = "Total Sales Distribution")

# Boxplot of Total Sales by Region
ggplot(data, aes(x = Region, y = TotalSales, fill = Region)) + 
  geom_boxplot() + 
  labs(title = "Total Sales by Region")

# Density plot of Total Sales by Region
ggplot(data, aes(x = TotalSales, fill = Region)) + 
  geom_density(alpha = 0.5) + 
  labs(title = "Total Sales Density by Region")

# Violin plot of Total Sales by Region
ggplot(data, aes(x = Region, y = TotalSales, fill = Region)) + 
  geom_violin() + 
  labs(title = "Total Sales Distribution by Region")

# 3D Visualization
# Convert Region to numeric for 3D plotting
data$RegionNumeric <- as.numeric(data$Region)

# 3D scatter plot using scatterplot3d
scatterplot3d(data$TotalSales, data$NumberOfCustomers, data$RegionNumeric,
              xlab = "Total Sales", ylab = "Number Of Customers", zlab = "Region",
              pch = 16, color = "blue")

# Scatter plot of Total Sales vs. Number of Customers colored by Region with trend line
ggplot(data, aes(x = TotalSales, y = NumberOfCustomers, color = Region)) + 
  geom_point() + 
  geom_smooth(method = "lm") + 
  labs(title = "Total Sales vs. Number of Customers by Region with Trend Line")

# Scatter plot of Total Sales vs. Number of Customers colored by Month with trend line
ggplot(data, aes(x = TotalSales, y = NumberOfCustomers, color = Month)) + 
  geom_point() + 
  geom_smooth(method = "lm") + 
  labs(title = "Total Sales vs. Number of Customers by Month with Trend Line")

# Scatter plot of Total Sales vs. Average Transaction Value colored by Region
ggplot(data, aes(x = TotalSales, y = AverageTransactionValue, color = Region)) + 
  geom_point() + 
  labs(title = "Total Sales vs. Average Transaction Value by Region")

# Scatter plot of Total Sales vs. Average Transaction Value colored by Month
ggplot(data, aes(x = TotalSales, y = AverageTransactionValue, color = Month)) + 
  geom_point() + 
  labs(title = "Total Sales vs. Average Transaction Value by Month")

# Scatter plot of Number of Customers vs. Average Transaction Value colored by Region
ggplot(data, aes(x = NumberOfCustomers, y = AverageTransactionValue, color = Region)) + 
  geom_point() + 
  labs(title = "Number of Customers vs. Average Transaction Value by Region")

# Scatter plot of Number of Customers vs. Average Transaction Value colored by Month
ggplot(data, aes(x = NumberOfCustomers, y = AverageTransactionValue, color = Month)) + 
  geom_point() + 
  labs(title = "Number of Customers vs. Average Transaction Value by Month")