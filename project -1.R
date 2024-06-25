# Load necessary libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(scatterplot3d)

# Define the data
data <- data.frame(
  CustomerID = 1:30,
  Age = c(35, 28, 42, 25, 38, 45, 23, 34, 40, 30, 29, 50, 32, 47, 36, 27, 41, 33, 39, 26, 48, 31, 44, 37, 43, 24, 46, 49, 21, 22),
  Gender = c('Male', 'Female', 'Male', 'Female', 'Male', 'Female', 'Male', 'Female', 'Male', 'Female', 'Male', 'Female', 'Male', 'Female', 'Male', 'Female', 'Male', 'Female', 'Male', 'Female', 'Male', 'Female', 'Male', 'Female', 'Male', 'Female', 'Male', 'Female', 'Male', 'Female'),
  PurchaseAmount = c(100, 80, 120, 90, 110, 150, 60, 130, 95, 85, 70, 160, 75, 140, 115, 65, 125, 135, 105, 95, 145, 90, 120, 125, 130, 85, 155, 140, 50, 60),
  MembershipLevel = c('Gold', 'Silver', 'Bronze', 'Bronze', 'Silver', 'Gold', 'Silver', 'Gold', 'Bronze', 'Silver', 'Bronze', 'Gold', 'Silver', 'Bronze', 'Gold', 'Silver', 'Bronze', 'Gold', 'Silver', 'Bronze', 'Gold', 'Silver', 'Bronze', 'Gold', 'Silver', 'Bronze', 'Gold', 'Silver', 'Bronze', 'Silver')
)
# Checking for missing values
sapply(data, function(x) sum(is.na(x)))

# Check data types
str(data)

# Convert MembershipLevel to factor
data$MembershipLevel <- as.factor(data$MembershipLevel)
# Summary statistics
summary(data)

# Group by Gender and Membership Level
grouped_data <- data %>%
  group_by(Gender, MembershipLevel) %>%
  summarise(AverageAge = mean(Age), TotalPurchase = sum(PurchaseAmount))
# Summary plots
boxplot(Age ~ Gender, data = data, main = "Age Distribution by Gender", ylab = "Age")
boxplot(PurchaseAmount ~ Gender, data = data, main = "Purchase Amount by Gender", ylab = "Purchase Amount")
boxplot(PurchaseAmount ~ MembershipLevel, data = data, main = "Purchase Amount by Membership Level", ylab = "Purchase Amount")
# Scatter plot of Age vs. Purchase Amount colored by Gender
ggplot(data, aes(x = Age, y = PurchaseAmount, color = Gender)) + 
  geom_point() + 
  labs(title = "Age vs. Purchase Amount by Gender")

# Scatter plot of Age vs. Purchase Amount colored by Membership Level
ggplot(data, aes(x = Age, y = PurchaseAmount, color = MembershipLevel)) + 
  geom_point() + 
  labs(title = "Age vs. Purchase Amount by Membership Level")

# 3D scatter plot using scatterplot3d
scatterplot3d(data$Age, data$PurchaseAmount, as.numeric(data$MembershipLevel),
              xlab = "Age", ylab = "Purchase Amount", zlab = "Membership Level",
              pch = 16, color = "blue")

# Bar plot of Purchase Amount by Membership Level
ggplot(data, aes(x = MembershipLevel, y = PurchaseAmount, fill = MembershipLevel)) + 
  geom_bar(stat = "identity") + 
  labs(title = "Total Purchase Amount by Membership Level")

# Histogram of Age
ggplot(data, aes(x = Age)) + 
  geom_histogram(binwidth = 5, fill = "blue", color = "black") + 
  labs(title = "Age Distribution")

# Boxplot of Purchase Amount by Membership Level
ggplot(data, aes(x = MembershipLevel, y = PurchaseAmount, fill = MembershipLevel)) + 
  geom_boxplot() + 
  labs(title = "Purchase Amount by Membership Level")

# Density plot of Age by Gender
ggplot(data, aes(x = Age, fill = Gender)) + 
  geom_density(alpha = 0.5) + 
  labs(title = "Age Density by Gender")

# Violin plot of Purchase Amount by Gender
ggplot(data, aes(x = Gender, y = PurchaseAmount, fill = Gender)) + 
  geom_violin() + 
  labs(title = "Purchase Amount Distribution by Gender")

# Scatter plot of Age vs. Purchase Amount colored by Membership Level with trend line
ggplot(data, aes(x = Age, y = PurchaseAmount, color = MembershipLevel)) + 
  geom_point() + 
  geom_smooth(method = "lm") + 
  labs(title = "Age vs. Purchase Amount by Membership Level with Trend Line")
# Generate more plots
plot_list <- list(
  ggplot(data, aes(x = Age, y = PurchaseAmount, color = Gender)) + geom_point(),
  ggplot(data, aes(x = Age, y = PurchaseAmount, color = MembershipLevel)) + geom_point(),
  ggplot(data, aes(x = MembershipLevel, y = PurchaseAmount, fill = MembershipLevel)) + geom_boxplot(),
  ggplot(data, aes(x = Gender, y = PurchaseAmount, fill = Gender)) + geom_boxplot(),
  ggplot(data, aes(x = Age, fill = Gender)) + geom_histogram(binwidth = 5),
  ggplot(data, aes(x = Age, fill = MembershipLevel)) + geom_histogram(binwidth = 5),
  ggplot(data, aes(x = MembershipLevel, y = PurchaseAmount, fill = MembershipLevel)) + geom_bar(stat = "identity"),
  ggplot(data, aes(x = Gender, y = PurchaseAmount, fill = Gender)) + geom_bar(stat = "identity"),
  ggplot(data, aes(x = Age, fill = Gender)) + geom_density(alpha = 0.5),
  ggplot(data, aes(x = Age, fill = MembershipLevel)) + geom_density(alpha = 0.5),
  ggplot(data, aes(x = Gender, y = PurchaseAmount, fill = Gender)) + geom_violin(),
  ggplot(data, aes(x = MembershipLevel, y = PurchaseAmount, fill = MembershipLevel)) + geom_violin(),
  ggplot(data, aes(x = Age, y = PurchaseAmount, color = MembershipLevel)) + geom_point() + geom_smooth(method = "lm"),
  ggplot(data, aes(x = Age, y = PurchaseAmount, color = Gender)) + geom_point() + geom_smooth(method = "lm")
)

# Print all plots
for (plot in plot_list) {
  print(plot)
}