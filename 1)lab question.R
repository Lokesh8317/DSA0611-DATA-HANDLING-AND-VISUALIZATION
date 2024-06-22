# Load necessary libraries
library(ggplot2)
library(tidyr)
library(dplyr)

data <- data.frame(
  Month = c("Jan", "Feb", "March", "April", "May", "Jun", "July", "August", "Sep", "Oct", "Nov", "Dec"),
  Economic_Condition = c("Good", "Good", "Good", "Good", "Fair", "Fair", "Good", "Bad", "Fair", "Good", "Bad", "Fair"),
  Unemployment_Rate = c(10.7, 9.8, 10.2, 11.2, 15.75, 17.8, 19.4, 25.6, 18.6, 15.6, 26.7, 19.5)
)

data$Unemployment_Rate <- as.numeric(gsub("%", "", data$Unemployment_Rate))

stacked_data <- data

ggplot(stacked_data, aes(fill=Economic_Condition, y=Unemployment_Rate, x=Month)) + 
  geom_bar(position="stack", stat="identity") +
  labs(title="Stacked Bar Plot of Unemployment Rate by Month and Economic Condition",
       x="Month", y="Unemployment Rate (%)")

pie_data <- data %>%
  group_by(Economic_Condition) %>%
  summarize(Unemployment_Rate = sum(Unemployment_Rate))

pie_data$percent <- pie_data$Unemployment_Rate / sum(pie_data$Unemployment_Rate) * 100

ggplot(pie_data, aes(x="", y=percent, fill=Economic_Condition)) +
  geom_bar(width=1, stat="identity") +
  coord_polar("y", start=0) +
  labs(title="Pie Chart of Unemployment Rate by Economic Condition", y="Percentage") +
  theme_void()

ggplot(data, aes(fill=Economic_Condition, y=Unemployment_Rate, x=Month)) + 
  geom_bar(position="dodge", stat="identity") +
  labs(title="Grouped Bar Plot of Unemployment Rate by Month and Economic Condition",
       x="Month", y="Unemployment Rate (%)")