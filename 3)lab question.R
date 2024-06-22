# Load necessary libraries
library(ggplot2)
library(tidyr)
library(dplyr)
library(lubridate)

data <- read.csv("C:/Users/Pavan/Downloads/weather_dataset.csv")

data$Date <- dmy(data$Date)
ggplot(data, aes(x = Date)) +
  geom_line(aes(y = Station_A_Temperature, color = "Station A Temperature")) +
  geom_line(aes(y = Station_A_Precipitation * 10, color = "Station A Precipitation")) +  # Scale precipitation for better visualization
  geom_line(aes(y = Station_B_Temperature, color = "Station B Temperature")) +
  geom_line(aes(y = Station_B_Precipitation * 10, color = "Station B Precipitation")) +  # Scale precipitation for better visualization
  labs(title = "Time Series Plot of Temperature and Precipitation",
       x = "Date",
       y = "Value",
       color = "Legend") +
  scale_y_continuous(sec.axis = sec_axis(~./10, name = "Precipitation (scaled)")) +
  theme_minimal()
ggplot(data, aes(x = Date)) +
  geom_line(aes(y = Station_A_Temperature, color = "Station A Temperature")) +
  geom_line(aes(y = Station_B_Temperature, color = "Station B Temperature")) +
  labs(title = "Line Plot of Temperature",
       x = "Date",
       y = "Temperature (°C)",
       color = "Station") +
  theme_minimal()
stacked_data <- data %>%
  select(Date, Station_A_Temperature, Station_A_Precipitation, Station_B_Temperature, Station_B_Precipitation) %>%
  gather(key = "Variable", value = "Value", -Date)

ggplot(stacked_data, aes(x = Date, y = Value, fill = Variable)) +
  geom_bar(position = "stack", stat = "identity") +
  labs(title = "Stacked Bar Plot of Temperature and Precipitation",
       x = "Date",
       y = "Value",
       fill = "Variable") +
  theme_minimal()