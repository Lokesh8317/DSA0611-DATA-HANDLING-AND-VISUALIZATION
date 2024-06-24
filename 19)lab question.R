# Load necessary libraries
library(ggplot2)
library(plotly)
library(akima)

# Create a data frame with the given environmental data
env_data <- data.frame(
  Location = c('A', 'B', 'C', 'D', 'E'),
  Temperature = c(15, 20, 18, 12, 17),
  Humidity = c(65, 70, 68, 60, 72),
  CO2_Levels = c(400, 450, 420, 380, 430)
)

# 3D scatter plot to visualize the relationship between temperature, humidity, and CO2 levels
scatter_plot <- plot_ly(env_data, x = ~Temperature, y = ~Humidity, z = ~CO2_Levels, 
                        type = 'scatter3d', mode = 'markers', marker = list(size = 5)) %>%
  layout(scene = list(xaxis = list(title = 'Temperature (°C)'),
                      yaxis = list(title = 'Humidity (%)'),
                      zaxis = list(title = 'CO2 Levels (ppm)')))
scatter_plot

# Generating a 3D surface plot to illustrate how CO2 levels change with variations in both temperature and humidity
# Use interpolation to create a smooth surface
interp_data <- with(env_data, interp(x = Temperature, y = Humidity, z = CO2_Levels, duplicate = "mean"))
surface_plot <- plot_ly(x = interp_data$x, y = interp_data$y, z = interp_data$z, type = 'surface') %>%
  layout(scene = list(xaxis = list(title = 'Temperature (°C)'),
                      yaxis = list(title = 'Humidity (%)'),
                      zaxis = list(title = 'CO2 Levels (ppm)')))
surface_plot

# Compare the 3D plots of CO2 levels against both temperature and humidity separately
# CO2 vs Temperature
temp_plot <- plot_ly(env_data, x = ~Temperature, y = ~CO2_Levels, type = 'scatter', mode = 'markers') %>%
  layout(xaxis = list(title = 'Temperature (°C)'),
         yaxis = list(title = 'CO2 Levels (ppm)'))
temp_plot

# CO2 vs Humidity
humidity_plot <- plot_ly(env_data, x = ~Humidity, y = ~CO2_Levels, type = 'scatter', mode = 'markers') %>%
  layout(xaxis = list(title = 'Humidity (%)'),
         yaxis = list(title = 'CO2 Levels (ppm)'))
humidity_plot
