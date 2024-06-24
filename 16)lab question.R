# Load required libraries
library(ggplot2)
library(plotly)

# Define the weather data
weather_data <- data.frame(
  Date = as.Date(c("2023-01-01", "2023-01-02", "2023-01-03", "2023-01-04", "2023-01-05")),
  Temperature = c(10, 12, 8, 15, 14),
  Humidity = c(75, 70, 80, 65, 72),
  Wind_Speed = c(15, 12, 18, 20, 16)
)

# Question 1: Scatter plots to visualize temperature with humidity and wind speed

# Temperature vs Humidity
ggplot(weather_data, aes(x = Humidity, y = Temperature)) +
  geom_point(size = 3) +
  labs(title = "Temperature vs Humidity",
       x = "Humidity (%)", y = "Temperature (°C)")

# Temperature vs Wind Speed
ggplot(weather_data, aes(x = Wind_Speed, y = Temperature)) +
  geom_point(size = 3) +
  labs(title = "Temperature vs Wind Speed",
       x = "Wind Speed (km/h)", y = "Temperature (°C)")

# Question 2: 3D scatter plot with temperature, humidity, and wind speed

plot_ly(weather_data, x = ~Humidity, y = ~Wind_Speed, z = ~Temperature,
        type = "scatter3d", mode = "markers",
        marker = list(size = 5)) %>%
  layout(scene = list(xaxis = list(title = "Humidity (%)"),
                      yaxis = list(title = "Wind Speed (km/h)"),
                      zaxis = list(title = "Temperature (°C)")))

# Question 3: Detecting patterns using 3D surface plot

# Create a grid of humidity and wind speed values
humidity_range <- seq(min(weather_data$Humidity), max(weather_data$Humidity), length.out = 20)
wind_speed_range <- seq(min(weather_data$Wind_Speed), max(weather_data$Wind_Speed), length.out = 20)
grid <- expand.grid(Humidity = humidity_range, Wind_Speed = wind_speed_range)

# Interpolate temperature values
library(akima)
interp <- interp(x = weather_data$Humidity, y = weather_data$Wind_Speed, z = weather_data$Temperature,
                 xo = grid$Humidity, yo = grid$Wind_Speed)

# Create 3D surface plot
surface_plot <- plot_ly(x = interp$x, y = interp$y, z = interp$z, type = "surface")

# Question 5: Compare 3D plots of temperature against humidity and wind speed separately

# Temperature vs Humidity (3D surface)
surface_plot_humidity <- plot_ly(x = interp$x, y = interp$y, z = interp$z, type = "surface",
                                 colorscale = "Viridis", showscale = FALSE) %>%
  layout(scene = list(xaxis = list(title = "Humidity (%)"),
                      yaxis = list(title = "Wind Speed (km/h)"),
                      zaxis = list(title = "Temperature (°C)", range = c(min(weather_data$Temperature), max(weather_data$Temperature)))))

# Temperature vs Wind Speed (3D surface)
surface_plot_wind <- plot_ly(x = interp$x, y = interp$y, z = interp$z, type = "surface",
                             colorscale = "Viridis", showscale = FALSE) %>%
  layout(scene = list(xaxis = list(title = "Humidity (%)"),
                      yaxis = list(title = "Wind Speed (km/h)"),
                      zaxis = list(title = "Temperature (°C)", range = c(min(weather_data$Temperature), max(weather_data$Temperature)))))

# Print or visualize the plots
print(surface_plot)
print(surface_plot_humidity)
print(surface_plot_wind)
