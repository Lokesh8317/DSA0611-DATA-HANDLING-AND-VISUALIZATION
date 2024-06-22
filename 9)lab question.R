library(ggplot2)
library(maps)
library(dplyr)

cities <- data.frame(
  City = c("New York", "Los Angeles", "Chicago", "Houston", "Phoenix"),
  Latitude = c(40.7128, 34.0522, 41.8781, 29.7604, 33.4484),
  Longitude = c(-74.0060, -118.2437, -87.6298, -95.3698, -112.0740),
  Population = c(8398748, 3990456, 2705994, 2325502, 1660272)
)

world_map <- map_data("world")
ggplot() +
  geom_polygon(data = world_map, aes(x = long, y = lat, group = group), fill = "lightgray", color = "white") +
  geom_point(data = cities, aes(x = Longitude, y = Latitude), color = "blue", size = 3) +
  ggtitle("Scatter Plot of Cities") +
  xlab("Longitude") +
  ylab("Latitude")

ggplot() +
  geom_polygon(data = world_map, aes(x = long, y = lat, group = group), fill = "lightgray", color = "white") +
  geom_point(data = cities, aes(x = Longitude, y = Latitude, size = Population), color = "blue", alpha = 0.6) +
  scale_size_continuous(range = c(1, 10)) +
  ggtitle("Bubble Map of Cities by Population") +
  xlab("Longitude") +
  ylab("Latitude")

ggplot() +
  geom_polygon(data = world_map, aes(x = long, y = lat, group = group), fill = "lightgray", color = "white") +
  geom_point(data = cities, aes(x = Longitude, y = Latitude, color = Population), size = 5) +
  scale_color_gradient(low = "yellow", high = "red") +
  ggtitle("Choropleth Map of Population by City") +
  xlab("Longitude") +
  ylab("Latitude")

ggplot(cities %>% arrange(desc(Population)), aes(x = reorder(City, -Population), y = Population)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(title = "Top 5 Cities by Population", x = "City", y = "Population") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(cities, aes(x = Longitude, y = Latitude, fill = Population)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "yellow", high = "red", name = "Population") +
  labs(title = "Density Heatmap of Cities Based on Population", x = "Longitude", y = "Latitude") +
  coord_fixed(ratio = 1.3)