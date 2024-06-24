# Load necessary libraries
library(ggplot2)
library(dplyr)

# Create the data frame
data <- data.frame(
  ID = c(1, 2, 3, 4, 5),
  Rating = factor(c("Good", "Excellent", "Fair", "Poor", "Good"), 
                  levels = c("Poor", "Fair", "Good", "Excellent"), 
                  ordered = TRUE)
)

# 1. Create a bar plot showing the count of each Rating category
ggplot(data, aes(x = Rating)) +
  geom_bar() +
  labs(title = "Count of Each Rating Category", x = "Rating", y = "Count")

# 2. Generate a stacked bar plot of Rating over IDs
ggplot(data, aes(x = factor(ID), fill = Rating)) +
  geom_bar(position = "stack") +
  labs(title = "Stacked Bar Plot of Rating over IDs", x = "ID", y = "Count")

# 3. Plot a pie chart representing the distribution of Ratings
rating_counts <- data %>% 
  count(Rating) %>% 
  mutate(percentage = n / sum(n) * 100)

ggplot(rating_counts, aes(x = "", y = percentage, fill = Rating)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  labs(title = "Pie Chart of Rating Distribution") +
  theme_void()

# 4. Create a lollipop plot showing the average Rating
# Converting Rating to numeric values for calculation
data <- data %>%
  mutate(Rating_numeric = as.numeric(Rating))

average_rating <- data %>%
  group_by(Rating) %>%
  summarise(avg_rating = mean(Rating_numeric))

ggplot(average_rating, aes(x = Rating, y = avg_rating)) +
  geom_segment(aes(x = Rating, xend = Rating, y = 0, yend = avg_rating)) +
  geom_point(size = 3) +
  labs(title = "Lollipop Plot of Average Rating", x = "Rating", y = "Average")

# 5. Plot a bar chart showing the frequency of each Rating category
ggplot(data, aes(x = Rating)) +
  geom_bar() +
  labs(title = "Frequency of Each Rating Category", x = "Rating", y = "Frequency")
