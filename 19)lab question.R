# Load necessary libraries
library(ggplot2)
library(dplyr)
library(tidyr)

# Create the data frame
data <- data.frame(
  ID = c(1, 2, 3, 4, 5),
  Rating = c("Good", "Excellent", "Fair", "Poor", "Good")
)

# 1. Bar plot showing the count of each Rating category
ggplot(data, aes(x = Rating)) +
  geom_bar() +
  xlab("Rating") +
  ylab("Count") +
  ggtitle("Count of Each Rating Category")

# 2. Stacked bar plot of Rating over IDs
ggplot(data, aes(x = ID, fill = Rating)) +
  geom_bar() +
  xlab("ID") +
  ylab("Count") +
  ggtitle("Stacked Bar Plot of Rating over IDs")

# 3. Pie chart representing the distribution of Ratings
rating_counts <- data %>%
  count(Rating)

ggplot(rating_counts, aes(x = "", y = n, fill = Rating)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  ggtitle("Distribution of Ratings") +
  scale_fill_discrete(name = "Rating")

# 4. Lollipop plot showing the average Rating
rating_avg <- data %>%
  mutate(Rating = factor(Rating, levels = c("Poor", "Fair", "Good", "Excellent"))) %>%
  group_by(Rating) %>%
  summarise(Average_ID = mean(ID))

ggplot(rating_avg, aes(x = Rating, y = Average_ID)) +
  geom_segment(aes(x = Rating, xend = Rating, y = 0, yend = Average_ID), color = "blue") +
  geom_point(color = "blue", size = 3) +
  xlab("Rating") +
  ylab("Average ID") +
  ggtitle("Average Rating")

# 5. Bar chart showing the frequency of each Rating category
ggplot(data, aes(x = Rating)) +
  geom_bar() +
  xlab("Rating") +
  ylab("Frequency") +
  ggtitle("Frequency of Each Rating Category")
