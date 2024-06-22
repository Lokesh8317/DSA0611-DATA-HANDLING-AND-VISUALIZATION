library(ggplot2)
library(dplyr)

# Create the data frame
data <- data.frame(
  ID = c(1, 2, 3, 4, 5),
  Score = c(85, 92, 78, 88, 90)
)

# Histogram of Score
ggplot(data, aes(x = Score)) +
  geom_histogram(binwidth = 5, fill = "blue", color = "black") +
  ggtitle("Histogram of Scores") +
  xlab("Score") +
  ylab("Frequency") +
  theme_minimal()

# Box plot of Score
ggplot(data, aes(y = Score)) +
  geom_boxplot(fill = "orange", color = "black") +
  ggtitle("Box Plot of Scores") +
  ylab("Score") +
  theme_minimal()

# Bar chart of score ranges
data <- data %>%
  mutate(Score_Range = cut(Score, breaks = c(0, 50, 100), labels = c("0-50", "51-100")))

ggplot(data, aes(x = Score_Range)) +
  geom_bar(fill = "purple", color = "black") +
  ggtitle("Bar Chart of Score Ranges") +
  xlab("Score Range") +
  ylab("Count") +
  theme_minimal()

# Density plot of Score
ggplot(data, aes(x = Score)) +
  geom_density(fill = "green", alpha = 0.5) +
  ggtitle("Density Plot of Scores") +
  xlab("Score") +
  ylab("Density") +
  theme_minimal()

# Violin plot of Score
ggplot(data, aes(x = factor(1), y = Score)) +
  geom_violin(fill = "pink", color = "black") +
  ggtitle("Violin Plot of Scores") +
  xlab("") +
  ylab("Score") +
  theme_minimal()