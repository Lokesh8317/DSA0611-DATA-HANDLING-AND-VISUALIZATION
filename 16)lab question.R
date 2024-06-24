# Load necessary libraries
library(ggplot2)

# Create the data frame
data <- data.frame(
  ID = c(1, 2, 3, 4, 5),
  Score = c(85, 92, 78, 88, 90)
)

# Add a dummy variable for x-axis
data$Category <- "Score"

# 1. Create a histogram of Score
ggplot(data, aes(x = Score)) +
  geom_histogram(binwidth = 5, fill = "blue", color = "black") +
  labs(title = "Histogram of Scores", x = "Score", y = "Frequency")

# 2. Generate a box plot of Score
ggplot(data, aes(y = Score)) +
  geom_boxplot(fill = "orange", color = "black") +
  labs(title = "Box Plot of Scores", y = "Score")

# 3. Plot a bar chart showing the count of scores in different ranges
data$ScoreRange <- cut(data$Score, breaks = c(0, 50, 100), labels = c("0-50", "51-100"))
ggplot(data, aes(x = ScoreRange)) +
  geom_bar(fill = "green", color = "black") +
  labs(title = "Bar Chart of Score Ranges", x = "Score Range", y = "Count")

# 4. Create a density plot of Score
ggplot(data, aes(x = Score)) +
  geom_density(fill = "purple", alpha = 0.5) +
  labs(title = "Density Plot of Scores", x = "Score", y = "Density")

# 5. Plot a violin plot of Score
ggplot(data, aes(x = Category, y = Score)) +
  geom_violin(fill = "lightblue", color = "black") +
  labs(title = "Violin Plot of Scores", x = "", y = "Score")
