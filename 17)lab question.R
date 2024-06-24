# Load necessary libraries
library(wordcloud)
library(ggplot2)
library(dplyr)
library(ggthemes)

# Data
words <- c("Apple", "Orange", "Banana", "Grape", "Cherry")
frequencies <- c(15, 10, 8, 12, 5)
word_data <- data.frame(words, frequencies)

# 1. Word Cloud
wordcloud(words = word_data$words, freq = word_data$frequencies, scale=c(3,0.5), colors=brewer.pal(6, "Dark2"))

# 2. Bar Plot of the Top 5 Most Frequent Words
top_5_words <- word_data %>%
  arrange(desc(frequencies)) %>%
  head(5)

ggplot(top_5_words, aes(x = reorder(words, -frequencies), y = frequencies)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  theme_minimal() +
  labs(title = "Top 5 Most Frequent Words",
       x = "Words",
       y = "Frequency")

# 3. Stacked Bar Chart Showing Word Frequencies
ggplot(word_data, aes(x = "", y = frequencies, fill = words)) +
  geom_bar(stat = "identity") +
  coord_polar(theta = "y") +
  theme_minimal() +
  labs(title = "Stacked Bar Chart of Word Frequencies")

# 4. Pie Chart Representing the Distribution of Word Frequencies
ggplot(word_data, aes(x = "", y = frequencies, fill = words)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  theme_minimal() +
  labs(title = "Pie Chart of Word Frequencies")

# 5. Histogram of Frequency
ggplot(word_data, aes(x = frequencies)) +
  geom_histogram(binwidth = 1, fill = "coral", color = "black") +
  theme_minimal() +
  labs(title = "Histogram of Word Frequencies",
       x = "Frequency",
       y = "Count")
