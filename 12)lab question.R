
library(ggplot2)
library(dplyr)
library(wordcloud2)

data <- data.frame(
  Word = c("Apple", "Orange", "Banana", "Grape", "Cherry"),
  Frequency = c(15, 10, 8, 12, 5)
)

wordcloud2(data, size = 1)

ggplot(data, aes(x = reorder(Word, -Frequency), y = Frequency)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  ggtitle("Bar Plot of Top 5 Most Frequent Words") +
  xlab("Word") +
  ylab("Frequency") +
  theme_minimal()

ggplot(data, aes(x = "Words", y = Frequency, fill = Word)) +
  geom_bar(stat = "identity") +
  ggtitle("Stacked Bar Chart of Word Frequencies") +
  xlab("Words") +
  ylab("Frequency") +
  theme_minimal()

ggplot(data, aes(x = "", y = Frequency, fill = Word)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 0) +
  geom_text(aes(label = paste0(round(Frequency/sum(Frequency)*100, 1), "%")), 
            position = position_stack(vjust = 0.5)) +
  ggtitle("Pie Chart of Word Frequencies") +
  theme_void()

ggplot(data, aes(x = Frequency)) +
  geom_histogram(binwidth = 1, fill = "green", color = "black") +
  ggtitle("Histogram of Frequencies") +
  xlab("Frequency") +
  ylab("Count") +
  theme_minimal()
