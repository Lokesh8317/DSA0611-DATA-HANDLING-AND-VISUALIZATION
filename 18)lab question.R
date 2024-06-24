# Load necessary libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(ggmosaic)

# Create the data frame
data <- data.frame(
  ID = c(1, 2, 3, 4, 5),
  Variable1 = c(0, 1, 0, 1, 0),
  Variable2 = c(1, 0, 1, 1, 0),
  Variable3 = c(0, 1, 1, 0, 0)
)

# 1. Bar plot showing the count of each value (0 or 1) for Variable1
ggplot(data, aes(x = factor(Variable1))) +
  geom_bar() +
  xlab("Variable1") +
  ylab("Count") +
  ggtitle("Count of Each Value for Variable1")

# 2. Stacked bar plot of Variable1, Variable2, and Variable3
data_long <- data %>%
  pivot_longer(cols = starts_with("Variable"), names_to = "Variable", values_to = "Value")

ggplot(data_long, aes(x = Variable, fill = factor(Value))) +
  geom_bar(position = "stack") +
  xlab("Variable") +
  ylab("Count") +
  ggtitle("Stacked Bar Plot of Variable1, Variable2, and Variable3") +
  scale_fill_discrete(name = "Value")

# 3. Pie chart representing the proportion of 0s and 1s in Variable2
variable2_count <- data %>%
  count(Variable2) %>%
  mutate(Percentage = n / sum(n) * 100)

ggplot(variable2_count, aes(x = "", y = Percentage, fill = factor(Variable2))) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  ggtitle("Proportion of 0s and 1s in Variable2") +
  scale_fill_discrete(name = "Variable2")

# 4. Mosaic plot to visualize the association between Variable1 and Variable2
ggplot(data) +
  geom_mosaic(aes(x = product(factor(Variable1), factor(Variable2)), fill = factor(Variable2))) +
  xlab("Variable1") +
  ylab("Variable2") +
  ggtitle("Mosaic Plot of Variable1 and Variable2")

# 5. Bar chart showing the frequency of each combination of values in Variable1 and Variable2
combination_count <- data %>%
  count(Variable1, Variable2)

ggplot(combination_count, aes(x = interaction(Variable1, Variable2), y = n, fill = factor(Variable1))) +
  geom_bar(stat = "identity") +
  xlab("Combination of Variable1 and Variable2") +
  ylab("Count") +
  ggtitle("Frequency of Each Combination of Values in Variable1 and Variable2") +
  scale_fill_discrete(name = "Variable1")
