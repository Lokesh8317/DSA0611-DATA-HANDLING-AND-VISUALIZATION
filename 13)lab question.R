library(ggplot2)
library(dplyr)
library(ggmosaic)
library(tidyr)
library(reshape2)
library(vcd)

data <- data.frame(
  ID = c(1, 2, 3, 4, 5),
  Variable1 = c(0, 1, 0, 1, 0),
  Variable2 = c(1, 0, 1, 1, 0),
  Variable3 = c(0, 1, 1, 0, 0)
)

ggplot(data, aes(x = as.factor(Variable1))) +
  geom_bar(fill = "blue", color = "black") +
  ggtitle("Count of Each Value for Variable1") +
  xlab("Variable1") +
  ylab("Count") +
  theme_minimal()

data_melted <- melt(data, id.vars = "ID", variable.name = "Variable", value.name = "Value")

ggplot(data_melted, aes(x = Variable, fill = as.factor(Value))) +
  geom_bar(position = "stack") +
  ggtitle("Stacked Bar Plot of Variable1, Variable2, and Variable3") +
  xlab("Variable") +
  ylab("Count") +
  scale_fill_manual(values = c("0" = "red", "1" = "green"), name = "Value") +
  theme_minimal()

var2_data <- data %>%
  count(Variable2) %>%
  mutate(percentage = n / sum(n) * 100)

ggplot(var2_data, aes(x = "", y = percentage, fill = as.factor(Variable2))) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 0) +
  geom_text(aes(label = paste0(round(percentage, 1), "%")), 
            position = position_stack(vjust = 0.5)) +
  ggtitle("Pie Chart of Variable2 Proportions") +
  theme_void()

comb_data <- data %>%
  count(Variable1, Variable2)

ggplot(comb_data, aes(x = interaction(Variable1, Variable2), y = n, fill = as.factor(Variable1))) +
  geom_bar(stat = "identity") +
  ggtitle("Bar Chart of Variable1 and Variable2 Combinations") +
  xlab("Variable1 and Variable2 Combinations") +
  ylab("Frequency") +
  scale_fill_manual(values = c("0" = "red", "1" = "green"), name = "Variable1") +
  theme_minimal()

contingency_table <- table(data$Variable1, data$Variable2)

mosaic(contingency_table, shade = TRUE, legend = TRUE, main = "Mosaic Plot of Variable1 and Variable2")