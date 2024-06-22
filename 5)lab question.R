library(ggplot2)
library(ggmosaic)

data <- data.frame(
  Year = factor(c(2019, 2020, 2022, 2023, 2024)),
  Job_Sector = c('IT', 'Government Job', 'Customer care', 'Bank', 'Games'),
  Job_Seekers_Rate = c(95, 97, 98, 82, 74),
  Selection_Rate = c(25, 12, 45, 20, 35)
)

pie_plot <- ggplot(data, aes(x = "", y = Job_Seekers_Rate, fill = Job_Sector)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  geom_text(aes(label = paste0(Job_Seekers_Rate, "%")), position = position_stack(vjust = 0.5), color = "white") +
  labs(title = "Pie Plot of Job Seekers Rate by Job Sector", x = NULL, y = "Job Seekers Rate", fill = "Job Sector") +
  theme_minimal()

print(pie_plot)

mosaic_plot <- ggplot(data, aes(x = factor(Year), y = Job_Seekers_Rate, fill = Job_Sector)) +
  geom_tile(stat = "identity") +
  theme_minimal() +
  labs(title = "Mosaic Plot of Job Seekers' Rate by Year and Job Sector",
       x = "Year", y = "Job Seekers Rate")

print(mosaic_plot)

data <- data[order(data$Selection_Rate, decreasing = TRUE), ]  # Sort data by Selection Rate

funnel_plot <- ggplot(data, aes(x = reorder(Job_Sector, Selection_Rate), y = Selection_Rate)) +
  geom_bar(stat = "identity", fill = "skyblue", width = 0.7) +
  coord_flip() +
  labs(title = "Funnel Plot of Selection Rate by Job Sector", x = "Job Sector", y = "Selection Rate") +
  theme_minimal()

print(funnel_plot)