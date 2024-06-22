library(ggplot2)
library(ggmosaic)

data <- data.frame(
  ID = 1:5,
  Gender = c("Male", "Female", "Male", "Female", "Male"),
  Education = c("Graduate", "Undergrad", "High School", "Graduate", "Undergrad"),
  Occupation = c("Engineer", "Teacher", "Doctor", "Lawyer", "Artist")
)

gender_count <- ggplot(data, aes(x = Gender)) +
  geom_bar() +
  ggtitle("Count of Each Gender") +
  xlab("Gender") +
  ylab("Count")

print(gender_count)

education_count <- data.frame(table(data$Education))
education_count$Percentage <- education_count$Freq / sum(education_count$Freq) * 100
education_count$Label <- paste0(round(education_count$Percentage, 1), "%")

education_pie <- ggplot(education_count, aes(x = "", y = Freq, fill = Var1)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  ggtitle("Distribution of Education Levels") +
  theme(axis.text.x = element_blank()) +
  xlab("") +
  ylab("") +
  geom_text(aes(y = cumsum(Freq) - Freq / 2, label = Label), color = "white")

print(education_pie)

occupation_gender_count <- ggplot(data, aes(x = Occupation, fill = Gender)) +
  geom_bar(position = "stack") +
  ggtitle("Count of Each Occupation by Gender") +
  xlab("Occupation") +
  ylab("Count")

print(occupation_gender_count)

mosaic_plot <- ggplot(data) +
  geom_mosaic(aes(x = product(Education), fill = Occupation)) +
  ggtitle("Association Between Education and Occupation") +
  xlab("Education") +
  ylab("Occupation")

print(mosaic_plot)

gender_education_count <- ggplot(data, aes(x = Education, fill = Gender)) +
  geom_bar(position = "dodge") +
  ggtitle("Counts of Gender Across Different Education Levels") +
  xlab("Education") +
  ylab("Count")

print(gender_education_count)