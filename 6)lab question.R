data <- data.frame(
  ID = c(1, 2, 3, 4, 5),
  Age = c(25, 30, 28, 35, 40),
  Height = c(175, 180, 170, 165, 185),
  Weight = c(70, 80, 65, 75, 90)
)

plot(data$Age, data$Weight, 
     xlab = "Age", 
     ylab = "Weight", 
     main = "Scatter Plot of Age vs Weight",
     pch = 16, col = "blue")

hist(data$Height, 
     xlab = "Height", 
     main = "Histogram of Height",
     col = "green", 
     breaks = 5)

plot(data$ID, data$Weight, 
     type = "o", 
     xlab = "ID", 
     ylab = "Weight", 
     main = "Line Chart of Weight over IDs", 
     col = "red")

boxplot(data$Age, 
        main = "Box Plot of Age", 
        ylab = "Age", 
        col = "purple")

height_density <- density(data$Height)
plot(height_density, 
     main = "Density Plot of Height", 
     xlab = "Height", 
     ylab = "Density", 
     col = "orange")
polygon(height_density, col = "orange", border="black")