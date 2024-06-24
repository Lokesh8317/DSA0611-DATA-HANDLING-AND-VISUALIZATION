# Load necessary libraries
install.packages("plotly")
library(plotly)

# Create the data frame
data <- data.frame(
  Student = c("A", "B", "C", "D", "E"),
  Math_Score = c(85, 72, 90, 78, 88),
  Science_Score = c(78, 85, 80, 75, 82),
  Attendance = c(95, 92, 98, 85, 93)
)

# 1. View the data
print(data)

# 2. Create a 3D scatter plot
plot_ly(data, x = ~Math_Score, y = ~Attendance, z = ~Science_Score, 
        type = 'scatter3d', mode = 'markers', 
        marker = list(size = 5)) %>%
  layout(scene = list(xaxis = list(title = 'Math Score'),
                      yaxis = list(title = 'Attendance (%)'),
                      zaxis = list(title = 'Science Score')))

# 3. Correlation between attendance, math scores, and science scores
correlation_matrix <- cor(data[,2:4])
print(correlation_matrix)

# 4. Generate a 3D surface plot
# For the surface plot, we'll create a grid of values for Math_Score and Attendance
math_grid <- seq(min(data$Math_Score), max(data$Math_Score), length.out = 100)
attendance_grid <- seq(min(data$Attendance), max(data$Attendance), length.out = 100)

# Create a function to interpolate science scores
fit <- lm(Science_Score ~ Math_Score + Attendance, data = data)
science_grid <- outer(math_grid, attendance_grid, 
                      function(x, y) predict(fit, data.frame(Math_Score = x, Attendance = y)))

# Plot the surface
plot_ly(x = ~math_grid, y = ~attendance_grid, z = ~science_grid, type = 'surface') %>%
  layout(scene = list(xaxis = list(title = 'Math Score'),
                      yaxis = list(title = 'Attendance (%)'),
                      zaxis = list(title = 'Science Score')))

# 5. Compare the 3D plots
# Plot science scores against math scores
plot_ly(data, x = ~Math_Score, y = ~Science_Score, z = ~Attendance, 
        type = 'scatter3d', mode = 'markers', 
        marker = list(size = 5, color = ~Science_Score)) %>%
  layout(scene = list(xaxis = list(title = 'Math Score'),
                      yaxis = list(title = 'Science Score'),
                      zaxis = list(title = 'Attendance (%)')))

# Plot science scores against attendance
plot_ly(data, x = ~Attendance, y = ~Science_Score, z = ~Math_Score, 
        type = 'scatter3d', mode = 'markers', 
        marker = list(size = 5, color = ~Science_Score)) %>%
  layout(scene = list(xaxis = list(title = 'Attendance (%)'),
                      yaxis = list(title = 'Science Score'),
                      zaxis = list(title = 'Math Score')))
