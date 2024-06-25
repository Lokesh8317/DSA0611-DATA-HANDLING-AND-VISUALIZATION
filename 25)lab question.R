# Load necessary libraries
library(plotly)
library(akima)

# Create the dataset
data <- data.frame(
  Student = c('A', 'B', 'C', 'D', 'E'),
  MathScore = c(85, 72, 90, 78, 88),
  ReadingScore = c(78, 85, 80, 75, 82),
  Attendance = c(95, 92, 98, 85, 93)
)

# View the data
print(data)

# 3D scatter plot: Math Score vs Reading Score vs Attendance
plot_ly(data, x = ~MathScore, y = ~ReadingScore, z = ~Attendance, type = 'scatter3d', mode = 'markers',
        marker = list(size = 5, color = ~ReadingScore, colorscale = 'Viridis')) %>%
  layout(title = '3D Scatter Plot: Math Score vs Reading Score vs Attendance',
         scene = list(xaxis = list(title = 'Math Score'),
                      yaxis = list(title = 'Reading Score'),
                      zaxis = list(title = 'Attendance (%)')))

# Calculate correlations
correlations <- cor(data[, c('MathScore', 'ReadingScore', 'Attendance')])
print(correlations)

# Interpolating the data for surface plot
interp_data <- interp(data$MathScore, data$Attendance, data$ReadingScore)

# 3D surface plot
plot_ly(x = interp_data$x, y = interp_data$y, z = interp_data$z, type = 'surface') %>%
  layout(title = '3D Surface Plot: Reading Score with varying Math Score and Attendance',
         scene = list(xaxis = list(title = 'Math Score'),
                      yaxis = list(title = 'Attendance (%)'),
                      zaxis = list(title = 'Reading Score')))

# 3D scatter plot: Math Score vs Reading Score vs Attendance
plot_ly(data, x = ~MathScore, y = ~ReadingScore, z = ~Attendance, type = 'scatter3d', mode = 'markers',
        marker = list(size = 5, color = ~ReadingScore, colorscale = 'Viridis')) %>%
  layout(title = '3D Scatter Plot: Math Score vs Reading Score vs Attendance',
         scene = list(xaxis = list(title = 'Math Score'),
                      yaxis = list(title = 'Reading Score'),
                      zaxis = list(title = 'Attendance (%)')))

# 3D scatter plot: Reading Score vs Attendance vs Math Score
plot_ly(data, x = ~Attendance, y = ~ReadingScore, z = ~MathScore, type = 'scatter3d', mode = 'markers',
        marker = list(size = 5, color = ~ReadingScore, colorscale = 'Viridis')) %>%
  layout(title = '3D Scatter Plot: Attendance vs Reading Score vs Math Score',
         scene = list(xaxis = list(title = 'Attendance (%)'),
                      yaxis = list(title = 'Reading Score'),
                      zaxis = list(title = 'Math Score')))