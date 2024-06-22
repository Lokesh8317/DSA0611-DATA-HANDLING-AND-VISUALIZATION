# Create the data frame
data <- data.frame(
  ID = 1:5,
  X1 = c(5, 7, 6, 8, 4),
  X2 = c(10, 12, 11, 9, 13),
  X3 = c(15, 14, 13, 16, 17)
)

# Pairwise scatter plot matrix
pairs(data[, -1], main = "Pairwise Scatter Plot Matrix")
# Load necessary library
library(plotly)

# 3D Scatter plot
plot_ly(data, x = ~X1, y = ~X2, z = ~X3, color = ~ID, type = "scatter3d", mode = "markers",
        marker = list(size = 10)) %>%
  layout(scene = list(xaxis = list(title = 'X1'),
                      yaxis = list(title = 'X2'),
                      zaxis = list(title = 'X3')),
         title = "3D Scatter Plot of X1, X2, and X3")
# Load necessary library
library(MASS)  # For parcoord function

# Parallel coordinates plot
parcoord(data[, -1], col = rainbow(nrow(data)), lty = 1)
# Radar chart for ID 1
library(fmsb)  # For radar plotting function

# Select data for ID 1
id1_data <- data[data$ID == 1, -1]

# Convert to matrix format for radar plot
id1_matrix <- as.matrix(id1_data)

# Radar plot
radarchart(data, axistype = 1, seg = 4,
           pcol = rainbow(ncol(id1_matrix), alpha = 0.6),
           plwd = 2, cglcol = "grey", cglty = 1,
           title = "Radar Chart for ID 1")

# Calculate correlations
correlation_matrix <- cor(data[, -1])

# Heatmap of correlations
heatmap(correlation_matrix, 
        Colv = NA, Rowv = NA,
        col = colorRampPalette(c("blue", "white", "red"))(100),
        main = "Heatmap of Correlations Between X1, X2, X3, and X4")