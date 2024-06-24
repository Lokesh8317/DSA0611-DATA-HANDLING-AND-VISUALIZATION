# Install and load required packages if not already installed
if (!requireNamespace("scatterplot3d", quietly = TRUE)) {
  install.packages("scatterplot3d")
}
if (!requireNamespace("plot3D", quietly = TRUE)) {
  install.packages("plot3D")
}
library(scatterplot3d)
library(plot3D)

# Data
date <- as.Date(c("2023-01-01", "2023-01-02", "2023-01-03", "2023-01-04", "2023-01-05"))
stock_price <- c(100, 102, 98, 105, 108)
volume_traded <- c(2.5, 3.0, 2.2, 2.8, 3.5)
market_cap <- c(500, 510, 490, 525, 540)

# 1. Relationship between stock price, volume traded, and market capitalization
# Stock price generally responds to changes in both volume traded and market cap.
# Higher trading volumes can indicate increased investor interest, potentially affecting stock price.
# Market capitalization is directly related to stock price through the number of shares outstanding.

# 2. Create 3D Scatter Plot
scatterplot3d(volume_traded, market_cap, stock_price, 
              main = "Relationship between Volume Traded, Market Cap, and Stock Price",
              xlab = "Volume Traded (millions)",
              ylab = "Market Cap ($)",
              zlab = "Stock Price ($)",
              color = "blue",
              pch = 16)

# 3. Identify Clustering or Outliers (Visual Inspection)
# In the 3D plot, look for any groups of points closely packed together (clustering) or points significantly distant from others (outliers).

# 4. Generate 3D Surface Plot
# Prepare data for surface plot
x <- volume_traded
y <- stock_price
z <- market_cap

# Create a grid of x and y values
xgrid <- seq(min(x), max(x), length.out = 20)
ygrid <- seq(min(y), max(y), length.out = 20)
interp_data <- interp(x, y, z, 
                      xo = xgrid, 
                      yo = ygrid, 
                      duplicate = "mean")

# Extract the z values as a matrix
zgrid <- interp_data$z
zmatrix <- matrix(zgrid, nrow = length(xgrid), ncol = length(ygrid), byrow = TRUE)

# Create surface plot
persp3D(x = xgrid, y = ygrid, z = zmatrix, theta = 30, phi = 30,
        ticktype = "detailed", col = "lightblue",
        xlab = "Volume Traded (millions)",
        ylab = "Stock Price ($)",
        zlab = "Market Cap ($)",
        main = "Market Capitalization Surface Plot")

# 5. Compare 3D Plots
# Plot stock price against volume traded separately
scatterplot3d(volume_traded, stock_price, market_cap, 
              main = "Relationship between Volume Traded and Stock Price",
              xlab = "Volume Traded (millions)",
              ylab = "Stock Price ($)",
              zlab = "Market Cap ($)",
              color = "green",
              pch = 16)

# Plot stock price against market cap separately
scatterplot3d(market_cap, stock_price, volume_traded, 
              main = "Relationship between Market Cap and Stock Price",
              xlab = "Market Cap ($)",
              ylab = "Stock Price ($)",
              zlab = "Volume Traded (millions)",
              color = "red",
              pch = 16)
