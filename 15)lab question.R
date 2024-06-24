# Install required packages
install.packages(c("ggplot2", "dendextend", "treemap", "plotly", "ggraph", "igraph"))

# Load the libraries
library(ggplot2)
library(dendextend)
library(treemap)
library(plotly)
library(ggraph)
library(igraph)

# Data
data <- data.frame(
  Parent = c("A", "A", "B", "B", "C"),
  Subcategory = c("A1", "A2", "B1", "B2", "C1"),
  Value = c(10, 15, 20, 25, 12)
)

# 1. Dendrogram
dist_matrix <- dist(data$Value)
hc <- hclust(dist_matrix)
dend <- as.dendrogram(hc)
plot(dend, main = "Dendrogram of Hierarchical Data")

# 2. Treemap
treemap(data,
        index = c("Parent", "Subcategory"),
        vSize = "Value",
        title = "Treemap of Hierarchical Data",
        palette = "Set3",
        border.col = "white")

# 3. Sunburst Chart
sunburst_data <- data.frame(
  labels = c("A", "A1", "A2", "B", "B1", "B2", "C", "C1"),
  parents = c("", "A", "A", "", "B", "B", "", "C"),
  values = c(NA, 10, 15, NA, 20, 25, NA, 12)
)

fig <- plot_ly(
  sunburst_data,
  labels = ~labels,
  parents = ~parents,
  values = ~values,
  type = 'sunburst',
  branchvalues = 'total'
)

fig <- fig %>% layout(title = "Sunburst Chart of Hierarchical Data")
fig

# 4. Nested Bar Plot
ggplot(data, aes(x = Parent, y = Value, fill = Subcategory)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Nested Bar Plot of Hierarchical Data",
       x = "Parent Category",
       y = "Value") +
  theme_minimal()

# 5. Hierarchical Edge Bundling Plot
edges <- data.frame(from = data$Parent, to = data$Subcategory)
graph <- graph_from_data_frame(edges)

ggraph(graph, layout = 'dendrogram', circular = TRUE) + 
  geom_edge_link(aes(alpha = ..index..), show.legend = FALSE) + 
  geom_node_point() + 
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_void() +
  ggtitle("Hierarchical Edge Bundling Plot")

