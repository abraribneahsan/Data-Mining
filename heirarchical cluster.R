# Assuming 'dataset' is your loaded data frame
# Assuming 'dataset' is your loaded data frame
library(dplyr)

file_path <- "D:/12th sem/DATA/Auto Sales data.csv"
dataset <- read.csv(file_path)
str(dataset)
head(dataset)

features <- dataset[, c("QUANTITYORDERED", "PRICEEACH", "MSRP", "DAYS_SINCE_LASTORDER")]

# Standardize numerical features
scaled_features <- scale(features)

# Perform hierarchical clustering
dist_matrix <- dist(scaled_features, method = "euclidean")
hclust_result <- hclust(dist_matrix, method = "ward.D")

# Cut the dendrogram to get clusters
num_clusters <- 3  # Choose the number of clusters
clusters <- cutree(hclust_result, k = num_clusters)

# Assign cluster labels to dataset
dataset$cluster <- clusters

# Function to calculate predictive accuracy
calculate_accuracy <- function(actual, predicted) {
  sum(actual == predicted) / length(actual)
}

# Calculate most frequent DEALSIZE within each cluster
cluster_to_dealsize <- function(cluster_labels, dealsize) {
  unique_clusters <- unique(cluster_labels)
  cluster_mapping <- character(length(unique_clusters))
  
  for (i in seq_along(unique_clusters)) {
    cluster_members <- dealsize[cluster_labels == unique_clusters[i]]
    cluster_mapping[i] <- names(table(cluster_members))[which.max(table(cluster_members))]
  }
  
  return(cluster_mapping[match(cluster_labels, unique_clusters)])
}

# Map cluster labels to DEALSIZE
predicted_dealsize <- cluster_to_dealsize(clusters, dataset$DEALSIZE)

# Calculate predictive accuracy
accuracy <- calculate_accuracy(dataset$DEALSIZE, predicted_dealsize)

cat("Predictive Accuracy:", accuracy * 100, "%\n")


