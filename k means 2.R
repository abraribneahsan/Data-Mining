# Assuming 'dataset' is your loaded data frame
library(dplyr)

file_path <- "D:/12th sem/DATA/Auto Sales data.csv"
dataset <- read.csv(file_path)
str(dataset)
head(dataset)
# Select relevant columns for clustering
features <- dataset[, c("QUANTITYORDERED", "PRICEEACH", "MSRP", "DAYS_SINCE_LASTORDER")]

# Standardize numerical features
scaled_features <- scale(features)

# Set number of clusters (k)
k <- 3

# Initialize random centroids
set.seed(123)  # For reproducibility
centroids <- scaled_features[sample(1:nrow(scaled_features), k), ]

# Function to calculate Euclidean distance
euclidean_dist <- function(x1, x2) {
  sqrt(sum((x1 - x2)^2))
}

# K-means clustering
max_iter <- 100
for (iter in 1:max_iter) {
  # Assign each data point to nearest centroid
  cluster_labels <- apply(scaled_features, 1, function(x) which.min(apply(centroids, 1, function(c) euclidean_dist(x, c))))
  
  # Update centroids
  new_centroids <- t(sapply(1:k, function(i) colMeans(scaled_features[cluster_labels == i, ])))
  
  # Check convergence
  if (all(round(centroids, 6) == round(new_centroids, 6))) {
    break
  }
  
  centroids <- new_centroids
}

# Add cluster labels to dataset
dataset$cluster <- cluster_labels

# Calculate predictive accuracy
accuracy <- sum(dataset$DEALSIZE == "Small" & dataset$cluster == 1 |
                  dataset$DEALSIZE == "Medium" & dataset$cluster == 2 |
                  dataset$DEALSIZE == "Large" & dataset$cluster == 3) / nrow(dataset)

cat("Predictive Accuracy:", accuracy * 100, "%\n")

