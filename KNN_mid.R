library(dplyr)

file_path <- "D:/12th sem/DATA/Auto Sales data.csv"
dataset <- read.csv(file_path)
str(dataset)
head(dataset)
categorical_columns <- sapply(dataset, is.character)


for (col in names(dataset)[categorical_columns]) {
  dataset[[col]] <- as.numeric(factor(dataset[[col]], levels = unique(dataset[[col]])))
}

str(dataset)

dataset <- dataset[, !(names(dataset) %in% c("PRODUCTLINE"))]


correlation_matrix <- cor(dataset[, sapply(dataset, is.numeric)])


significant_attributes <- names(which(abs(correlation_matrix[, "DEALSIZE"]) > 0.5))


dataset <- dataset[, c("DEALSIZE", significant_attributes)]

glimpse(dataset)
str(dataset)

#training and testing
set.seed(123)  
split_ratio <- 0.7
train_indices <- sample(1:nrow(dataset), split_ratio * nrow(dataset))

train_set <- dataset[train_indices, ]
test_set <- dataset[-train_indices, ]

str(train_set)
str(test_set)


#distance measurement

euclidean_distance <- function(point1, point2) {
  sum((point1 - point2) ^ 2) ^ 0.5
}

manhattan_distance <- function(point1, point2) {
  sum(abs(point1 - point2))
}

maxdim_distance <- function(point1, point2) {
  max(abs(point1 - point2))
}

#IMPLEMENTING K-NN ALGORITHM
knn <- function(train_set, test_set, train_labels, k, distance_metric) {
  
  distances <- matrix(0, nrow = nrow(test_set), ncol = nrow(train_set))
  for (i in 1:nrow(test_set)) {
    for (j in 1:nrow(train_set)) {
      if (distance_metric == "euclidean") {
        distances[i, j] <- euclidean_distance(test_set[i, ], train_set[j, ])
      } else if (distance_metric == "manhattan") {
        distances[i, j] <- manhattan_distance(test_set[i, ], train_set[j, ])
      } else if (distance_metric == "maxdim") {
        distances[i, j] <- maxdim_distance(test_set[i, ], train_set[j, ])
      }
    }
  }
  print(distances)
  
  # Find k nearest neighbors for each test data point
  neighbors <- apply(distances, 1, function(x) {
    sort.list(x)[1:k]
  })
  
  # Predict the class labels
  predicted_labels <- apply(neighbors, 1, function(x) {
    tbl <- table(train_labels[x])
    sorted_tbl <- sort(tbl, decreasing = TRUE)
    names(sorted_tbl)[1]
  })
  
  return(predicted_labels)
}

k_values <- c(3,5)
test_subset <- test_set[sample(nrow(test_set),300),]

knn_results <- data.frame(Distance_Metric = character(), K_Value = numeric(), Accuracy = numeric(), Recall = numeric(), stringsAsFactors = FALSE)

distance_metrics <- c("Euclidean", "Manhattan", "Maximum Dimension")

for (metric in distance_metrics) {
  for (k in k_values) {
    predicted_labels <- knn(train_set = as.matrix(train_set[, c("PRICEEACH", "SALES")]),
                            test_set = as.matrix(test_subset[, c("PRICEEACH", "SALES")]),
                            train_labels = train_set$DEALSIZE,
                            k = k,
                            distance_metric = metric)
    
    accuracy <- sum(predicted_labels == test_subset$DEALSIZE) / nrow(test_subset)
    
    # Calculate true positive (TP) and false negative (FN) values for recall
    TP <- sum(predicted_labels == 1 & test_subset$DEALSIZE == 1)
    FN <- sum(predicted_labels == 0 & test_subset$DEALSIZE == 0)
    
    # Calculate recall
    recall <- TP / (TP + FN)
    
    knn_results <- rbind(knn_results, data.frame(Distance_Metric = metric, K_Value = k, Accuracy = accuracy, Recall = recall))
  }  
}

 write.csv(knn_results, file = "results.csv", row.names = TRUE)
  print(knn_results)
  
  
  View(knn_results)
