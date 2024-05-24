library(dplyr)

# Function to calculate probabilities
calculate_probabilities <- function(data, target_column, feature_column, feature_value) {
  # P(feature | target)
  numerator <- sum(data[[feature_column]] == feature_value & data[[target_column]] == 1)
  denominator <- sum(data[[target_column]] == 1)
  probability <- (numerator + 1) / (denominator + length(unique(data[[feature_column]])))
  return(probability)
}

# Function to train Naive Bayes model
train_naive_bayes <- function(data, target_column) {
  model <- list()
  model$prior_probability <- sum(data[[target_column]] == 1) / nrow(data)
  
  # Store conditional probabilities for each feature
  model$probabilities <- list()
  for (col in setdiff(names(data), target_column)) {
    probabilities <- list()
    unique_values <- unique(data[[col]])
    for (value in unique_values) {
      probabilities[[as.character(value)]] <- calculate_probabilities(data, target_column, col, value)
    }
    model$probabilities[[col]] <- probabilities
  }
  
  return(model)
}

# Function to make predictions using Naive Bayes model
predict_naive_bayes <- function(model, new_instance) {
  # Calculate P(target = 1)
  prior_probability <- model$prior_probability
  
  # Initialize likelihood
  likelihood <- prior_probability
  
  # Multiply probabilities for each feature
  for (col in names(new_instance)) {
    feature_value <- as.character(new_instance[[col]])
    # Ensure the feature value is present in the model
    if (feature_value %in% names(model$probabilities[[col]])) {
      likelihood <- likelihood * as.numeric(model$probabilities[[col]][feature_value])
    }
  }
  
  # Return the predicted class
  if (likelihood >= 0.5) {
    return(1)
  } else {
    return(0)
  }
}

# Your original code starts here

file_path <- 
  
  "D:/12th sem/DATA/Auto Sales data.csv"
dataset <- read.csv(file_path)

categorical_columns <- sapply(dataset, is.character)

for (col in names(dataset)[categorical_columns]) {
  dataset[[col]] <- as.numeric(factor(dataset[[col]], levels = unique(dataset[[col]])))
}

dataset <- dataset[, !(names(dataset) %in% c("PRODUCTLINE"))]

correlation_matrix <- cor(dataset[, sapply(dataset, is.numeric)])

significant_attributes <- names(which(abs(correlation_matrix[, "DEALSIZE"]) > 0.5))

dataset <- dataset[, c("DEALSIZE", significant_attributes)]

set.seed(123)
split_ratio <- 0.7
train_indices <- sample(1:nrow(dataset), split_ratio * nrow(dataset))

train_set <- dataset[train_indices, ]
test_set <- dataset[-train_indices, ]

# Train Naive Bayes model
naive_bayes_model <- train_naive_bayes(train_set, "DEALSIZE")

# Predict on the test set
predictions <- sapply(1:nrow(test_set), function(i) predict_naive_bayes(naive_bayes_model, test_set[i, ]))

accuracy <- sum(predictions == test_set$DEALSIZE) / nrow(test_set)
cat("Predictive Accuracy:", accuracy, "\n")



recall <- conf_matrix[2, 2] / sum(conf_matrix[2, ])
precision <- conf_matrix[2, 2] / sum(conf_matrix[, 2])
f_measure <- (2 * precision * recall) / (precision + recall)


cat("Confusion Matrix:\n", conf_matrix, "\n")
cat("Recall:", recall, "\n")
cat("Precision:", precision, "\n")
cat("F-measure:", f_measure, "\n")

