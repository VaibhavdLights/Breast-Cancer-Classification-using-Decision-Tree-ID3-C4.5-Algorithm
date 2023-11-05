# Define a function to train a decision tree
train_decision_tree <- function(X, y, max_depth = NULL) {
  n_samples <- nrow(X)
  n_features <- ncol(X)
  n_labels <- length(unique(y))
  
  if (is.null(max_depth) || max_depth > 0) {
    if (n_labels == 1) {
      return(unique(y))
    }
    if (n_samples < 2) {
      return(sample(y, 1))
    }
    
    best_gini <- 1.0
    best_split <- NULL
    left_mask <- NULL
    right_mask <- NULL
    
    for (feature in 1:n_features) {
      thresholds <- unique(X[, feature])
      for (threshold in thresholds) {
        left_index <- X[, feature] < threshold
        right_index <- !left_index
        gini <- (sum(y[left_index] == 0) / sum(left_index))^2 +
          (sum(y[left_index] == 1) / sum(left_index))^2 +
          (sum(y[right_index] == 0) / sum(right_index))^2 +
          (sum(y[right_index] == 1) / sum(right_index))^2
        if (gini < best_gini) {
          best_gini <- gini
          best_split <- c(feature, threshold)
          left_mask <- left_index
          right_mask <- right_index
        }
      }
    }
    
    if (best_gini < 1.0) {
      left_tree <- train_decision_tree(X[left_mask, ], y[left_mask], max_depth - 1)
      right_tree <- train_decision_tree(X[right_mask, ], y[right_mask], max_depth - 1)
      return(list(feature = best_split[1], threshold = best_split[2], left = left_tree, right = right_tree))
    }
  }
  
  return(sample(y, 1))
}

# Define a function to predict using the decision tree
predict_decision_tree <- function(x, tree) {
  if (!is.list(tree)) {
    return(tree)
  }
  if (x[tree$feature] < tree$threshold) {
    return(predict_decision_tree(x, tree$left))
  } else {
    return(predict_decision_tree(x, tree$right))
  }
}

# Example usage:
X <- matrix(c(2.3, 1.7, 3.5, 2.5, 5.1, 4.2, 7.1, 3.8, 8.2, 5.5), ncol = 2)
y <- c(0, 1, 0, 1, 1)

tree <- train_decision_tree(X, y, max_depth = 2)

predictions <- sapply(1:nrow(X), function(i) predict_decision_tree(X[i, ], tree))
print(predictions)
# Save the trained decision tree to a file
saveRDS(tree, file = "decision_tree.rds")
