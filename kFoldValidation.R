library(C50)
library(caret)
library(e1071)
library(pROC)

# Load the breast cancer dataset from a CSV file
breast_cancer_data <- read.csv("BreastCancerCleaned.csv")

# Ensure that the "Class" column is a factor
breast_cancer_data$Class <- as.factor(breast_cancer_data$Class)

# Define the target variable (Class) and the features
target_variable <- "Class"
features <- setdiff(names(breast_cancer_data), target_variable)

# Split the dataset into training and testing sets
set.seed(123)
train_indices <- sample(1:nrow(breast_cancer_data), 0.7 * nrow(breast_cancer_data))  # 70% for training
train_data <- breast_cancer_data[train_indices, ]
test_data <- breast_cancer_data[-train_indices, ]

#Diiferent K values
k_values <- c(3, 4, 7)

# Create an empty list to store ROC curve data
roc_data <- list()

# Loop through different k values for cross-validation
for (k in k_values) {
  # Set up k-fold cross-validation using the "trainControl" function
  control <- trainControl(method = "cv", number = k)
  
  # Build the C5.0 decision tree model with cross-validation
  c5.0_model <- train(Class ~ ., data = train_data, method = "C5.0", trControl = control)
  
  # Predict on the test data
  predictions <- predict(c5.0_model, newdata = test_data)
  
  # Create a confusion matrix
  confusion_matrix <- confusionMatrix(predictions, test_data$Class)
  
  # Extract ROC curve data
  roc <- roc(test_data$Class, as.numeric(predictions))
  
  # Store ROC curve data in the list
  roc_data[[as.character(k)]] <- roc
}

# Plot ROC curves for different k values
par(mfrow = c(1, 1))  # Reset the plotting layout
plot(roc_data[["3"]], col = "red", main = "ROC Curves for Different Values of k", xlab = "False Positive Rate", ylab = "True Positive Rate")
lines(roc_data[["4"]], col = "blue")
lines(roc_data[["7"]], col = "green")
legend("bottomright", legend = c("k = 3", "k = 4", "k = 7"), col = c("red", "blue", "green"), lty = 1)
