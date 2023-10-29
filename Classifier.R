# Install and load required packages
install.packages("rpart")
library(rpart)

# Load your dataset (replace 'your_dataset.csv' with your actual file path)
file_path <- "cleaned_data.csv"
data <- read.csv(file_path)

# Split the data into training and testing sets (2:1 ratio)
set.seed(42)  # Set seed for reproducibility
split_index <- sample(1:nrow(data), 0.67 * nrow(data))
train_data <- data[split_index, ]
test_data <- data[-split_index, ]

# Create and train the decision tree model
tree_model <- rpart(Class ~ ., data = train_data, method = "class")

# Make predictions on the test set
predictions <- predict(tree_model, test_data, type = "class")

# Evaluate the model
accuracy <- sum(predictions == test_data$Class) / nrow(test_data)
print(paste("Accuracy on the test set: ", round(accuracy * 100, 2), "%", sep = ""))

# Display the decision tree
print("Decision Tree:")
print(tree_model)
