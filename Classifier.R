
library(C50)

# Step 2: Load the breast cancer dataset from a CSV file
breast_cancer_data <- read.csv("BreastCancer.csv")

# Step 3: Check the structure of the dataset
str(breast_cancer_data)

# Step 4: Ensure that the "Class" column is a factor
breast_cancer_data$Class <- as.factor(breast_cancer_data$Class)

# Step 5: Split the dataset into training and testing sets
set.seed(123)
train_indices <- sample(1:nrow(breast_cancer_data), 0.7 * nrow(breast_cancer_data))  # 70% for training
train_data <- breast_cancer_data[train_indices, ]
test_data <- breast_cancer_data[-train_indices, ]

# Step 6: Build the C5.0 decision tree model
c5.0_model <- C5.0(Class ~ ., data = train_data)



# Step 7: Summarize the model
summary(c5.0_model)

# Step 8: Predict on the test data
predictions <- predict(c5.0_model, newdata = test_data)

# Step 9: Evaluate the model's performance
accuracy <- sum(predictions == test_data$Class) / nrow(test_data)
cat("Accuracy:", accuracy, "\n")
