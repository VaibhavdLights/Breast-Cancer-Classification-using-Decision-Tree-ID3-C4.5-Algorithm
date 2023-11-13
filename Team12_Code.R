#Team 12 - IDA Project
#Breast Cancer Classification using decision tree ID3 C4.5 algorithm and K-fold cross validation

#Load required libraries
library(C50)
library(caret)
library(e1071)
library(pROC)

#Please ensure the dataset is saved as 'BreastCancer.csv' and is in the same directory as the code
#Please ensure both the data  and code are in the current working directory
# Load given dataset from topic 12 PS into the code
cancer_dataset <- read.csv("BreastCancer.csv")

#PS (a) Data Preprocessing

# Check for missing values
missing_values <- colSums(is.na(cancer_dataset))
print("Missing Values:")
print(missing_values)

# Handle missing values 

#Possible ways include replacing with mean or other central tendencies, here
#we drop the rows contaning missing values
cancer_dataset <- na.omit(cancer_dataset)  # Removes rows with missing values

# Outlier Detection and Handling

# Define a function to detect and handle outliers (e.g., remove them)
outliers_removal <- function(data, name_of_col, z_threshold = 2) {
  
  # Step 1: Calculate Z-scores for the specified column (normalize it)
  z_scores <- scale(data[[name_of_col]])
  # Step 2: Identify indices of outliers based on the Z-score and the specified threshold
  outliers <- abs(z_scores) > z_threshold
  # Step 3: Remove rows containing outliers from the original data frame
  return(data[!outliers, ])
}

# Ensure that the "Class" column is a factor
cancer_dataset$Class <- as.factor(cancer_dataset$Class)

#PS (b): Build a classifier model based on ID3/C4.5 algorithm. You should divide the data set
#randomly in 2:1 ratio using any random sampling method and then learn the model
#using the training data set.

# Data Splitting (Training and Test Sets)
set.seed(123)  # for reproducibility
split_ratio <- 2/3  # Define split ratio for training and test set
num_rows <- nrow(cancer_dataset) # Calculate the total number of rows in the dataset
train_indices <- sample(1:num_rows, split_ratio * num_rows) # Randomly sample row indices for the training set based on the split ratio
train_data <- cancer_dataset[train_indices, ] # Create the training set by selecting rows using the sampled indices
test_data <- cancer_dataset[-train_indices, ] # Create the test set by using the leftover rows used

# Print summary of data
print("Training Data Summary:")
print(summary(train_data))
print("Test Data Summary:")
print(summary(test_data))

# Store Cleaned Data in a New CSV File for easy access or further process
write.csv(cancer_dataset, "BreastCancerCleaned.csv", row.names = FALSE)

# Check if the file was successfully created
if ("BreastCancerCleaned.csv" %in% list.files()) {
  cat("Cleaned data has been saved as 'BreastCancerCleaned.csv' in the working directory.\n")
} else {
  cat("Error: Could not save the cleaned data to 'BreastCancerCleaned.csv'.\n")
}

#Model Creation

#Build the C5.0 decision tree model - uses ID3 C4.5 algorithm
c5.0_model <- C5.0(Class ~ ., data = train_data)

#plotting the model
plot(c5.0_model)

#Summarizing the model we have created
summary(c5.0_model)

#Predictions based on model

#Running the model on the test data and making predictions
predictions <- predict(c5.0_model, newdata = test_data)

#PS (c): To Verify the classifier’s performance on the test set. 
#Report the performance measure in terms of Confusion matrix, Predictive accuracy,
#F1-score, Precision and Recall.

#Evaluating model performance 

#We will evaluate the model's performance using its accuracy
accuracy <- sum(predictions == test_data$Class) / nrow(test_data)
cat("Accuracy:", accuracy, "\n")

#Creating a confusion matrix
conf_mat <- confusionMatrix(predictions, test_data$Class)

#Way I: Visualize the confusion matrix using four fold plot
fourfoldplot(as.table(conf_mat$table),color=c("deeppink","darkturquoise"),main = "Confusion Matrix")

#Way II: Visualize the confusion matrix using traditional 4 rectangle plot 
plot_conf_mat_using_rect <- function(confusion_matrix) {
  
  par(mar=c(2,2,2,2))
  plot(c(100, 345), c(300, 450), type = "n", xlab="", ylab="", xaxt='n', yaxt='n')
  
  # create the matrix 
  rect(150, 430, 240, 370, col='darkturquoise')
  text(195, 435, 'Class 0', cex=1.2)
  rect(250, 430, 340, 370, col='deeppink')
  text(295, 435, 'Class 1', cex=1.2)
  text(125, 370, 'Predicted', cex=1.3, srt=90, font=2)
  text(245, 450, 'Target', cex=1.3, font=2)
  rect(150, 305, 240, 365, col='deeppink')
  rect(250, 305, 340, 365, col='darkturquoise')
  text(140, 400, 'Class 0', cex=1.2, srt=90)
  text(140, 335, 'Class 1', cex=1.2, srt=90)
  
  # add in the confusion_matrix results 
  res <- as.numeric(confusion_matrix$table)
  text(195, 400, res[1], cex=1.6, font=2, col='azure')
  text(195, 335, res[2], cex=1.6, font=2, col='azure')
  text(295, 400, res[3], cex=1.6, font=2, col='azure')
  text(295, 335, res[4], cex=1.6, font=2, col='azure')
}

#calling custom function to visualize confusion matrix
plot_conf_mat_using_rect(conf_mat)

# Extract the relevant performance measures
accuracy <- conf_mat$overall["Accuracy"]
f1_score <- conf_mat$byClass["F1"]
precision <- conf_mat$byClass["Pos Pred Value"]
recall <- conf_mat$byClass["Sensitivity"]

#Display the key performance measures
cat("Confusion Matrix:\n")
print(conf_mat$table)

cat("\nPredictive Accuracy:", accuracy, "\n")
cat("F1-Score:", f1_score, "\n")
cat("Precision:", precision, "\n")
cat("Recall (Sensitivity):", recall, "\n")

#PS (d) Use k-fold cross validation with different values of k. Obtain an ROC curve with
#different values of k.

#K fold cross validation:

#creating an array of different K values
k_values <- c(3, 4, 7, 10, 15, 25)

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
lines(roc_data[["10"]], col = "deeppink")
lines(roc_data[["15"]], col = "cyan")
lines(roc_data[["25"]], col = "darkorchid")
legend("bottomright", legend = c("k = 3", "k = 4", "k = 7", "k = 10", "k = 15", "k = 25"), col = c("red", "blue", "green","deeppink","cyan", "darkorchid"), lty = 1)

