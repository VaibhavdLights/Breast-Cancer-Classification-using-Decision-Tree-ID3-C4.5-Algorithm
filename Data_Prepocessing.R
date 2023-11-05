

# Load a sample dataset (use your own dataset or replace with one from your source)
data <- read.csv("BreastCancer.csv")
dataset <- data

# Data Cleaning
# Check for missing values
missing_values <- colSums(is.na(dataset))
print("Missing Values:")
print(missing_values)

# Handle missing values (e.g., impute with mean or remove rows/columns)
dataset <- na.omit(dataset)  # Removes rows with missing values

# Outlier Detection and Handling
# Define a function to detect and handle outliers (e.g., remove them)
handle_outliers <- function(data, column_name, z_threshold = 2) {
  z_scores <- scale(data[[column_name]])
  outliers <- abs(z_scores) > z_threshold
  return(data[!outliers, ])
}





# Data Splitting (Training and Test Sets)
set.seed(123)  # for reproducibility
split_ratio <- 2/3  # 70% training, 30% test
num_rows <- nrow(dataset)
train_indices <- sample(1:num_rows, split_ratio * num_rows)
train_data <- dataset[train_indices, ]
test_data <- dataset[-train_indices, ]

# Print summary of data
print("Training Data Summary:")
print(summary(train_data))
print("Test Data Summary:")
print(summary(test_data))


# ... (Previous code for data pre-processing)

# Store Cleaned Data in a New CSV File
write.csv(dataset, "BreastCancerCleaned.csv", row.names = FALSE)

# Check if the file was successfully created
if ("BreastCancerCleaned.csv" %in% list.files()) {
  cat("Cleaned data has been saved as 'BreastCancerCleaned.csv' in the working directory.\n")
} else {
  cat("Error: Could not save the cleaned data to 'BreastCancerCleaned.csv'.\n")
}
