# Load necessary libraries
library(dplyr)
library(readr)
library(writexl)
library(tidyr)
library(randomForest)
library(ggplot2)
library(rpart)
library(rpart.plot)

# Load the dataset
data <- read.csv("master_data_with_gdp_india.csv")
data <- data[, -c(1, 2)]

head(data)
# Split the data into training and testing sets
set.seed(123)  # For reproducibility
train_indices <- sample(seq_len(nrow(data)), size = 0.8 * nrow(data))
train_data <- data[train_indices, ]
test_data <- data[-train_indices, ]

# Define a range of mtry values to test
mtry_values <- seq(1, ncol(train_data) - 1, by = 1)

# Store the OOB error rates for each mtry value
oob_errors <- data.frame(
  mtry = integer(),
  OOBError = numeric()
)

# Loop over different mtry values
for (mtry in mtry_values) {
  # Train the Random Forest model
  set.seed(123)
  rf_model <- randomForest(GDP ~ ., data = train_data, ntree = 500, mtry = mtry, importance = TRUE)
  
  # Store the mtry value and corresponding OOB error
  oob_errors <- rbind(oob_errors, data.frame(mtry = mtry, OOBError = tail(rf_model$mse, 1)))
}

# Plot the OOB error rate for different mtry values
oob_error_plot <- ggplot(oob_errors, aes(x = mtry, y = OOBError)) +
  geom_line(color = "blue") +
  geom_point(color = "red") +
  labs(title = "OOB Error Rate vs. Number of Variables Tried at Each Split (mtry)",
       x = "Number of Variables (mtry)",
       y = "OOB Error Rate (MSE)") +
  theme_minimal()

# Display the plot
print(oob_error_plot)

# Print all the OOB values
print(oob_errors)

# Identify the mtry value with the lowest OOB error rate
best_mtry <- oob_errors$mtry[which.min(oob_errors$OOBError)]
cat("Best mtry value:", best_mtry, "\n")

# Train the Random Forest model with the best mtry value
set.seed(123)
best_rf_model <- randomForest(GDP ~ ., data = train_data, ntree = 500, mtry = best_mtry, importance = TRUE)

# Make predictions on the test data
test_predictions <- predict(best_rf_model, test_data)

# Create a data frame with actual and predicted values
results <- data.frame(
  Actual = test_data$GDP,
  Predicted = test_predictions
)

# Plot the actual values vs. predicted values
actual_vs_predicted_plot <- ggplot(results, aes(x = Actual, y = Predicted)) +
  geom_point(alpha = 0.6) +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Actual vs. Predicted Values",
       x = "Actual GDP",
       y = "Predicted GDP") +
  theme_minimal()

# Display the plot
print(actual_vs_predicted_plot)

# Train a single decision tree using rpart
decision_tree <- rpart(GDP ~ ., data = train_data)

# Plot the decision tree
rpart.plot(decision_tree, main = "Decision Tree for GDP Prediction", type = 3, extra = 1)
