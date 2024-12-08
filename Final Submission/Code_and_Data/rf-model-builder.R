build_best_rf_model <- function(file_name, country, return_model = FALSE) {
  # Check if the file exists
  if (!file.exists(file_name)) {
    cat("File does not exist:", file_name, "\n")
    return(NULL)
  }
  
  # Load the dataset
  data <- read.csv(file_name)
  
  # Display the first few rows of the dataset
  cat("Processing:", file_name, "\n")
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
  
  # Loop over different mtry values to train the Random Forest model
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
    labs(title = paste("OOB Error Rate vs. Number of Variables Tried at Each Split (mtry) -", country),
         x = "Number of Variables (mtry)",
         y = "OOB Error Rate (MSE)") +
    theme_minimal()
  
  # Display the plot
  print(oob_error_plot)
  
  # Print all the OOB values
  print(oob_errors)
  
  # Identify the mtry value with the lowest OOB error rate
  best_mtry <- oob_errors$mtry[which.min(oob_errors$OOBError)]
  cat("Best mtry value for", country, ":", best_mtry, "\n")
  
  # Train the Random Forest model with the best mtry value
  set.seed(123)
  best_rf_model <- randomForest(GDP ~ ., data = train_data, ntree = 500, mtry = best_mtry, importance = TRUE, keep.forest = TRUE)
  
  if (return_model) {
    # Return the trained Random Forest model
    return(best_rf_model)
  }
  
  # Make predictions on the test data
  test_predictions <- predict(best_rf_model, test_data)
  
  # Create a data frame with actual and predicted values
  results <- data.frame(
    Actual = test_data$GDP,
    Predicted = test_predictions
  )
  
  # Calculate variance explained
  variance_explained <- 1 - var(test_predictions - test_data$GDP) / var(test_data$GDP)
  
  # Plot the actual values vs. predicted values
  actual_vs_predicted_plot <- ggplot(results, aes(x = Actual, y = Predicted)) +
    geom_point(alpha = 0.6) +
    geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
    labs(title = paste("Actual vs. Predicted Values -", country),
         x = "Actual GDP",
         y = "Predicted GDP") +
    theme_minimal()
  
  # Display the plot
  print(actual_vs_predicted_plot)
  
  # Train a single decision tree using rpart
  decision_tree <- rpart(GDP ~ ., data = train_data, control = rpart.control(cp = 0, maxdepth = 30))
  
  # Plot the full decision tree
  rpart.plot(decision_tree, main = paste("Full Decision Tree for GDP Prediction -", country), type = 3, extra = 1, under = TRUE, faclen = 0, cex = 0.6)
  
  # Plot OOB error rate vs. number of trees
  oob_error_data <- data.frame(
    Trees = 1:best_rf_model$ntree,
    OOBError = best_rf_model$mse
  )
  
  oob_error_vs_trees_plot <- ggplot(oob_error_data, aes(x = Trees, y = OOBError)) +
    geom_line(color = "blue") +
    labs(title = paste("OOB Error Rate vs. Number of Trees -", country),
         x = "Number of Trees",
         y = "OOB Error Rate (MSE)") +
    theme_minimal()
  
  # Display the plot
  print(oob_error_vs_trees_plot)
  
  # Return evaluation metrics
  return(data.frame(
    country = country,
    best_mtry = best_mtry,
    best_number_of_trees = 500,  # Since we are using 500 trees in the Random Forest model
    variance_explained = variance_explained
  ))
}
