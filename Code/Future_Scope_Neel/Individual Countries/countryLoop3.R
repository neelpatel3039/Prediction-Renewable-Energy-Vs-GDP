# Load necessary libraries
library(dplyr)
library(caret)
library(ggplot2)  # For visualization

# Load your data
data <- read.csv("master_data_with_gdp.csv")  # Replace with your actual file

# Function to train and test linear regression for each country
train_test_model <- function(country_data, country_name) {
  set.seed(123)  # Set seed for reproducibility
  
  # Define the proportion for the training set
  train_proportion <- 0.8
  
  # Calculate the number of observations for the training set
  train_size <- floor(train_proportion * nrow(country_data))
  
  # Select the training indexes sequentially
  train_index <- 1:train_size
  
  # Split the data into training and testing sets using the sequential indexes
  train_data <- country_data[train_index, ]
  test_data <- country_data[-train_index, ]
  
  # Verify the split
  #print(train_data)
  #print(test_data)

  # Train the linear regression model

  model <- lm(GDP ~ Wind_Gen_TWh + Wind_Elec_TWh + Renew_Elec_Pct, data = train_data)
    
  # Predict on the test set
  predictions <- predict(model, newdata = test_data)
  
  # Calculate performance metrics
  actual <- test_data$GDP
  mse <- mean((predictions - actual) ^ 2)
  rmse <- sqrt(mse)
  r_squared <- summary(model)$r.squared
  
  # Create a dataframe for visualization
  vis_data <- data.frame(
    Year = test_data$Year,
    Actual = actual,
    Predicted = predictions
  )
  
  # Plot the actual vs. predicted values
  plot <- ggplot(vis_data, aes(x = Year)) +
    geom_line(aes(y = Actual, color = "Actual")) +
    geom_line(aes(y = Predicted, color = "Predicted")) +
    labs(title = paste("Actual vs Predicted GDP for", country_name),
         x = "Year",
         y = "GDP") +
    theme_minimal()
  
  # Return model, performance metrics, and plot
  list(
    model = model,
    predictions = predictions,
    actual = actual,
    mse = mse,
    rmse = rmse,
    r_squared = r_squared,
    plot = plot
  )
}

# Get a list of countries
countries <- unique(data$Entity)
#countries <- c("India", "China", "United States")

# Initialize a list to store results
results <- list()

# Loop through each country and train/test the model
for (country in countries) {
  cat("Processing country:", country, "\n")
  country_data <- data %>% filter(Entity == country)
  
  # Train and test the model
  result <- train_test_model(country_data, country)
  
  # Store the results
  results[[country]] <- result
  
  # Print the plot for visual inspection
  print(result$plot)
}

# Display the results
results
