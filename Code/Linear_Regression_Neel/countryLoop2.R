# Load necessary libraries
library(dplyr)
library(caret)
library(ggplot2)  # For visualization

# Load your data
data <- read.csv("master_data_with_gdp.csv")  # Replace with your actual file

data_top_10_economics <- data %>% 
  filter(Entity %in% c("United States","China","Germany","Japan","India","United Kingdom","France","Brazil","Italy"))

# Function to select the top 3 most important variables using RFE and train/test linear regression for each country
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
  
  # Convert necessary columns to factors
  train_data <- train_data %>% mutate_if(is.character, as.factor)
  test_data <- test_data %>% mutate_if(is.character, as.factor)
  
  # Remove variables with only one level
  train_data <- train_data %>% select_if(~ nlevels(as.factor(.)) > 1)
  test_data <- test_data %>% select_if(~ nlevels(as.factor(.)) > 1)
  
  # Define the control using RFE
  control <- rfeControl(functions = lmFuncs, method = "cv", number = 10)
  
  # Run the RFE algorithm to select the top 3 variables
  rfe_result <- rfe(
    train_data %>% select(-GDP, -Year), 
    train_data$GDP, 
    sizes = 3, 
    rfeControl = control
  )
  
  # Get the top 3 selected variables
  selected_vars <- predictors(rfe_result)
  
  # Print the selected variables
  cat("Selected variables for", country_name, ":", selected_vars, "\n")
  
  # Create a formula for the linear regression model using the selected variables
  formula <- as.formula(paste("GDP ~", paste(selected_vars, collapse = " + ")))
  
  # Train the linear regression model
  model <- lm(formula, data = train_data)
  
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
countries <- unique(data_top_10_economics$Entity)

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
