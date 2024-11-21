# Load necessary libraries
library(dplyr)
library(readr)
library(writexl)
library(tidyr)
library(readr)
library(randomForest)
library(ggplot2)

data <- read.csv("master_data_with_gdp.csv")

# Select relevant columns
data <- data %>%
  select(Entity, Year, GDP, everything())

# Split the data into training and testing sets
set.seed(123)  # For reproducibility
train_indices <- sample(seq_len(nrow(data)), size = 0.8 * nrow(data))
train_data <- data[train_indices, ]
test_data <- data[-train_indices, ]

## Try lagging

# Train the Random Forest model
set.seed(123)
rf_model <- randomForest(GDP ~ . - Entity, data = train_data, ntree = 500, importance = TRUE)

# View the model summary
print(rf_model)

# Make predictions on the test set
predictions <- predict(rf_model, newdata = test_data)

# Calculate Mean Squared Error (MSE) for model evaluation
mse <- mean((test_data$GDP - predictions)^2)
print(paste("Mean Squared Error:", mse))

# Create future data points with the Entity column
future_years <- data.frame(
  Entity = rep("SampleCountry", 3),  # Replace with actual country or a placeholder
  Year = c(2033, 2073, 2123),
  Wind_Elec_TWh = c(100, 150, 200),
  Hydro_Elec_TWh = c(200, 250, 300),
  Solar_Elec_TWh = c(150, 200, 250),
  Other_Renew_Elec_TWh = c(50, 75, 100),
  Other_Renew_Gen_TWh = c(75, 100, 125),
  Solar_Gen_TWh = c(150, 200, 250),
  Wind_Gen_TWh = c(100, 150, 200),
  Hydro_Gen_TWh = c(200, 250, 300),
  Renew_Elec_Pct = c(60, 65, 70),
  Renew_Energy_Pct = c(50, 55, 60),
  Hydro_Energy_Pct = c(40, 45, 50),
  Solar_Energy_Pct = c(50, 55, 60),
  Wind_Energy_Pct = c(60, 65, 70),
  Hydro_Elec_Pct = c(40, 45, 50),
  Solar_Elec_Pct = c(50, 55, 60),
  Wind_Elec_Pct = c(60, 65, 70),
  Prim_Energy_Cons_TWh = c(1000, 1050, 1100),
  Prim_Energy_Cons_Capita = c(5000, 5500, 6000),
  Annual_Change_Cons_Pct = c(2, 2.5, 3),
  Elec_Gen_TWh = c(1000, 1100, 1200),
  Elec_Capita_kWh = c(5000, 5500, 6000),
  Developed_Flag = c(1, 1, 1),
  Developing_Flag = c(0, 0, 0)
)

# Print the future_years dataframe
print(future_years)

# Predict future GDP
future_gdp_predictions <- predict(rf_model, newdata = future_years)
print(future_gdp_predictions)

# Combine actual and predicted GDP for visualization
test_data$Predicted_GDP <- predictions

# Plot actual vs predicted GDP
ggplot(test_data, aes(x = Year)) +
  geom_line(aes(y = GDP, color = "Actual GDP")) +
  geom_line(aes(y = Predicted_GDP, color = "Predicted GDP")) +
  labs(title = "Actual vs Predicted GDP", x = "Year", y = "GDP") +
  theme_minimal() +
  scale_color_manual(values = c("Actual GDP" = "blue", "Predicted GDP" = "red"))
