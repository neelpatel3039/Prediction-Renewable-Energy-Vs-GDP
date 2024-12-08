# ---------------------------------- (1) Load necessary libraries ------------------------------

library(dplyr)
library(readr)
library(writexl)
library(tidyr)
library(randomForest)
library(ggplot2)
library(rpart)
library(rpart.plot)
library(readxl)
library(forecast)
library(tseries)

# ----------------------------------------------------------------------------------------------

# ---------------------------------- (2) Source the external functions --------------------------

source("rf-model-builder.R")
source("arima-model-builder.R")

# ----------------------------------------------------------------------------------------------

# ---------------------------------- (3) Load and prepare the dataset ---------------------------

# Load the dataset for India
file_location <- file.path(getwd(), "processed-datasets")
data_india <- read.csv(file.path(file_location, "master_data_with_gdp_india.csv"))

# Load the world dataset
data_world <- read.csv(file.path(file_location, "master_data_with_gdp_world.csv"))

# Filter the Year column for India from the world dataset
year_data <- data_world %>% filter(Entity == "India") %>% select(Entity, Year)

# Add the Year column to the India dataset
data_india <- cbind(year_data, data_india)

# Save the updated India dataset
write.csv(data_india, file.path(file_location, "master_data_india_forecast.csv"), row.names = FALSE)

# Display the first few rows of the updated dataset

head(data_india)

# Train and evaluate the Random Forest model using the imported function
# Ensure to get the Random Forest model directly
best_rf_model <- build_best_rf_model(file.path(file_location, "master_data_with_gdp_india.csv"), "India", return_model = TRUE)
print(best_rf_model)

# ----------------------------------------------------------------------------------------------

# ---------------------------------- (4) Perform ARIMA forecast and diagnostics ----------------

# Filter the data to have only Year and Wind_Elec_TWh
data_india_arima <- data_india %>% select(Entity, Year, Wind_Elec_TWh)

# Predict the 2030 value of India's Wind_Elec_TWh
arima_result <- run_arima_forecast(data_india_arima, "India")
forecast_2030 <- tail(arima_result$mean[2030-2023], 1)  # Get the forecasted value for 2030
print(forecast_2030)

# ----------------------------------------------------------------------------------------------

# ---------------------------------- (5) Calculate and predict future GDP ----------------------------------

# Calculate the offset of the ARIMA forecasted 2030 value and the 2023 value
value_2023 <- data_india_arima %>% filter(Year == 2023) %>% select(Wind_Elec_TWh) %>% pull()
offset <- forecast_2030 - value_2023
print(offset)

# Use the offset with the Random Forest model to calculate the GDP offset for 2030
test_data_2023 <- data_india %>% filter(Year == 2023) %>% select(-c(Entity, Year, GDP))
test_data_2030 <- test_data_2023 %>% mutate(across(.cols = -Wind_Elec_TWh, ~0))
test_data_2030$Wind_Elec_TWh <- offset

# Predict the GDP for 2030 using the Random Forest model
gdp_2030_prediction <- predict(best_rf_model, newdata = test_data_2030)
gdp_2023 <- data_india %>% filter(Year == 2023) %>% select(GDP) %>% pull()

# Calculate the final GDP for 2030 by adding the GDP offset to the 2023 GDP
gdp_2030 <- gdp_2023 + gdp_2030_prediction
print(gdp_2030)

# ----------------------------------------------------------------------------------------------
