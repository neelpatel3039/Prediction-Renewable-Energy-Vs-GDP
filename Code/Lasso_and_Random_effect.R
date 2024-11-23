library(readr)
library(leaps)
library(ISLR2)
library(tidyverse)
library(dplyr)  
library(prophet)

data <- read_csv("C:/Users/yhsol/Downloads/master_data_with_gdp.csv")




------------------------------------------------------------------------------------
  

# Prepare data for Prophet
china_india_data <- china_india_data %>%
  mutate(ds = as.Date(paste(Year, "-01-01", sep = "")),
         y = GDP) %>%
  select(Entity, ds, y)

# Split data by country
china_data <- filter(china_india_data, Entity == "China")
india_data <- filter(china_india_data, Entity == "India")

# Fit Prophet model for China
china_model <- prophet(china_data %>% select(ds, y))
china_future <- make_future_dataframe(china_model, periods = 10, freq = "year")
china_forecast <- predict(china_model, china_future)

# Fit Prophet model for India
india_model <- prophet(india_data %>% select(ds, y))
india_future <- make_future_dataframe(india_model, periods = 10, freq = "year")
india_forecast <- predict(india_model, india_future)

# Plot results
plot(china_model, china_forecast) + ggtitle("China GDP Forecast")
plot(india_model, india_forecast) + ggtitle("India GDP Forecast")

# Evaluate Model
library(Metrics)

# Evaluation for China
china_actual <- tail(china_data$y, 10)
china_predicted <- tail(china_forecast$yhat, 10)
mae_china <- mae(china_actual, china_predicted)
rmse_china <- rmse(china_actual, china_predicted)

# Evaluation for India
india_actual <- tail(india_data$y, 10)
india_predicted <- tail(india_forecast$yhat, 10)
mae_india <- mae(india_actual, india_predicted)
rmse_india <- rmse(india_actual, india_predicted)

# Print evaluation metrics
cat("China: MAE =", mae_china, ", RMSE =", rmse_china, "\n")
cat("India: MAE =", mae_india, ", RMSE =", rmse_india, "\n")


------------------------------------------------------------------------------------------
library(MASS)
stepwise_model <- stepAIC(lm(GDP ~ ., data = data_clean), direction = "both")
summary(stepwise_model)


------------------------------------------------------------------------------------
  # Filter the data for Germany, France, and the Netherlands
filtered_data <- data %>%
  filter(Entity %in% c("Germany", "France", "Netherlands"))

# Ensure the time series structure is preserved
# Assuming your data has a 'Year' column and a 'GDP' or similar time-series column
filtered_data <- filtered_data %>%
  mutate(ds = as.Date(paste(Year, "-01-01", sep = ""))) %>% # Convert year to date format
  arrange(Entity, ds) # Sort by country and date

# Filtered significant data
significant_filtered_data <- filtered_data %>%
  dplyr::select(GDP, Year, Wind_Elec_TWh, Hydro_Elec_TWh, Other_Renew_Gen_TWh, 
                Solar_Gen_TWh, Wind_Gen_TWh, Hydro_Gen_TWh, Renew_Elec_Pct, 
                Renew_Energy_Pct, Hydro_Energy_Pct, Wind_Energy_Pct, 
                Hydro_Elec_Pct, Wind_Elec_Pct, Prim_Energy_Cons_Capita, 
                Elec_Gen_TWh)

# Fit the model with filtered data
filtered_model <- lm(GDP ~ Year + Wind_Elec_TWh + Hydro_Elec_TWh +
                       Other_Renew_Gen_TWh + Solar_Gen_TWh + Wind_Gen_TWh + 
                       Hydro_Gen_TWh + Renew_Elec_Pct + Renew_Energy_Pct + 
                       Hydro_Energy_Pct + Wind_Energy_Pct + Hydro_Elec_Pct + 
                       Wind_Elec_Pct + Prim_Energy_Cons_Capita + Elec_Gen_TWh, 
                     data = significant_filtered_data)
library(car)
# Summary of the model
summary(filtered_model)

vif_values <- vif(filtered_model)

# View VIF
print(vif_values)

library(glmnet)

# Prepare the data
x <- as.matrix(filtered_data %>% dplyr::select(Year, Wind_Elec_TWh, Hydro_Elec_TWh, Other_Renew_Gen_TWh,
                                        Solar_Gen_TWh, Wind_Gen_TWh, Hydro_Gen_TWh, Renew_Elec_Pct,
                                        Renew_Energy_Pct, Hydro_Energy_Pct, Wind_Energy_Pct,
                                        Hydro_Elec_Pct, Wind_Elec_Pct, Prim_Energy_Cons_Capita,
                                        Elec_Gen_TWh))
y <- filtered_data$GDP
# Remove rows with any NA values
complete_cases <- complete.cases(x)
x <- x[complete_cases,]
y <- y[complete_cases]
# Fit Ridge Regression
ridge_model <- glmnet(x, y, alpha = 0)

# Cross-validation to find the optimal lambda
cv_ridge <- cv.glmnet(x, y, alpha = 0)
best_lambda_ridge <- cv_ridge$lambda.min
cat("Optimal Lambda for Ridge:", best_lambda_ridge, "\n")

# Refit the model with optimal lambda
ridge_final <- glmnet(x, y, alpha = 0, lambda = best_lambda_ridge)
ridge_coefficients <- coef(ridge_final)
print(ridge_coefficients)

------------------------------------------------------------------
  # Fit Lasso Regression
lasso_model <- glmnet(x, y, alpha = 1)

# Cross-validation to find the optimal lambda
cv_lasso <- cv.glmnet(x, y, alpha = 1)
best_lambda_lasso <- cv_lasso$lambda.min
cat("Optimal Lambda for Lasso:", best_lambda_lasso, "\n")

# Refit the model with optimal lambda
lasso_final <- glmnet(x, y, alpha = 1, lambda = best_lambda_lasso)
lasso_coefficients <- coef(lasso_final)
print(lasso_coefficients)
---------------------------------------------------------------------------------
# Predictions
ridge_predictions <- predict(ridge_final, s = best_lambda_ridge, newx = x)
lasso_predictions <- predict(lasso_final, s = best_lambda_lasso, newx = x)

# Calculate MAE and RMSE
library(Metrics)
ridge_mae <- mae(y, ridge_predictions)
ridge_rmse <- rmse(y, ridge_predictions)

lasso_mae <- mae(y, lasso_predictions)
lasso_rmse <- rmse(y, lasso_predictions)

cat("Ridge - MAE:", ridge_mae, "RMSE:", ridge_rmse, "\n")
cat("Lasso - MAE:", lasso_mae, "RMSE:", lasso_rmse, "\n")

cv_lasso <- cv.glmnet(x, y, alpha = 1)  # alpha = 1 for Lasso
plot(cv_lasso)
best_lambda <- cv_lasso$lambda.min
final_lasso_model <- glmnet(x, y, alpha = 1, lambda = best_lambda)
---------------------------------------------------------------------
  library(glmnet)

# Remove rows with missing values
filtered_data_clean <- na.omit(filtered_data)

# Recreate x and y from the cleaned data
x_clean <- as.matrix(filtered_data_clean %>% select(Year, Wind_Elec_TWh, Hydro_Elec_TWh, Other_Renew_Gen_TWh, 
                                                    Solar_Gen_TWh, Wind_Gen_TWh, Hydro_Gen_TWh, Renew_Elec_Pct, 
                                                    Renew_Energy_Pct, Hydro_Energy_Pct, Wind_Energy_Pct, 
                                                    Prim_Energy_Cons_Capita, Elec_Gen_TWh))

y_clean <- filtered_data_clean$GDP

# Run Lasso model on the cleaned data
lasso_model <- glmnet(x_clean, y_clean, alpha = 1)

# Use cross-validation to find the best lambda (regularization parameter)
cv_lasso <- cv.glmnet(x_clean, y_clean, alpha = 1)

# The best lambda found by cross-validation
best_lambda <- cv_lasso$lambda.min

# Fit the final Lasso model with the best lambda
final_lasso_model <- glmnet(x_clean, y_clean, alpha = 1, lambda = best_lambda)

# Make predictions using the Lasso model
lasso_pred <- predict(final_lasso_model, newx = x_clean)

# Calculate MAE and RMSE
lasso_mae <- mean(abs(lasso_pred - y_clean))
lasso_rmse <- sqrt(mean((lasso_pred - y_clean)^2))

# Output the results
cat("Lasso - MAE:", lasso_mae, "RMSE:", lasso_rmse, "\n")

# Display the coefficients
lasso_coefficients <- coef(final_lasso_model)
print(lasso_coefficients)





