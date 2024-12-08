# ---------------------------------- (1) Load necessary libraries ------------------------------

library(dplyr)
library(readr)
library(writexl)
library(tidyr)
library(randomForest)
library(ggplot2)
library(rpart)
library(rpart.plot)

# ----------------------------------------------------------------------------------------------

# ------------------------------- (2) Source the Random Forest Model Builder function ----------

source("rf-model-builder.R")

# ----------------------------------------------------------------------------------------------

# ---------------------------------- (3) List of country dataset files -------------------------

file_location <- file.path(getwd(), "processed-datasets")
country_files <- list(
  "Brazil" = "master_data_with_gdp_brazil.csv",
  "India" = "master_data_with_gdp_india.csv",
  "Canada" = "master_data_with_gdp_canada.csv",
  "Germany" = "master_data_with_gdp_germany.csv",
  "Saudi Arabia" = "master_data_with_gdp_saudi_arabia.csv"
)

# Initialize an empty dataframe to store evaluation metrics
evaluation_table <- data.frame(
  country = character(),
  best_mtry = integer(),
  best_number_of_trees = integer(),
  variance_explained = numeric(),
  stringsAsFactors = FALSE
)

# Loop through each file and process the data
for (country in names(country_files)) {
  file_path <- file.path(file_location, country_files[[country]])
  evaluation_metrics <- build_best_rf_model(file_path, country)
  
  if (!is.null(evaluation_metrics)) {
    evaluation_table <- rbind(evaluation_table, evaluation_metrics)
  }
}

# ----------------------------------------------------------------------------------------------

# ---------------------------------- (4) Print the final evaluation table ----------------------

# Print the final evaluation table
print(evaluation_table)

# ----------------------------------------------------------------------------------------------
