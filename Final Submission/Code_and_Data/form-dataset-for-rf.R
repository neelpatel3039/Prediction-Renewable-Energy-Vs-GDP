# ---------------------------------- (1) Load necessary libraries ------------------------------

library(dplyr)

# ----------------------------------------------------------------------------------------------

# ---------------------------------- (2) Read the master dataset -------------------------------

# Read the master dataset
master_data <- read.csv("processed-datasets/master_data_with_gdp_world.csv")

# ----------------------------------------------------------------------------------------------

# ---------------------------------- (3) List of countries to filter and save ------------------

# List of countries to filter
countries <- c("Brazil", "India", "Canada", "Germany", "Saudi Arabia")

# ----------------------------------------------------------------------------------------------

# ---------------------------------- (4) Function to filter and save country data --------------

# Function to filter data for a specific country and save to a new file
save_country_data <- function(country) {
  country_data <- master_data %>%
    filter(Entity == country) %>%
    select(-Entity, -Year, -Change_in_GDP, -Developed_Flag, -Developing_Flag) %>%
    mutate(across(everything(), ~ifelse(is.na(.), 0, .)))  # Fill NA's with zeros
  
  file_name <- paste0("processed-datasets/master_data_with_gdp_", tolower(gsub(" ", "_", country)), ".csv")
  write.csv(country_data, file_name, row.names = FALSE)
}

# ----------------------------------------------------------------------------------------------

# ---------------------------------- (5) Apply the function to each country --------------------

# Apply the function to each country
lapply(countries, save_country_data)

# ----------------------------------------------------------------------------------------------