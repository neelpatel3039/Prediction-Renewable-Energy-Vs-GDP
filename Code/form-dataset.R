# Load necessary libraries
library(dplyr)
library(readr)
library(writexl)

# Get the current working directory
current_dir <- getwd()

# Import the datasets with appended file paths
renewal_electricity_generation <- read_csv(file.path(current_dir, "renewal-electricity-generation.csv"))
renewal_energy_generation <- read_csv(file.path(current_dir, "renewal-energy-generation.csv"))
renewables_electricity_share <- read_csv(file.path(current_dir, "renewables-electricity-share.csv"))
renewables_energy_share <- read_csv(file.path(current_dir, "renewable-energy-share.csv"))

# Ignore - Already covered in above datasets
#hydro_energy_consumption <- read_csv(file.path(current_dir, "hydro-energy-consumption.csv"))
#solar_energy_consumption <- read_csv(file.path(current_dir, "solar-energy-consumption.csv"))
#wind_energy_consumption <- read_csv(file.path(current_dir, "wind-energy-consumption.csv"))

hydro_energy_share <- read_csv(file.path(current_dir, "hydro-energy-share.csv"))
solar_energy_share <- read_csv(file.path(current_dir, "solar-energy-share.csv"))
wind_energy_share <- read_csv(file.path(current_dir, "wind-energy-share.csv"))

hydro_electricity_share <- read_csv(file.path(current_dir, "hydro-electricity-share.csv"))
solar_electricity_share <- read_csv(file.path(current_dir, "solar-electricity-share.csv"))
wind_electricity_share <- read_csv(file.path(current_dir, "wind-electricity-share.csv"))

energy_consumption <- read_csv(file.path(current_dir, "energy-consumption.csv"))
energy_consumption_per_capita <- read_csv(file.path(current_dir, "energy-consumption-per-capita.csv"))
energy_consumption_change <- read_csv(file.path(current_dir, "energy-consumption-change.csv"))
grand_total_electricity_generation <- read_csv(file.path(current_dir, "electricity-generation.csv"))
electricity_generation_per_capita <- read_csv(file.path(current_dir, "electricity-gen-per-capita.csv"))

# Join the datasets
master_dataset <- renewal_electricity_generation %>%
  full_join(renewal_energy_generation, by = c("Entity", "Year")) %>%
  full_join(renewables_electricity_share, by = c("Entity", "Year")) %>%
  full_join(renewables_energy_share, by = c("Entity", "Year")) %>%
  full_join(hydro_energy_share, by = c("Entity", "Year")) %>%
  full_join(solar_energy_share, by = c("Entity", "Year")) %>%
  full_join(wind_energy_share, by = c("Entity", "Year")) %>%
  full_join(hydro_electricity_share, by = c("Entity", "Year")) %>%
  full_join(solar_electricity_share, by = c("Entity", "Year")) %>%
  full_join(wind_electricity_share, by = c("Entity", "Year")) %>%
  full_join(energy_consumption, by = c("Entity", "Year")) %>%
  full_join(energy_consumption_per_capita, by = c("Entity", "Year")) %>%
  full_join(energy_consumption_change, by = c("Entity", "Year")) %>%
  full_join(grand_total_electricity_generation, by = c("Entity", "Year")) %>%
  full_join(electricity_generation_per_capita, by = c("Entity", "Year"))

# Remove the "Code" column from the master dataset 
master_dataset <- master_dataset %>% select(-starts_with("Code"))

# View the master dataset
print(master_dataset)

# Filter the master dataset to include only the years from 1985 to 2023
filtered_master_dataset <- master_dataset %>%
  filter(Year >= 1985 & Year <= 2023)

# List of entities to remove -- Removing Continents, Regions, World and Martinique since it is not present in Hydro
entities_to_remove <- c(
  "ASEAN (Ember)", "Africa", "Africa (EI)", "Africa (Ember)", "Asia", 
  "Asia (Ember)", "Asia Pacific (EI)", "CIS (EI)", "Central America (EI)", 
  "Eastern Africa (EI)", "Europe", "Europe (EI)", "Europe (Ember)", 
  "European Union (27)", "G20 (Ember)", "G7 (Ember)", "High-income countries", 
  "Latin America and Caribbean (Ember)", "Low-income countries", 
  "Lower-middle-income countries", "Middle Africa (EI)", "Middle East (EI)", 
  "Middle East (Ember)", "Non-OECD (EI)", "North America", "North America (EI)", 
  "North America (Ember)", "OECD (EI)", "OECD (Ember)", "Oceania", 
  "Oceania (Ember)", "South America", "South and Central America (EI)", 
  "Upper-middle-income countries", "Western Africa (EI)", "USSR", "World", "Martinique"
)

# Filter the master dataset
filtered_master_dataset <- master_dataset %>%
  filter(!(Entity %in% entities_to_remove))

# View the filtered dataset
print(filtered_master_dataset)

# Export the filtered master dataset to an Excel file
write_xlsx(filtered_master_dataset, "filtered_master_dataset.xlsx")
