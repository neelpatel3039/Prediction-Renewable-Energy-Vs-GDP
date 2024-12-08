# ---------------------------------- (1) Load necessary libraries ------------------------------ 

library(dplyr)
library(readr)
library(writexl)
library(tidyr)

# ----------------------------------------------------------------------------------------------

# ----------------------------------------------------------------------------------------------

# Set global options to avoid scientific notation 
options(scipen = 999)

# ----------------------------------------------------------------------------------------------

# ---------------------------------- (2) Import All Renewable Energy Datasets ------------------ 

# Get the current working directory | Next Import the datasets with appended file paths
current_dir <- paste0(getwd(), "/raw-datasets")

# Read renewable energy datasets
renewal_electricity_generation <- read_csv(file.path(current_dir, "renewal-electricity-generation.csv"))
renewal_energy_generation <- read_csv(file.path(current_dir, "renewal-energy-generation.csv"))
renewables_electricity_share <- read_csv(file.path(current_dir, "renewables-electricity-share.csv"))
renewables_energy_share <- read_csv(file.path(current_dir, "renewable-energy-share.csv"))

# Read specific energy source datasets
hydro_energy_share <- read_csv(file.path(current_dir, "hydro-energy-share.csv"))
solar_energy_share <- read_csv(file.path(current_dir, "solar-energy-share.csv"))
wind_energy_share <- read_csv(file.path(current_dir, "wind-energy-share.csv"))

hydro_electricity_share <- read_csv(file.path(current_dir, "hydro-electricity-share.csv"))
solar_electricity_share <- read_csv(file.path(current_dir, "solar-electricity-share.csv"))
wind_electricity_share <- read_csv(file.path(current_dir, "wind-electricity-share.csv"))

# Read energy consumption datasets
energy_consumption <- read_csv(file.path(current_dir, "energy-consumption.csv"))
energy_consumption_per_capita <- read_csv(file.path(current_dir, "energy-consumption-per-capita.csv"))
energy_consumption_change <- read_csv(file.path(current_dir, "energy-consumption-change.csv"))
grand_total_electricity_generation <- read_csv(file.path(current_dir, "electricity-generation.csv"))
electricity_generation_per_capita <- read_csv(file.path(current_dir, "electricity-gen-per-capita.csv"))

# Join the datasets into a master dataset
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

# ----------------------------------------------------------------------------------------------

# ---------------------------------- (3) Pre-process Combined Renewable Energy -----------------

# Remove the "Code" column from the master dataset 
master_dataset <- master_dataset %>% select(-starts_with("Code"))

# Filter the master dataset to include only the years from 1985 to 2023
filtered_master_dataset <- master_dataset %>%
  filter(Year >= 1985 & Year <= 2023)

# Rename columns in the dataframe for readability
colnames(filtered_master_dataset) <- c(
  "Entity", 
  "Year", 
  "Wind_Elec_TWh", 
  "Hydro_Elec_TWh", 
  "Solar_Elec_TWh", 
  "Other_Renew_Elec_TWh", 
  "Other_Renew_Gen_TWh", 
  "Solar_Gen_TWh", 
  "Wind_Gen_TWh", 
  "Hydro_Gen_TWh", 
  "Renew_Elec_Pct", 
  "Renew_Energy_Pct", 
  "Hydro_Energy_Pct", 
  "Solar_Energy_Pct", 
  "Wind_Energy_Pct", 
  "Hydro_Elec_Pct", 
  "Solar_Elec_Pct", 
  "Wind_Elec_Pct", 
  "Prim_Energy_Cons_TWh", 
  "Prim_Energy_Cons_Capita", 
  "Annual_Change_Cons_Pct", 
  "Elec_Gen_TWh", 
  "Elec_Capita_kWh"
)

# Removing Continents, Regions, World and Martinique since it is not present in Hydro dataset
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

# Filter the master dataset to remove unwanted entities
filtered_master_dataset <- filtered_master_dataset %>%
  filter(!(Entity %in% entities_to_remove))

# Standardise numerical precision to 3 decimal places
filtered_master_dataset <- filtered_master_dataset %>%
  mutate(across(where(is.numeric) & !c(Entity, Year), ~ round(., 3)))

# ----------------------------------------------------------------------------------------------

# ---------------------------------- (4) Import GDP Datasets ----------------------------------- 

# Read the GDP data
world_gdp <- read_csv(file.path(current_dir, "world-gdp.csv"))

# Read the economy classification data
economy_classification <- read_csv(file.path(current_dir, "economy-classification.csv"))

# ----------------------------------------------------------------------------------------------

# ---------------------------------- (5) Pre-process GDP Dataset ------------------------------- 

# Reshape the GDP data from wide to long format
world_gdp_long <- world_gdp %>%
  pivot_longer(cols = -Country, names_to = "Year", values_to = "GDP") %>%
  mutate(Year = as.integer(Year))  # Convert Year to integer if needed

# Join the GDP data with the filtered master data
master_data_with_gdp <- filtered_master_dataset %>%
  left_join(world_gdp_long, by = c("Entity" = "Country", "Year" = "Year")) %>% filter(!is.na(GDP))

# Convert GDP to Billion Dollars for consistency
master_data_with_gdp <- master_data_with_gdp %>% mutate(GDP = round(GDP / 1e9, 2))

# Calculate the change in GDP from the previous year
master_data_with_gdp <- master_data_with_gdp %>% arrange(Entity, Year) %>%
  group_by(Entity) %>% 
  mutate(Change_in_GDP = round((GDP - lag(GDP)) / lag(GDP) * 100, 3)) %>%
  ungroup()

# Join the Economy Classification data with the filtered master data
master_data_with_gdp <- master_data_with_gdp %>%
  left_join(economy_classification, by = c("Entity" = "Entity"))

# ----------------------------------------------------------------------------------------------

# ---------------------------------- (6) Remove all countries with NAs ------------------------ 

# Identify rows with at least one NA
rows_with_na <- master_data_with_gdp[rowSums(is.na(master_data_with_gdp)) > 0, ]

# View the updated dataframe
glimpse(master_data_with_gdp)

# Specify file name, location, and sheet name
file_location <- paste0(getwd(), "/processed-datasets")
file_name <- "master_data_with_gdp_world.csv"

# Save the updated dataframe with specified parameters
write_csv(master_data_with_gdp, file.path(file_location, file_name))

# ----------------------------------------------------------------------------------------------
