# Load necessary libraries
library(dplyr)
library(readr)
library(writexl)
library(tidyr)
library(ggplot2)
library(gridExtra)

# Load data
data <- read.csv("master_data_with_gdp.csv")

# Create a dataframe to store p-values and their significance
p_values_df <- data.frame(
  Country = character(),
  Variable = character(),
  P_Value = numeric(),
  Significant_0.05 = logical(),
  Significant_0.01 = logical(),
  Significant_0.001 = logical(),
  stringsAsFactors = FALSE
)

# Filter data for each country
data_india <- data %>% filter(Entity == 'India')
data_china <- data %>% filter(Entity == 'China')
data_usa <- data %>% filter(Entity == 'United States')

# Function to plot GDP vs. a given variable with a linear regression line and title
plot_regression <- function(df, x_var, y_var, country_name) {
  # Perform linear regression
  model <- lm(as.formula(paste(y_var, "~", x_var)), data = df)
  # Print summary of the model to get significance of correlation
  model_summary <- summary(model)
  print(model_summary)
  
  # Extract p-value and correlation significance
  p_value <- coef(model_summary)[2, 4]  # p-value for the predictor
  cat("P-value for", x_var, "in", country_name, ":", p_value, "\n")
  
  # Determine significance
  significant_0.05 <- p_value < 0.05
  significant_0.01 <- p_value < 0.01
  significant_0.001 <- p_value < 0.001
  
  # Store p-value and significance in the dataframe
  p_values_df <<- rbind(p_values_df, data.frame(
    Country = country_name,
    Variable = x_var,
    P_Value = p_value,
    Significant_0.05 = significant_0.05,
    Significant_0.01 = significant_0.01,
    Significant_0.001 = significant_0.001
  ))
  
  ggplot(df, aes_string(x = x_var, y = y_var)) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE, color = "red") +
    labs(title = paste("GDP vs", x_var),
         x = x_var,
         y = "GDP") +
    theme_minimal()
}

# Function to generate plots for a given country and group of variables
generate_plots <- function(data_country, country_name, variables, group_title) {
  plots <- list()
  for (var in variables) {
    cat("\n### Correlation Significance for", group_title, "-", var, "###\n")
    plot <- plot_regression(data_country, var, "GDP", country_name)
    plots <- append(plots, list(plot))
  }
  grid.arrange(grobs = plots, ncol = 1, top = group_title)
}

# Define variable groups
groups <- list(
  c("Wind_Elec_TWh", "Hydro_Elec_TWh", "Solar_Elec_TWh"),
  c("Other_Renew_Elec_TWh", "Other_Renew_Gen_TWh"),
  c("Solar_Gen_TWh", "Wind_Gen_TWh", "Hydro_Gen_TWh"),
  c("Renew_Elec_Pct", "Renew_Energy_Pct"),
  c("Hydro_Energy_Pct", "Solar_Energy_Pct", "Wind_Energy_Pct"),
  c("Hydro_Elec_Pct", "Solar_Elec_Pct", "Wind_Elec_Pct"),
  c("Prim_Energy_Cons_TWh", "Annual_Change_Cons_Pct"),
  c("Elec_Gen_TWh", "Elec_Capita_kWh")
)

# Titles for each group
group_titles <- c(
  "Wind, Hydro, and Solar Electricity",
  "Other Renewable Electricity and Generation",
  "Solar, Wind, and Hydro Generation",
  "Renewable Electricity and Energy Percentage",
  "Hydro, Solar, and Wind Energy Percentage",
  "Hydro, Solar, and Wind Electricity Percentage",
  "Primary Energy Consumption and Annual Change",
  "Electricity Generation and Per Capita Electricity"
)

# Generate and display plots for each group for India
for (i in seq_along(groups)) {
  generate_plots(data_india, "India", groups[[i]], paste("India:", group_titles[i]))
}

# Repeat for China
group_titles_china <- sub("India", "China", group_titles)
for (i in seq_along(groups)) {
  generate_plots(data_china, "China", groups[[i]], paste("China:", group_titles_china[i]))
}

# Repeat for USA
group_titles_usa <- sub("India", "USA", group_titles)
for (i in seq_along(groups)) {
  generate_plots(data_usa, "USA", groups[[i]], paste("USA:", group_titles_usa[i]))
}

# Print the dataframe with p-values and significance
print(p_values_df)

write_csv(p_values_df, "p_values_df.csv")

