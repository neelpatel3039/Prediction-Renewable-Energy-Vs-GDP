# Load necessary libraries
library(dplyr)
library(readr)
library(writexl)
library(tidyr)
library(readr)
library(ggplot2)
library(gridExtra)

data <- read.csv("master_data_with_gdp.csv") 

data_india <- data %>% filter(Entity == 'India')
data_china <- data %>% filter(Entity == 'China')
data_usa <- data %>% filter(Entity == 'United States')

# Define a function to plot GDP vs. a given variable with a linear regression line
plot_regression <- function(df, x_var, y_var) {
  ggplot(df, aes_string(x = x_var, y = y_var)) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE, color = "red") +
    labs(title = paste("GDP vs", x_var),
         x = x_var,
         y = "GDP") +
    theme_minimal()
}

plot_wind_india <- plot_regression(data_india, "Wind_Elec_TWh", "GDP")
plot_hydro_india <- plot_regression(data_india, "Hydro_Elec_TWh", "GDP")
plot_solar_india <- plot_regression(data_india, "Solar_Elec_TWh", "GDP")
grid.arrange(plot_wind_india, plot_hydro_india, plot_solar_india, ncol = 1)

# ------------------------------------------------------------------

plot_wind_china <- plot_regression(data_china, "Wind_Elec_TWh", "GDP")
plot_hydro_china <- plot_regression(data_china, "Hydro_Elec_TWh", "GDP")
plot_solar_china <- plot_regression(data_china, "Solar_Elec_TWh", "GDP")
grid.arrange(plot_wind_china, plot_hydro_china, plot_solar_china, ncol = 1)

# ------------------------------------------------------------------

plot_wind_usa <- plot_regression(data_usa, "Wind_Elec_TWh", "GDP")
plot_hydro_usa <- plot_regression(data_usa, "Hydro_Elec_TWh", "GDP")
plot_solar_usa <- plot_regression(data_usa, "Solar_Elec_TWh", "GDP")
grid.arrange(plot_wind_usa, plot_hydro_usa, plot_solar_usa, ncol = 1)

# ------------------------------------------------------------------

data_numeric <- data_india %>%
  select_if(is.numeric)

# Generate the correlation matrix
correlation_matrix <- cor(data_numeric, use = "complete.obs")

# Print the correlation matrix
print(correlation_matrix)

# If you want to visualize the correlation matrix, you can use the corrplot library
# Install the corrplot package if you haven't already
#install.packages("corrplot")

library(corrplot)

# Visualize the correlation matrix
corrplot(correlation_matrix, method = "color", tl.cex = 0.8)
