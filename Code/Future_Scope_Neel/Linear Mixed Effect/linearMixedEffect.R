# Load necessary libraries
library(lme4)
library(ggplot2)

# Load your data
data <- read.csv("master_data_with_gdp.csv")  # Replace with your actual file

# Fit the linear mixed model
model <- lmer(GDP ~ Renew_Elec_Pct + (1 | Entity), data = data)

# Summary of the model
summary(model)