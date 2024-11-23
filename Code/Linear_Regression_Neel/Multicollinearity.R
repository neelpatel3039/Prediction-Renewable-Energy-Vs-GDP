# Load necessary libraries
library(dplyr)
library(corrplot)

# Load your data
data <- read.csv("master_data_with_gdp.csv")  # Replace with your actual file

# Select relevant columns for correlation
correlation_data <- data %>% select(Wind_Elec_TWh, Hydro_Elec_TWh, Solar_Elec_TWh, 
                                    Other_Renew_Elec_TWh, Other_Renew_Gen_TWh, Solar_Gen_TWh, 
                                    Wind_Gen_TWh, Hydro_Gen_TWh, Renew_Elec_Pct, Renew_Energy_Pct, 
                                    Hydro_Energy_Pct, Solar_Energy_Pct, Wind_Energy_Pct, 
                                    Hydro_Elec_Pct, Solar_Elec_Pct, Wind_Elec_Pct, 
                                    Prim_Energy_Cons_TWh, Prim_Energy_Cons_Capita, 
                                    Annual_Change_Cons_Pct, Elec_Gen_TWh, Elec_Capita_kWh)

# Calculate correlation matrix
cor_matrix <- cor(correlation_data, use = "complete.obs")

# Create a beautiful correlation plot
corrplot(cor_matrix, method = "color", type = "upper", 
         addCoef.col = "black",  # Add correlation coefficients
         tl.col = "black", tl.srt = 45,  # Text label color and rotation
         col = colorRampPalette(c("red", "white", "blue"))(200),  # Color palette
         title = "Multicollinearity Matrix for Renewable Energy and GDP Variables",
         mar = c(0, 0, 1, 0),  # Margins
         number.cex = 0.7)  # Adjust text size for coefficients

