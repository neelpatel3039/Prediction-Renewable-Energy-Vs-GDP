# ---------------------------------- (1) Set the directory of this file as the current working directory ------------------------------

current_dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(current_dir)

# ----------------------------------------------------------------------------------------------

# ---------------------------------- (2) Check and Install Required Libraries ------------------

required_packages <- c(
  "dplyr", "readr", "writexl", "tidyr", "randomForest", 
  "ggplot2", "rpart", "rpart.plot", "readxl", "forecast", "tseries"
)

# Checking and install missing packages
check_install_packages <- function(packages) {
  for (pkg in packages) {
    if (!require(pkg, character.only = TRUE)) {
      install.packages(pkg)
      library(pkg, character.only = TRUE)
      cat(paste("Installed and loaded:", pkg, "\n"))
    } else {
      cat(paste("Already installed and loaded:", pkg, "\n"))
    }
  }
}

# Check and install required packages
check_install_packages(required_packages)

# ----------------------------------------------------------------------------------------------
