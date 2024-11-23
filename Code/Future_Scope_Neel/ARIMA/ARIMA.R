library(readxl)
library(dplyr)
library(forecast)
library(tseries)

# Load the data
gdp <- read_csv("master_data_with_gdp.csv")

# Define a function to perform the ARIMA forecast and diagnostics
run_arima_forecast <- function(data, country_name) {
  country_data <- data %>% filter(Entity == country_name) %>% select(c("Year", "GDP"))
  
  View(country_data)
  print(class(country_data))
  
  gdptime <- ts(country_data$GDP, start = min(country_data$Year), end = max(country_data$Year), frequency = 1)
  print(class(gdptime))
  
  plot(gdptime, main = paste("GDP Time Series for", country_name))
  
  acf(gdptime, main = paste("ACF for GDP Time Series for", country_name))
  pacf(gdptime, main = paste("PACF for GDP Time Series for", country_name))
  
  adf_test_result <- adf.test(gdptime)
  print(adf_test_result)
  
  gdpmodel <- auto.arima(gdptime, ic = "aic", trace = TRUE)
  print(gdpmodel)
  
  acf(ts(gdpmodel$residuals), main = paste("ACF of Residuals for", country_name))
  pacf(ts(gdpmodel$residuals), main = paste("PACF of Residuals for", country_name))
  
  mygdpforecast <- forecast(gdpmodel, level = c(95), h = 10 * 4)  # 10 years
  print(mygdpforecast)
  
  plot(mygdpforecast, main = paste("GDP Forecast for", country_name))
  
  print(Box.test(mygdpforecast$resid, lag = 5, type = "Ljung-Box"))
  print(Box.test(mygdpforecast$resid, lag = 15, type = "Ljung-Box"))
  print(Box.test(mygdpforecast$resid, lag = 25, type = "Ljung-Box"))
}

# Run the ARIMA forecast and diagnostics for China
run_arima_forecast(gdp, "India")

# Run the ARIMA forecast and diagnostics for China
run_arima_forecast(gdp, "China")

# Run the ARIMA forecast and diagnostics for United States
run_arima_forecast(gdp, "United States")
