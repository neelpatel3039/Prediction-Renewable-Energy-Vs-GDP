run_arima_forecast <- function(data, country_name) {
  country_data <- data %>% filter(Entity == country_name) %>% select(c("Year", "Wind_Elec_TWh"))
  
  View(country_data)
  print(class(country_data))
  
  gdptime <- ts(country_data$Wind_Elec_TWh, start = min(country_data$Year), end = max(country_data$Year), frequency = 1)
  print(class(gdptime))
  
  plot(gdptime, main = paste("Wind Energy Time Series for", country_name))
  
  acf(gdptime, main = paste("ACF for Wind Energy Time Series for", country_name))
  pacf(gdptime, main = paste("PACF for Wind Energy Time Series for", country_name))
  
  adf_test_result <- adf.test(gdptime)
  print(adf_test_result)
  
  gdpmodel <- auto.arima(gdptime, ic = "aic", trace = TRUE)
  print(gdpmodel)
  
  acf(ts(gdpmodel$residuals), main = paste("ACF of Residuals for", country_name))
  pacf(ts(gdpmodel$residuals), main = paste("PACF of Residuals for", country_name))
  
  mygdpforecast <- forecast(gdpmodel, level = c(95), h = 10 * 4)  # 10 years
  print(mygdpforecast)
  
  # Plot the forecast with axis labels
  plot(mygdpforecast, main = paste("Wind Energy Forecast for", country_name),
       xlab = "Year", ylab = "Wind Energy (TWh)")
  
  print(Box.test(mygdpforecast$resid, lag = 5, type = "Ljung-Box"))
  print(Box.test(mygdpforecast$resid, lag = 15, type = "Ljung-Box"))
  print(Box.test(mygdpforecast$resid, lag = 25, type = "Ljung-Box"))
  
  return(mygdpforecast)
}
