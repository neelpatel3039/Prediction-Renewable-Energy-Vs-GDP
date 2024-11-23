library(MASS)
stepwise_model <- stepAIC(lm(GDP ~ ., data = data_clean), direction = "both")
summary(stepwise_model)