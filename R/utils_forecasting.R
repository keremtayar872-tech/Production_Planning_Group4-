# R/utils_forecasting.R

# Calculate error metrics: MAD, MSE, MAPE
calc_metrics <- function(actual, forecast) {
  idx <- !is.na(actual) & !is.na(forecast)
  act <- actual[idx]
  frc <- forecast[idx]
  
  if (length(act) == 0) return(list(MAD = NA, MSE = NA, MAPE = NA))
  
  error <- act - frc
  mad <- mean(abs(error))
  mse <- mean(error^2)
  mape <- mean(abs(error / act)) * 100
  
  return(list(
    MAD = round(mad, 4),
    MSE = round(mse, 4),
    MAPE = round(mape, 4)
  ))
}

# Automatically fit regression to obtain initial Level (intercept) and Slope (trend)
fit_initial_values <- function(data) {
  t <- 1:length(data)
  fit <- lm(data ~ t)
  coefs <- coef(fit)
  return(list(
    level = unname(coefs[1]),
    slope = unname(coefs[2])
  ))
}