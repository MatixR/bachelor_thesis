##----------------------------------------------------------------------------------------------------------##
## Forecasting using a moving window.
##----------------------------------------------------------------------------------------------------------##
## Select data for a window of a certain length
window_data <- function(list_data, window_length, i)
{
  window_data <- list()
  for (j in 1:length(list_data)) 
  {
    window_data[[j]] <- list_data[[j]][i:(i+window_length-1),]
  }
  return(window_data)
}





## Function to obtain the domestic factors from the ARIMA fit
recursive_f <- function(dynamics, global_factors)
{
  global_factors <- t(global_factors)
  if (length(dynamics) == 1)
  {
    f <- matrix(nrow = length(dynamics[[1]][[1]]), ncol = length(dynamics[[1]]))
    for (i in 1:length(dynamics[[1]])) 
    {
      for (j in 1:length(dynamics[[1]][[1]])) 
      {
        f[j,i] <- predict(dynamics[[1]][[i]][[j]], n.ahead = 1, newxreg = global_factors[j])$pred
      }
    }
    return(f)
  }
  else
  {
    regional_factors <- matrix(ncol = length(dynamics[[1]]), nrow = 1)
    for (i in 1:length(dynamics[[1]])) 
    {
      regional_factors[i] <- predict(dynamics[[1]][[i]], n.ahead = 1, newxreg = global_factors[i])$pred
    }
    dynamics <- dynamics[-1]
    f <- matrix(nrow = length(dynamics[[1]][[1]]), ncol = length(dynamics[[1]]))
    for (i in 1:length(dynamics[[1]])) 
    {
      for (j in 1:length(dynamics[[1]][[1]])) 
      {
        f[j,i] <- predict(dynamics[[1]][[i]][[j]], n.ahead = 1, newxreg = global_factors[j])$pred
      }
    }
    return(f)
  }
}





## Function to calculate yield forecast given specific latent factor dynamcis
next_forecast <- function(dynamics)
{
  # Predict the one-step-ahead forecast of the domestic latent factors for both specifications.
  ARmodel_global <- dynamics[[1]]
  globalFactor_forecast <- matrix(nrow = length(ARmodel_global), ncol = 1)
  for (i in 1:length(ARmodel_global)) 
  {
    forecast <- predict(ARmodel_global[[i]], n.ahead = 1)
    globalFactor_forecast[i] <- forecast$pred
  }
  # Obtain forecasted domestic latent factors
  forecasted_factors <- recursive_f(dynamics[-1], globalFactor_forecast)
  return(forecasted_factors)
}




## Forecast data using a moving window
forecast <- function(list_data, window_length)
{
  tau <- strtoi(colnames(list_data[[1]]))
  lambda <- 0.064
  slope <- (1 - exp(1)^(-lambda * tau))/(lambda*tau)
  curvature <- slope - exp(1)^(-lambda * tau)
  
  forecast_list <- list()
  for (i in 1:length(list_data)) 
  {
    forecast_list[[i]] <- matrix(ncol = length(list_data[[i]][1,]), 
                                 nrow = length(list_data[[i]][,1]) - window_length)
    colnames(forecast_list[[i]]) <- colnames(list_data[[1]])
    forecast_list[[i]] <- as.zoo(forecast_list[[i]])
    index(forecast_list[[i]]) <- index(list_data[[1]][(window_length+1):length(list_data[[1]][,1])])
  }
  
  forecast_main <- forecast_list
  forecast_ext <- forecast_list
  remove(forecast_list)
  
  for (i in 1:(length(list_data[[1]][,1]) - window_length)) 
  {
    data_window <- window_data(list_data, window_length, i)
    local_factors <- retrieve_country_lf(data_window)
    NA_factors <- pca(local_factors, 1:2)
    EUR_factors <- pca(local_factors, 3:6)
    ASIA_factors <- pca(local_factors, 7:10)
    Ext_factors <- pca_region(list(NA_factors$global, EUR_factors$global, ASIA_factors$global))
    Main_factors <- pca(local_factors, 1:10)
    
    Main_dynamics <- factor_dynamicsMain(Main_factors$local, Main_factors$global)
    NA_dynamics <- factor_dynamicsExt(NA_factors$local, NA_factors$global, Ext_factors)
    EUR_dynamics <- factor_dynamicsExt(EUR_factors$local, EUR_factors$global, Ext_factors)
    ASIA_dynamics <- factor_dynamicsExt(ASIA_factors$local, ASIA_factors$global, Ext_factors)

    Main_factors_1 <- next_forecast(Main_dynamics)
    NA_factors_1 <- next_forecast(NA_dynamics)
    EUR_factors_1 <- next_forecast(EUR_dynamics)
    ASIA_factors_1 <- next_forecast(ASIA_dynamics)
    Ext_factors_1 <- cbind(NA_factors_1, EUR_factors_1, ASIA_factors_1)
    
    for (j in 1:length(list_data)) 
    {
      forecast_main[[j]][i,] <- t(Main_factors_1[1,j] + Main_factors_1[2,j]*slope + Main_factors_1[3,j]*curvature)
      forecast_ext[[j]][i,] <- t(Ext_factors_1[1,j] + Ext_factors_1[2,j]*slope + Ext_factors_1[3,j]*curvature)
    }      
  }
  
  return(list(main = forecast_main, ext = forecast_ext))
}
##----------------------------------------------------------------------------------------------------------##
##----------------------------------------------------------------------------------------------------------##