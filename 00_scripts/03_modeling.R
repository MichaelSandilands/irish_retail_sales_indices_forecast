rsi_data_engineering <-
function(data, forecast_horizon = 12) {
  
  
  data <- data %>% 
    group_by(statistic, nace_group) %>% 
    arrange(date) %>% 
    # Extending
    future_frame(date, .length_out = forecast_horizon, .bind_data = TRUE) %>%
    # Fourier
    tk_augment_fourier(
      date, .periods = c(3, 6, forecast_horizon)
    ) %>% 
    # Lag
    tk_augment_lags(
      .value = value, .lags = forecast_horizon
    ) %>% 
    # Rolling Features
    tk_augment_slidify(
      .value   = str_c("value_lag", forecast_horizon),
      .f       = ~ mean(.x, na.rm = TRUE),
      .period  = c(3, 6, forecast_horizon, 2*forecast_horizon),
      .partial = TRUE, 
      .align   = "center"
    ) %>% 
    ungroup() 
  
  return(data)
  
}
