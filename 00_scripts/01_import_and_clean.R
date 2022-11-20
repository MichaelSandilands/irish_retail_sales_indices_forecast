rsi_import_and_clean <-
function(cso_id) {
  
  # IMPORT DATA
  data <- cso_get_data(cso_id) %>% 
    # PIVOT LONGER
    pivot_longer(
      cols      = -c(1, 2),
      names_to  = "date",
      values_to = "value"
    ) %>% 
    # CLEAN NAMES
    janitor::clean_names() %>% 
    # FILTER TO UNADJUSTED DATA
    filter(str_detect(statistic, "Unadjusted")) %>% 
    # CONVERT DATE COLUMN
    mutate(date = ym(date)) %>%
    # MISSING VALUES
    drop_na()
  
}
rsi_clean_anomalies <-
function(data, alpha = 0.5) {
  
  anomalies <- data %>% 
    group_by(statistic, nace_group) %>% 
    tk_anomaly_diagnostics(date, value, .message = FALSE, .alpha = alpha) %>% 
    pull(anomaly) %>% 
    unname()
  
  data <- data %>% 
    mutate(covid_anomalies = anomalies) %>% 
    mutate(value = ifelse(date >= "2020-01-01" & covid_anomalies == "Yes", NA_real_, value)) %>% 
    select(-covid_anomalies) %>% 
    group_by(statistic, nace_group) %>% 
    mutate(value = ts_impute_vec(value, period = 12)) %>% 
    ungroup()
  
  return(data)
  
}
