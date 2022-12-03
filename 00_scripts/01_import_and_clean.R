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
function(data, alpha = 0.1){
  
  data %>% 
    group_by(statistic, nace_group) %>% 
    tk_anomaly_diagnostics(date, value, .message = FALSE, .alpha = alpha) %>% 
    mutate(
      value_clean = ifelse(
        anomaly == "Yes" & date >= "2020-01-01" & date < "2021-06-01", 
        NA_real_, 
        observed
      ),
      value_clean = ts_impute_vec(value_clean, period = 12)
    ) %>% 
    ungroup() %>% 
    select(statistic, nace_group, date, observed, value_clean) %>% 
    pivot_longer(c(observed, value_clean), names_to = "name") 
  
}
