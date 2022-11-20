# IRELAND RETAIL SALES INDEX SHINY APP ---- 
# IMPORT DATA ----

# LIBRARIES ----

library(tidyverse)
library(timetk)
library(lubridate)
library(csodata)
library(skimr)
library(plotly)

# IMPORT DATA ----

rsi_monthly_tbl <- cso_get_data("RSM05") 

rsi_monthly_tbl %>% glimpse()

# PIVOT LONGER ----

rsi_monthly_long_tbl <- rsi_monthly_tbl %>% 
  pivot_longer(
    cols      = -c(1, 2),
    names_to  = "date",
    values_to = "value"
  )

rsi_monthly_long_tbl %>% skim()

# CLEAN NAMES ----

rsi_monthly_clean_names_tbl <- rsi_monthly_long_tbl %>% 
  janitor::clean_names()

# STATISTIC ----

rsi_monthly_clean_names_tbl %>% 
  distinct(statistic)

# FILTER STATISTIC

rsi_monthly_filter_statistic_tbl <- rsi_monthly_clean_names_tbl %>% 
  filter(str_detect(statistic, "Unadjusted"))


# CONVERT DATE COLUMN ----

rsi_monthly_convert_date_tbl <- rsi_monthly_filter_statistic_tbl %>% 
  mutate(date = ym(date))

# MISSING VALUES ----

rsi_monthly_convert_date_tbl %>% 
  filter(is.na(value))

# PAD BY TIME CHECK ----

rsi_monthly_convert_date_tbl %>% nrow()

rsi_monthly_convert_date_tbl %>% 
  group_by(statistic, nace_group) %>% 
  pad_by_time(date, .by = "month", .pad_value = NA) %>% 
  ungroup() %>% 
  nrow()

# pad_by_time not necessary

# TK SUMMARY ----

rsi_monthly_convert_date_tbl %>% 
  group_by(statistic, nace_group) %>% 
  tk_summary_diagnostics() 

# VISUALIZE ----

rsi_monthly_convert_date_tbl %>% 
  group_by(statistic, nace_group) %>%
  plot_time_series(
    date, value,
    .trelliscope = TRUE,
  )



# ANOMALIES ----

rsi_monthly_convert_date_tbl %>% 
  group_by(statistic, nace_group) %>% 
  plot_anomaly_diagnostics(
    date, value, .message = FALSE,
    .trelliscope = TRUE
  )

# FLAG COVID ANOMALIES

anomalies <- rsi_monthly_convert_date_tbl %>% 
  group_by(statistic, nace_group) %>% 
  tk_anomaly_diagnostics(date, value, .message = FALSE) %>% 
  pull(anomaly) %>% 
  unname()

rsi_monthly_anomalies_tbl <- rsi_monthly_convert_date_tbl %>% 
  mutate(covid_anomalies = anomalies) %>% 
  mutate(value = ifelse(date >= "2020-01-01" & covid_anomalies == "Yes", NA_real_, value)) %>% 
  select(-covid_anomalies)

rsi_monthly_anomalies_tbl %>% 
  group_by(statistic, nace_group) %>% 
  mutate(value = ts_impute_vec(value, period = 12)) %>% 
  plot_time_series(
    date, value, .trelliscope = TRUE
  )


# IMPORT & CLEAN FUNCTION ----

rsi_import_and_clean <- function(cso_id) {
  
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

rsi_clean_anomalies <- function(data, alpha = 0.5) {
  
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

rsi_monthly_tbl <- rsi_import_and_clean("RSM05") %>% 
  rsi_clean_anomalies()

rsi_monthly_tbl %>% 
  group_by(statistic, nace_group) %>% 
  plot_anomaly_diagnostics(
    date, value, .message = FALSE,
    .trelliscope = TRUE, .interactive = TRUE
  )


dump(c("rsi_import_and_clean", "rsi_clean_anomalies"), "00_scripts/01_import_and_clean.R")
