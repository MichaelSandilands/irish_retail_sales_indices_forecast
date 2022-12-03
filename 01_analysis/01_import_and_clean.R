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

rsi_monthly_tbl <- rsi_import_and_clean("RSM05")

# FLAG COVID ANOMALIES

rsi_monthly_clean_tbl <- rsi_monthly_tbl %>% 
  group_by(statistic, nace_group) %>% 
  tk_anomaly_diagnostics(date, value, .message = FALSE, .alpha = 0.1) %>% 
  mutate(
    value_clean = ifelse(
      anomaly == "Yes" & date >= "2020-01-01" & date < "2022-01-01", 
      NA_real_, 
      observed
      ),
    value_clean = ts_impute_vec(value_clean, period = 12)
    ) %>% 
  select(statistic, nace_group, date, observed, value_clean) %>% 
  pivot_longer(c(observed, value_clean))

rsi_clean_anomalies <- function(data, alpha = 0.1){
  
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

rsi_monthly_tbl %>% 
 rsi_clean_anomalies()




dump(c("rsi_import_and_clean", "rsi_clean_anomalies"), 
     "00_scripts/01_import_and_clean.R")
