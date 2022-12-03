library(tidymodels)
library(tidyverse)
library(modeltime)
library(modeltime.resample)
library(modeltime.ensemble)
library(msand)
library(plotly)

grid_results <- read_rds("00_models/grid_results.rds")

g <- grid_results %>% 
  collect_metrics() %>% 
  filter(.metric == "rmse") %>% 
  arrange(mean) %>% 
  group_by(wflow_id) %>% 
  slice(1) %>% 
  ungroup() %>% 
  mutate(wflow_id = wflow_id %>% str_replace_all("_", " ") %>%  fct_reorder(mean),
         rank = min_rank(mean),
         conf_hi = mean + std_err,
         conf_low = mean - std_err) %>% 
  ggplot(aes(rank, mean, color = wflow_id)) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = conf_low, ymax = conf_hi), linewidth = 1) +
  theme_msand() +
  theme(
    strip.text.x = element_text(margin = margin(c(8,0,8,0))),
    panel.spacing.y = unit(1.5, "lines"),
    panel.grid.minor = NULL, 
    panel.grid.major = element_line(colour = alpha("black", 0.15))
  ) +
  labs(
    color = "Model"
  )

ggplotly(g)

rsi_cross_validation_results <- function(data, metric = "rmse", select_best = TRUE) {
  
  data <- collect_metrics(data) %>% 
    filter(.metric == metric) %>% 
    arrange(mean)
  
  if(select_best) {
    
    data <- data %>% 
      group_by(wflow_id) %>% 
      slice(1) %>% 
      ungroup()
    
  }
  
  g <- data %>% 
    mutate(wflow_id = wflow_id %>% str_replace_all("_", " ") %>%  fct_reorder(mean),
           rank = min_rank(mean),
           conf_hi = mean + std_err,
           conf_low = mean - std_err) %>%  
    ggplot(aes(rank, mean, color = wflow_id)) +
    geom_point(size = 2) +
    geom_errorbar(aes(ymin = conf_low, ymax = conf_hi), linewidth = 1) +
    theme_msand() +
    theme(
      strip.text.x = element_text(margin = margin(c(8,0,8,0))),
      panel.spacing.y = unit(1.5, "lines"),
      panel.grid.minor = NULL, 
      panel.grid.major = element_line(colour = alpha("black", 0.15))
    ) +
    labs(
      color = "Model"
    )
  
  ggplotly(g)
  
  
}


rsi_cross_validation_results(grid_results)


submodels_tbl <- read_rds("00_models/submodels_tbl.rds")

library(tidyverse)
library(timetk)
library(csodata)
library(tidymodels)
library(lubridate)
library(plotly)
library(modeltime)
library(skimr)
library(baguette)
library(rules)


source("00_scripts/01_import_and_clean.R")

rsi_monthly_tbl <- rsi_import_and_clean("RSM05")

# IMPORT DATA ----

FORECAST_HORIZON <- 12

# Monthly series 
full_data_tbl <- rsi_import_and_clean("RSM05") %>%
  rsi_clean_anomalies() %>% 
  filter(name == "value_clean") %>% 
  mutate(id = str_c(statistic, " - ", nace_group)) %>% 
  select(id, date, value) %>% 
  # Log Transform Target
  mutate(value = log(value)) %>%
  # Group-Wise Feature Transformations
  group_by(id) %>%
  arrange(date) %>% 
  # Extending
  future_frame(date, .length_out = FORECAST_HORIZON, .bind_data = TRUE) %>%
  # Fourier
  tk_augment_fourier(
    date, .periods = c(3, 6, FORECAST_HORIZON)
  ) %>% 
  # Lag
  tk_augment_lags(
    .value = value, .lags = FORECAST_HORIZON
  ) %>% 
  # Rolling Features
  tk_augment_slidify(
    .value   = str_c("value_lag", FORECAST_HORIZON),
    .f       = ~ mean(.x, na.rm = TRUE),
    .period  = c(3, 6, FORECAST_HORIZON, 2*FORECAST_HORIZON),
    .partial = TRUE, 
    .align   = "center"
  ) %>% 
  ungroup() 


rsi_data_engineering <- function(data, clean_anomalies = TRUE, alpha = 0.1,
                                 log_transform = TRUE, FORECAST_HORIZON = 12) {
  
  if(clean_anomalies) {
    
    data <- data %>% 
      rsi_clean_anomalies(alpha = alpha) %>% 
      filter(name == "value_clean") %>% 
      select(-name)
    
  }
  
  if(log_transform) {
    
    data <- data %>% 
      mutate(value = log(value))
    
  }
  
  data <- data %>% 
    group_by(statistic, nace_group) %>% 
    arrange(date) %>% 
    # Extending
    future_frame(date, .length_out = FORECAST_HORIZON, .bind_data = TRUE) %>%
    # Fourier
    tk_augment_fourier(
      date, .periods = c(3, 6, FORECAST_HORIZON)
    ) %>% 
    # Lag
    tk_augment_lags(
      .value = value, .lags = FORECAST_HORIZON
    ) %>% 
    # Rolling Features
    tk_augment_slidify(
      .value   = str_c("value_lag", FORECAST_HORIZON),
      .f       = ~ mean(.x, na.rm = TRUE),
      .period  = c(3, 6, FORECAST_HORIZON, 2*FORECAST_HORIZON),
      .partial = TRUE, 
      .align   = "center"
    ) %>% 
    ungroup() 
  
  return(data)
  
}

dump("rsi_data_engineering", "00_scripts/03_modeling.R")


full_data_tbl <- rsi_data_engineering(rsi_monthly_tbl)

data_prepared_tbl <- full_data_tbl %>%
  filter(!is.na(value)) %>% 
  drop_na()

actual_data_tbl <- data_prepared_tbl %>% 
  select(-value) %>% 
  left_join(rsi_monthly_tbl %>% mutate(value = log(value)), by = c("statistic", "nace_group", "date"))

future_tbl <- full_data_tbl %>%
  filter(is.na(value))

splits <- data_prepared_tbl %>%
  time_series_split(date, assess = FORECAST_HORIZON, cumulative = TRUE)


recipe_spec <- recipe(value ~ ., data = training(splits)) %>%
  step_timeseries_signature(date) %>%
  step_rm(matches("(.xts$)|(.iso$)|(week)|(day)|(hour)|(minute)|(second)|(am.pm)")) %>% 
  step_normalize(date_index.num, date_year) %>%
  step_dummy(all_nominal_predictors(), one_hot = TRUE) %>% 
  step_zv(all_predictors())

recipe_spec %>% prep() %>% juice() %>% glimpse()

recipe_no_date_spec <- recipe_spec %>% 
  step_rm(date)

best_model_list <- read_rds("00_models/best_model_list.rds")

submodels_tbl <- modeltime_table(
  best_model_list[[1]] %>% fit(training(splits)),
  best_model_list[[2]] %>% fit(training(splits)),
  best_model_list[[3]] %>% fit(training(splits)),
  best_model_list[[4]] %>% fit(training(splits)),
  best_model_list[[5]] %>% fit(training(splits)),
  best_model_list[[6]] %>% fit(training(splits)),
  best_model_list[[7]] %>% fit(training(splits))
) %>% 
  

calibration_tbl <- submodels_tbl %>% 
  modeltime_calibrate(testing(splits))

calibration_tbl %>% 
  modeltime_accuracy() %>% 
  arrange(rmse)

modeltime_table(
  ensemble_average(submodels_calib_tbl)
) %>%
  modeltime_calibrate(testing(splits)) %>% 
  modeltime_accuracy()

calibration_tbl %>% 
  modeltime_forecast(
    new_data = testing(splits),
    actual_data = actual_data_tbl,
    keep_data = TRUE
  ) %>% 
  filter(statistic == "Retail Sales Index Value Unadjusted") %>% 
  group_by(nace_group) %>% 
  plot_modeltime_forecast(.conf_interval_show = FALSE, .facet_ncol = 3)

