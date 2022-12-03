# IRELAND NATIONAL AVERAGE PRICES SHINY APP ---- 
# IMPORT & CLEAN DATA ----

# LIBRARIES ----

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

# IMPORT DATA ----

FORECAST_HORIZON <- 12

# Monthly series 
full_data_tbl <- rsi_import_and_clean("RSM05") %>%
  rsi_clean_anomalies() %>% 
  filter(name == "value_clean") %>% 
  select(-name) %>% 
  # Log Transform Target
  mutate(value = log(value)) %>%
  # Group-Wise Feature Transformations
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

full_data_tbl %>% glimpse()

# * Data Prepared ----

data_prepared_tbl <- full_data_tbl %>%
  filter(!is.na(value)) %>% 
  drop_na()

data_prepared_tbl %>% skim()


# * Future Data ----
future_tbl <- full_data_tbl %>%
  filter(is.na(value))

future_tbl %>% skim()

# 2.0 TIME SPLIT ----

splits <- data_prepared_tbl %>% 
  time_series_split(date, assess = FORECAST_HORIZON, cumulative = TRUE)


# 3.0 RECIPE ----

recipe_spec <- recipe(value ~ ., data = training(splits)) %>%
  step_timeseries_signature(date) %>%
  step_rm(matches("(.xts$)|(.iso$)|(week)|(day)|(hour)|(minute)|(second)|(am.pm)")) %>% 
  step_normalize(date_index.num, date_year) %>%
  step_dummy(all_nominal_predictors(), one_hot = TRUE) %>% 
  step_zv(all_predictors())

recipe_spec %>% prep() %>% juice() %>% glimpse()

recipe_no_date_spec <- recipe_spec %>% 
  step_rm(date)


# 4.0 MODELS ----

set.seed(1991)
resamples_cv <- training(splits) %>% vfold_cv(v = 10)

prophet_spec <- 
  prophet_reg(prior_scale_changepoints = tune(), prior_scale_holidays = tune(), 
              season = tune()) %>% 
  set_engine("prophet")

prophet_param <- 
  prophet_spec %>% 
  extract_parameter_set_dials() %>% 
  update(season = season(values = c("additive", "multiplicative")))

prophet_boost_spec <- 
  prophet_boost(tree_depth = tune(), learn_rate = tune(), loss_reduction = tune(), 
                min_n = tune(), trees = tune(), mtry = tune(), sample_size = tune()) %>% 
  set_engine("prophet_xgboost")

prophet_boost_param <- 
  prophet_boost_spec %>% 
  extract_parameter_set_dials() %>% 
  update(sample_size = sample_prop())

linear_reg_spec <- 
  linear_reg(penalty = tune(), mixture = tune()) %>% 
  set_engine("glmnet")

nnet_spec <- 
  mlp(hidden_units = tune(), penalty = 0.9, epochs = tune()) %>% 
  set_engine("nnet", MaxNWts = 2600) %>% 
  set_mode("regression")

nnet_param <- 
  nnet_spec %>% 
  extract_parameter_set_dials() %>% 
  update(hidden_units = hidden_units(c(1, 27)))

mars_spec <- 
  mars(prod_degree = tune(), num_terms = tune()) %>%  #<- use GCV to choose terms
  set_engine("earth") %>% 
  set_mode("regression")

svm_r_spec <- 
  svm_rbf(cost = tune(), rbf_sigma = tune(), margin = tune()) %>% 
  set_engine("kernlab") %>% 
  set_mode("regression")

svm_p_spec <- 
  svm_poly(cost = tune(), degree = tune(), scale_factor = tune(), margin = tune()) %>% 
  set_engine("kernlab") %>% 
  set_mode("regression")

knn_spec <- 
  nearest_neighbor(neighbors = tune(), dist_power = tune(), weight_func = tune()) %>% 
  set_engine("kknn") %>% 
  set_mode("regression")

cart_spec <- 
  decision_tree(cost_complexity = tune(), min_n = tune(), tree_depth = tune()) %>% 
  set_engine("rpart") %>% 
  set_mode("regression")

bag_cart_spec <- 
  bag_tree(cost_complexity = tune(), min_n = tune(), tree_depth = tune()) %>% 
  set_engine("rpart", times = 50L) %>% 
  set_mode("regression")

rf_spec <- 
  rand_forest(mtry = tune(), min_n = tune(), trees = 1000) %>% 
  set_engine("ranger") %>% 
  set_mode("regression")

xgb_spec <- 
  boost_tree(tree_depth = tune(), learn_rate = tune(), loss_reduction = tune(), 
             min_n = tune(), trees = 1000, mtry = tune(), sample_size = tune()) %>% 
  set_engine("xgboost") %>% 
  set_mode("regression")

xgb_param <- 
  xgb_spec %>% 
  extract_parameter_set_dials() %>% 
  update(sample_size = sample_prop())

cubist_spec <- 
  cubist_rules(committees = tune(), neighbors = tune()) %>% 
  set_engine("Cubist") 

# WORKFLOWSETS ----

date <- 
  workflow_set(
    preproc = list(date = recipe_spec), 
    models = list(
      PROPHET = prophet_spec,
      PROPHET_BOOST = prophet_boost_spec
    )
  ) %>% 
  option_add(param_info = prophet_param, id = "date_PROPHET") %>% 
  option_add(param_info = prophet_boost_param, id = "date_PROPHET_BOOST")

no_date <- 
  workflow_set(
    preproc = list(no_date = recipe_no_date_spec), 
    models = list(
      GLMNET        = linear_reg_spec,
      NNET          = nnet_spec,
      EARTH         = mars_spec,
      SVM_RBF       = svm_r_spec,
      SVM_POLY      = svm_p_spec,
      KNN           = knn_spec,
      DECISION_TREE = cart_spec,
      BAGGED_TREE   = bag_cart_spec,
      RANDOM_FOREST = rf_spec,
      XGBOOST       = xgb_spec,
      CUBIST        = cubist_spec
    )
  ) %>% 
  option_add(param_info = nnet_param, id = "no_date_NNET") %>% 
  option_add(param_info = xgb_param, id = "no_date_XGBOOST")

all_workflows <- 
  bind_rows(date, no_date) %>% 
  mutate(wflow_id = wflow_id %>% str_remove_all("(^date_)|^no_date_"))

doParallel::registerDoParallel(cores = 16)

grid_ctrl <-
  control_grid(
    save_pred = TRUE,
    parallel_over = "everything",
    save_workflow = TRUE
  )

grid_results <-
  all_workflows %>%
  workflow_map(
    seed = 1991,
    resamples = resamples_cv,
    grid = 30,
    control = grid_ctrl
  )

write_rds(grid_results, "00_models/grid_results.rds")

grid_results <- read_rds("00_models/grid_results.rds")

rank_results(grid_results) %>% 
  group_by(wflow_id) %>% 
  dplyr::slice(1) %>% 
  ungroup() %>% 
  arrange(mean)

g <- autoplot(
  grid_results,
  rank_metric = "rmse",  # <- how to order models
  metric = "rmse",       # <- which metric to visualize
  select_best = TRUE     # <- one point per workflow
) 

ggplotly(g)

g <- autoplot(grid_results, id = "NNET", metric = "rmse")

ggplotly(g)

grid_results %>% 
  filter(wflow_id == "NNET") %>% 
  rank_results()

best_results <- 
  map(grid_results$wflow_id, 
      ~ grid_results %>% 
        extract_workflow_set_result(.x) %>% 
        select_best(metric = "rmse"))




# * ACCURACY CHECK ----

submodels_tbl <- modeltime_table(
  grid_results %>% extract_workflow("PROPHET") %>% finalize_workflow(best_results[[1]]) %>% fit(training(splits)),
  grid_results %>% extract_workflow("PROPHET_BOOST") %>% finalize_workflow(best_results[[2]]) %>% fit(training(splits)),
  grid_results %>% extract_workflow("GLMNET") %>% finalize_workflow(best_results[[3]]) %>% fit(training(splits)),
  grid_results %>% extract_workflow("NNET") %>% finalize_workflow(best_results[[4]]) %>% fit(training(splits)),
  grid_results %>% extract_workflow("EARTH") %>% finalize_workflow(best_results[[5]]) %>% fit(training(splits)),
  grid_results %>% extract_workflow("SVM_RBF") %>% finalize_workflow(best_results[[6]]) %>% fit(training(splits)),
  grid_results %>% extract_workflow("SVM_POLY") %>% finalize_workflow(best_results[[7]]) %>% fit(training(splits)),
  grid_results %>% extract_workflow("KNN") %>% finalize_workflow(best_results[[8]]) %>% fit(training(splits)),
  grid_results %>% extract_workflow("DECISION_TREE") %>% finalize_workflow(best_results[[9]]) %>% fit(training(splits)),
  grid_results %>% extract_workflow("BAGGED_TREE") %>% finalize_workflow(best_results[[10]]) %>% fit(training(splits)),
  grid_results %>% extract_workflow("RANDOM_FOREST") %>% finalize_workflow(best_results[[11]]) %>% fit(training(splits)),
  grid_results %>% extract_workflow("XGBOOST") %>% finalize_workflow(best_results[[12]]) %>% fit(training(splits)),
  grid_results %>% extract_workflow("CUBIST") %>% finalize_workflow(best_results[[13]]) %>% fit(training(splits))
) %>% 
  update_model_description(6, "SVM - RBF") %>%
  update_model_description(7, "SVM - POLY") %>%
  update_model_description(9, "DECISION TREE") %>%
  update_model_description(10, "BAG TREE") %>% 
  update_model_description(11, "RANDOM FOREST")

write_rds(submodels_tbl, "00_models/submodels_tbl.rds")

submodels_tbl <- read_rds("00_models/submodels_tbl.rds")

submodels_calibration_tbl <- submodels_tbl %>% 
  modeltime_calibrate(testing(splits))

all_model_results <- submodels_calibration_tbl %>% 
  modeltime_accuracy() %>% 
  arrange(rmse)

best_model_list <- list(
  best_prophet_boost = grid_results %>% extract_workflow("PROPHET_BOOST") %>% finalize_workflow(best_results[[2]]),
  best_svm_rbf       = grid_results %>% extract_workflow("SVM_RBF") %>% finalize_workflow(best_results[[6]]), 
  best_knn           = grid_results %>% extract_workflow("KNN") %>% finalize_workflow(best_results[[8]]),
  best_bagged_tree   = grid_results %>% extract_workflow("BAGGED_TREE") %>% finalize_workflow(best_results[[10]]),
  best_random_forest = grid_results %>% extract_workflow("RANDOM_FOREST") %>% finalize_workflow(best_results[[11]]),
  best_xgboost       = grid_results %>% extract_workflow("XGBOOST") %>% finalize_workflow(best_results[[12]]), 
  best_cubist        = grid_results %>% extract_workflow("CUBIST") %>% finalize_workflow(best_results[[13]]) 
) 




write_rds(best_model_list, "00_models/best_model_list.rds")



