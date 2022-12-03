box_test_set_ui <- function(id) {
  
  ns <- NS(id)
  uiOutput(ns("box_test_set"))
  
}

box_test_set_server <- function(id, data, rsi_monthly_tbl, 
                                clean_anomalies, log_transform, 
                                best_models) {
  
  # stopifnot(is.reactive(data))
  
  moduleServer(
    id,
    function(input, output, session) {
      
      rv <- reactiveValues()
      
      output$box_test_set <- renderUI({
        
        ns <- session$ns
        
        max_horizon <- rsi_monthly_tbl %>% 
          filter(statistic == "Retail Sales Index Value Unadjusted") %>% 
          filter(nace_group == "Motor trades (45)") %>% 
          filter(date >= "2021-06-01") %>% 
          nrow()
        
        box(
          title = "Evaluate Model Performance Using Train/Test Split",
          collapsible = TRUE,
          width = 12, 
          solidHeader = FALSE,
          fluidRow(
            column(
              width = 6,
              numericInputIcon(
                inputId = ns("test_horizon"),
                value   = 12,
                min     = 2,
                max     = max_horizon,
                label   = "Test Horizon",
                icon    = icon("chart-line")
              )
            ),
            column(
              style = "margin-top: 25px",
              width = 6,
              actionButton(
                inputId = ns("run_test_forecast"),
                label   = "Run Test",
                icon    = icon("cog"),
                class   = "pull-right"
              )
            )
          )
        )
      })
      
      observeEvent(input$run_test_forecast, {
        
        # req(input$test_horizon)
        
        if("name" %in% colnames(data())) {
          
          df <- data() %>% 
            pivot_wider(names_from = name, values_from = value) %>%
            mutate(value = ifelse(!is.na(`Value Clean`), `Value Clean`, Observed)) %>% 
            select(-Observed, -`Value Clean`)
          
        } else {
          
          df <- data()
          
        }
        
        rv$full_data_tbl <- rsi_data_engineering(df, forecast_horizon = input$test_horizon)
        
        rv$data_prepared_tbl <- rv$full_data_tbl %>%
          filter(!is.na(value)) %>% 
          drop_na()
        
        if(clean_anomalies()) {
          
          if(log_transform()) {
            
            rv$join_tbl <- rsi_monthly_tbl %>% mutate(value = log(value))
            
          } else {
            
            rv$join_tbl <- rsi_monthly_tbl
            
          }
          
          rv$actual_data_tbl <- rv$data_prepared_tbl %>%
            select(-value) %>%
            left_join(rv$join_tbl, by = c("statistic", "nace_group", "date"))
          
        } else {
          
          rv$actual_data_tbl <- rv$data_prepared_tbl
          
        }
        
        rv$future_tbl <- rv$full_data_tbl %>%
          filter(is.na(value))
        
        rv$splits <- rv$data_prepared_tbl %>%
          time_series_split(date, assess = input$test_horizon, cumulative = TRUE)
        
        rv$recipe_spec <- recipe(value ~ ., data = training(rv$splits)) %>%
          step_timeseries_signature(date) %>%
          step_rm(matches("(.xts$)|(.iso$)|(week)|(day)|(hour)|(minute)|(second)|(am.pm)")) %>%
          step_normalize(date_index.num, date_year) %>%
          step_dummy(all_nominal_predictors(), one_hot = TRUE) %>%
          step_zv(all_predictors())
        
        rv$fitted_wflw_list <- list(
          prophet_boost = best_models[[1]] %>%
            update_recipe(rv$recipe_spec) %>%
            fit(training(rv$splits))
        ) %>%
          append(
            best_models[2:7] %>%
              map(~ .x %>%
                    update_recipe(rv$recipe_spec %>% step_rm(date)) %>%
                    fit(training(rv$splits))
              )
          )
        
        rv$submodels_tbl <- as_modeltime_table(rv$fitted_wflw_list)
        
        rv$modeltime_ensemble_tbl <- modeltime_table(
          ensemble_average(rv$submodels_tbl)
        )
        
        rv$full_modeltime_tbl <- rv$modeltime_ensemble_tbl %>% 
          combine_modeltime_tables(rv$submodels_tbl) %>%
          modeltime_calibrate(testing(rv$splits))
        
        if(log_transform()) {
          
          rv$full_accuracy_tbl <- rv$full_modeltime_tbl %>%
            mutate(.calibration_data = map(.calibration_data, .f = function(tbl) {
              tbl %>%
                mutate(
                  .actual     = exp(.actual),
                  .prediction = exp(.prediction),
                  .residuals  = .actual - .prediction
                )
            })) %>%
            modeltime_accuracy() %>% 
            mutate(across(mae:rsq, ~ round(.x, 2)))
          
        } else {
          
          rv$full_accuracy_tbl <- rv$full_modeltime_tbl %>%
            modeltime_accuracy() %>% 
            mutate(across(mae:rsq, ~ round(.x, 2)))
          
        }
        
        if(log_transform()) {
          
          rv$test_forecast_tbl <- rv$full_modeltime_tbl %>%
            modeltime_forecast(
              new_data    = testing(rv$splits),
              actual_data = rv$actual_data_tbl,
              keep_data   = TRUE
            ) %>%
            mutate(
              across(.cols = c(.value, .conf_lo, .conf_hi),
                     .fns  = function(x) exp(x))
            )
          
        } else {
          
          rv$test_forecast_tbl <- rv$full_modeltime_tbl %>%
            modeltime_forecast(
              new_data    = testing(rv$splits),
              actual_data = rv$actual_data_tbl,
              keep_data   = TRUE
            )
          
        }
        
        
      }, ignoreNULL = TRUE)
      
      return(
        list(
          test_forecast_tbl = reactive({ rv$test_forecast_tbl }),
          test_accuracy_tbl = reactive({ rv$full_accuracy_tbl })
        )
      )
      
    })
  
}
