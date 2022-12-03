box_future_forecast_ui <- function(id) {
  
  ns <- NS(id)
  uiOutput(ns("box_test_set"))
  
}

box_future_forecast_server <- function(id, data, rsi_monthly_tbl, 
                                       clean_anomalies, log_transform, 
                                       best_models) {
  
  moduleServer(
    id,
    function(input, output, session) {
      
      rv <- reactiveValues()
      
      output$box_test_set <- renderUI({
        
        ns <- session$ns
        
        max_horizon <- 24
        
        box(
          title = "Forecast Future Values",
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
                label   = "Forecast Horizon",
                icon    = icon("chart-line")
              )
            ),
            column(
              style = "margin-top: 25px",
              width = 6,
              actionButton(
                inputId = ns("run_test_forecast"),
                label   = "Run Forecast",
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
        
        rv$recipe_spec <- recipe(value ~ ., data = rv$data_prepared_tbl) %>%
          step_timeseries_signature(date) %>%
          step_rm(matches("(.xts$)|(.iso$)|(week)|(day)|(hour)|(minute)|(second)|(am.pm)")) %>%
          step_normalize(date_index.num, date_year) %>%
          step_dummy(all_nominal_predictors(), one_hot = TRUE) %>%
          step_zv(all_predictors())
        
        rv$fitted_wflw_list <- list(
          prophet_boost = best_models[[1]] %>%
            update_recipe(rv$recipe_spec) %>%
            fit(rv$data_prepared_tbl)
        ) %>%
          append(
            best_models[2:7] %>%
              map(~ .x %>%
                    update_recipe(rv$recipe_spec %>% step_rm(date)) %>%
                    fit(rv$data_prepared_tbl)
              )
          )
        
        rv$submodels_tbl <- as_modeltime_table(rv$fitted_wflw_list)
        
        rv$modeltime_ensemble_tbl <- modeltime_table(
          ensemble_average(rv$submodels_tbl)
        )
        
        rv$full_modeltime_tbl <- rv$modeltime_ensemble_tbl %>% 
          combine_modeltime_tables(rv$submodels_tbl)
        
        print("we made it this far")
        
        if(log_transform()) {
          
          rv$future_forecast_tbl <- rv$full_modeltime_tbl %>%
            modeltime_forecast(
              new_data    = rv$future_tbl,
              actual_data = rv$actual_data_tbl,
              keep_data   = TRUE
            ) %>%
            mutate(
              across(.cols = c(.value),
                     .fns  = function(x) exp(x))
            )
          
        } else {
          
          rv$future_forecast_tbl <- rv$full_modeltime_tbl %>%
            modeltime_forecast(
              new_data    = rv$future_tbl,
              actual_data = rv$actual_data_tbl,
              keep_data   = TRUE
            )
          
        }
        
        
      }, ignoreNULL = TRUE)
      
      return(
        list(
          future_forecast_tbl = reactive({ rv$future_forecast_tbl })
        )
      )
      
    })
  
}
