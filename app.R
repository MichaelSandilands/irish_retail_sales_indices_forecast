# Plotting
library(plotly)
library(reactable)
library(msand)

# Shiny
library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyEffects)
library(shinyjs)

# Modeling
library(tidymodels)
library(modeltime)
library(modeltime.ensemble)
library(rules)
library(baguette)

# Data
library(csodata)

# Time Series
library(timetk)
library(lubridate)

# Core
library(tidyverse)

# Setup & Functions
source("00_scripts/01_import_and_clean.R")
source("00_scripts/02_visualizations.R")
source("00_scripts/03_modeling.R")

# Modules
list.files("00_modules/") %>% 
  map(~ source(str_c("00_modules/", .x)))

rsi_monthly_tbl <- rsi_import_and_clean("RSM05")

best_model_list <- read_rds("00_models/best_model_list.rds")

# ---- 1.0 UI ----
ui <- dashboardPage(
  
  title = "Sales Index Forecast",
  options = list(
    sidebarExpandOnHover = FALSE
  ), 
  # 1.1 Header ----
  header = dashboardHeader(
    title = tagList(
      span(class = "logo-lg", "Index Forecast")
    )
  ),
  # 1.2 Sidebar ----
  sidebar = dashboardSidebar(
    id = "sidebar_1",
    minified   = TRUE,
    collapsed  = FALSE,
    sidebarMenu(
      menuItem(
        text       = "Data Explorer",
        tabName    = "data_explorer",
        badgeLabel = "EDA",
        badgeColor = "light-blue",
        icon       = icon("chart-line")
      ),
      menuItem(
        text       = "Auto Forecast",
        tabName    = "auto_forecast",
        badgeLabel = "MODEL",
        badgeColor = "light-blue",
        icon       = icon("cogs")
      )
    )
  ),
  # 1.3 Control Bar ----
  controlbar = dashboardControlbar(
    disable = TRUE
  ),
  # 1.4 Dashboard Body ----
  body = dashboardBody(
    useShinyjs(),
    tags$head(
      tags$link(
        rel = "stylesheet",
        type = "text/css",
        href = "styles-msand.css"
      )
    ),
    tabItems(
      # * TS Explorer ----
      tabItem(
        tabName = "data_explorer",
        h1("TS Explorer"),
        fluidRow(
          column(
            width = 6,
            box_summary_ui("summary_module"),
            box_summary_tbl_ui("summary_tbl_module"),
            box_seasonality_plot_ui("seasonality_plot_module"),
            box_stl_plot_ui("stl_plot_module")
          ),
          column(
            width = 6,
            box_statistic_picker_ui("statistic_module"),
            box_time_plot_ui("time_plot_module"),
            box_acf_plot_ui("acf_plot_module"),
            box_anomaly_plot_ui("anomaly_plot_module")
          )
        )
      ),
      # * Auto Forecast ----
      tabItem(
        tabName = "auto_forecast",
        h1("Auto Forecast"),
        fluidRow(
          column(
            width = 4,
            box_data_preprocessing_ui("data_preprocessing_module")
          ),
          column(
            width = 8,
            box_data_preprocessing_plot_ui("data_preprocessing_plot_module")
          ),
          column(
            width = 4,
            box_test_set_ui("test_set_module"),
            box_test_metrics_ui("test_metrics_module")
          ),
          column(
            width = 8,
            box_test_forecast_ui("test_plot_module")
          )
        ),
        fluidRow(
          column(
            width = 4,
            box_future_forecast_ui("future_module")
          ),
          column(
            width = 8,
            box_future_forecast_plot_ui("future_plot_module")
          )
        )
      )
    )
  )
)



server <- function(session, input, output) {
  
  # *** PAGE 1 - TS EXPLORER *** ----
  
  # * BOX 1 - Summary 
  box_summary_server(
    id   = "summary_module",
    data = rsi_monthly_tbl
  )
  
  # * BOX 2 - Statistic Picker 
  statistic_filter <- box_statistic_picker_server(
    id   = "statistic_module",
    data = rsi_monthly_tbl
  )
  
  # * BOX 3 - Summary Table 
  box_summary_tbl_server(
    id               = "summary_tbl_module",
    data             = rsi_monthly_tbl,
    statistic_filter = statistic_filter
  )
  
  # * BOX 4 - Time Plot 
  box_time_plot_server(
    id               = "time_plot_module",
    data             = rsi_monthly_tbl,
    statistic_filter = statistic_filter
  )
  
  # * BOX 5 - Seasonality Plot 
  box_seasonality_plot_server(
    id               = "seasonality_plot_module",
    data             = rsi_monthly_tbl,
    statistic_filter = statistic_filter
  )
  
  # * BOX 6 - ACF Plot 
  box_acf_plot_server(
    id               = "acf_plot_module",
    data             = rsi_monthly_tbl,
    statistic_filter = statistic_filter
  )
  
  # * BOX 7 - STL Plot 
  box_stl_plot_server(
    id               = "stl_plot_module",
    data             = rsi_monthly_tbl,
    statistic_filter = statistic_filter
  )
  
  # * BOX 8 - Anomaly Plot 
  box_anomaly_plot_server(
    id               = "anomaly_plot_module",
    data             = rsi_monthly_tbl,
    statistic_filter = statistic_filter
  )
  
  
  # *** PAGE 2 - AUTO FORECAST *** ----
  
  # * Box 1 - Data Preprocessing ----
  data_preproc <- box_data_preprocessing_server(
    "data_preprocessing_module",
    rsi_monthly_tbl 
  )
  
  # * Box 2 - Data Preprocessing Plot ----
  box_data_preprocessing_plot_server(
    "data_preprocessing_plot_module",
    data       = data_preproc$processed_data,
    statistic  = rsi_monthly_tbl$statistic %>% as.character() %>% unique(),
    nace_group = rsi_monthly_tbl$nace_group %>% as.character() %>% unique()
  )
  
  # * Box 3 - Run Test Forecast ----
  test_forecast <- box_test_set_server(
    "test_set_module",
    data            = data_preproc$processed_data,
    best_models     = best_model_list,
    rsi_monthly_tbl = rsi_monthly_tbl,
    clean_anomalies = data_preproc$clean_anomalies_switch,
    log_transform   = data_preproc$log_transform_switch
  )
  
  # * Box 4 - Test Forecast Plot ----
  box_test_forecast_server(
    "test_plot_module",
    test_forecast$test_forecast_tbl
  )
  
  # * Box 5 - Test Forecast Metrics
  box_test_metrics_server(
    "test_metrics_module",
    test_forecast$test_accuracy_tbl
  )
  
  # * Box 6 - Forecast Future Data ----
  future_data <- box_future_forecast_server(
    "future_module",
    data            = data_preproc$processed_data,
    best_models     = best_model_list,
    rsi_monthly_tbl = rsi_monthly_tbl,
    clean_anomalies = data_preproc$clean_anomalies_switch,
    log_transform   = data_preproc$log_transform_switch
  )
  
  # * Box 7 - Future Data Plot ----
  box_future_forecast_plot_server(
    "future_plot_module",
    data = future_data$future_forecast_tbl
  )
  
  
}


# Run the application
shinyApp(ui = ui, server = server)





