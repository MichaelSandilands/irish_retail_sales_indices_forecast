# IRELAND RETAIL SALES INDEX SHINY APP ---- 
# Visualizations ----

library(tidyverse)
library(timetk)
library(csodata)
library(msand)
library(plotly)
library(lubridate)

source("00_scripts/01_import_and_clean.R")

rsi_monthly_tbl <- rsi_import_and_clean("RSM05") 


rsi_monthly_tbl 

# SUMMARY TABLE ----

rsi_monthly_tbl %>%  
  group_by(statistic, nace_group) %>% 
  tk_summary_diagnostics() %>% 
  select(statistic, nace_group, n.obs, start, end, scale) %>% 
  rename(`# OBS` = n.obs) %>% 
  rename_with(.fn = ~ str_to_upper(.x) %>% str_replace_all("_", " "))

rsi_time_series_summary <- function(data, statistic_filter) {
  
  data %>%  
    filter(statistic %in% statistic_filter) %>% 
    group_by(statistic, nace_group) %>% 
    tk_summary_diagnostics() %>% 
    ungroup() %>% 
    select(statistic, nace_group, n.obs, start, end, scale) %>% 
    rename(`# OBS` = n.obs) %>% 
    rename_with(.fn = ~ str_to_upper(.x) %>% str_replace_all("_", " "))
  
}

# TIME PLOT ----

g <- rsi_monthly_tbl %>% 
  filter(str_detect(statistic, "Value")) %>%
  group_by(statistic, nace_group) %>% 
  plot_time_series(
    date, value, .facet_ncol = 2, .color_var = data, .smooth = FALSE, .interactive = FALSE
  ) +
  theme_msand() +
  scale_color_manual(values = c("observed" = "#333333", "value_clean" = "#DD8925")) +
  theme(
    panel.grid.minor   = NULL, 
    panel.grid.major   = element_line(colour = alpha("black", 0.15)), 
    strip.background.y = ele
  ) +
  labs(title = "")

ggplotly(g)


rsi_plot_time_series <- 
  function(
    data, log_transform = FALSE,
    statistic_filter, nace_group_filter,
    facet_ncol = 2, smooth = FALSE, smooth_span = 0.75
  ) {
    
    if(log_transform) {
      
      data <- data %>% 
        mutate(value = log(value))
      
    }
    
    g <- data %>% 
      filter(statistic %in% statistic_filter) %>% 
      filter(nace_group %in% nace_group_filter) %>% 
      group_by(statistic, nace_group) %>% 
      plot_time_series(
        date, value, .facet_ncol = facet_ncol, 
        .smooth = smooth, .smooth_color = "#FAD25A", .smooth_span = smooth_span, 
        .line_color = "#333333", .interactive = FALSE
      ) +
      theme_msand() +
      theme(
        strip.text.x = element_text(margin = margin(c(8,0,8,0))),
        panel.spacing.y = unit(1.5, "lines"),
        panel.grid.minor = NULL, 
        panel.grid.major = element_line(colour = alpha("black", 0.15))
      ) +
      labs(title = "")
    
    ggplotly(g)
    
  }

statistic_filter <- "Retail Sales Index Value Unadjusted"

nace_group_filter <- unique(rsi_monthly_tbl$nace_group)

rsi_monthly_tbl %>% 
  rsi_plot_time_series(statistic = statistic_filter, nace_group = nace_group_filter, log_transform = TRUE, smooth = TRUE)

# SEASONALITY PLOT ----

rsi_monthly_tbl %>% 
  filter(data == "observed") %>% 
  filter(nace_group == "Motor trades (45)") %>%
  select(date, value) %>% 
  plot_seasonal_diagnostics(date, value, .geom_color = "#333333")

rsi_seasonality_plot <- function(data, statistic_filter, nace_group_filter, log_transform = FALSE, feature_set = c("month.lbl", "quarter", "year")) {
  
  if(log_transform) {
    
    data <- data %>% 
      mutate(value = log(value))
    
  }
  
  g <- data %>% 
    filter(statistic == statistic_filter) %>% 
    filter(nace_group == nace_group_filter) %>%
    select(nace_group, date, value) %>% 
    group_by(nace_group) %>% 
    plot_seasonal_diagnostics(date, value, .interactive = FALSE, .geom_color = "#333333", .feature_set = feature_set) +
    theme_msand() +
    theme(
      strip.text.x = element_text(margin = margin(c(8,0,8,0))),
      panel.spacing.y = unit(1.5, "lines"),
      panel.grid.minor = NULL, 
      panel.grid.major = element_line(colour = alpha("black", 0.15))
    ) +
    labs(title = "")
  
  ggplotly(g)
  
}

rsi_monthly_tbl %>% 
  rsi_seasonality_plot(statistic_filter = "Retail Sales Index Value Unadjusted", 
                       nace_group_filter = "Motor trades (45)")

# ANOMALY PLOT ----

rsi_anomaly_plot <- function(data, statistic_filter, nace_group_filter, alpha = 0.1,
                             log_transform = FALSE, facet_ncol = 2) {
  
  if(log_transform) {
    
    data <- data %>% 
      mutate(value = log(value))
    
  }
  
  g <- data %>% 
    filter(statistic %in% statistic_filter) %>% 
    filter(nace_group %in% nace_group_filter) %>%
    select(statistic, nace_group, date, value) %>% 
    group_by(statistic, nace_group) %>% 
    plot_anomaly_diagnostics(
      date, value, .interactive = FALSE, 
      .message = FALSE, .line_color = "#333333", 
      .alpha = alpha, .anom_color = "#FAD25A", .facet_ncol = facet_ncol
    ) +
    theme_msand() +
    theme(
      strip.text.x = element_text(margin = margin(c(8,0,8,0))),
      panel.spacing.y = unit(1.5, "lines"),
      panel.grid.minor = NULL, 
      panel.grid.major = element_line(colour = alpha("black", 0.15))
    ) +
    labs(title = "")
  
  ggplotly(g)
  
}

rsi_monthly_tbl %>% 
  rsi_anomaly_plot(statistic_filter = "Retail Sales Index Value Unadjusted", 
                   nace_group_filter = nace_group_filter)

# ACF PLOT

rsi_acf_plot <- function(data, statistic_filter, nace_group_filter, log_transform = FALSE) {
  
  if(log_transform) {
    
    data <- data %>% 
      mutate(value = log(value))
    
  }
  
  g <- data %>% 
    filter(statistic == statistic_filter) %>% 
    filter(nace_group == nace_group_filter) %>%
    select(nace_group, date, value) %>% 
    group_by(nace_group) %>% 
    plot_acf_diagnostics(date, value, .interactive = FALSE, .point_color = "#FAD25A",
                         .line_color = "#333333", .white_noise_line_color = "#FAD25A") +
    theme_msand() +
    theme(
      strip.text.x = element_text(margin = margin(c(8,0,8,0))),
      panel.spacing.y = unit(1.5, "lines"),
      panel.grid.minor = NULL, 
      panel.grid.major = element_line(colour = alpha("black", 0.15))
    ) +
    labs(title = "")
  
  ggplotly(g)
  
}

rsi_monthly_tbl %>% 
  rsi_acf_plot(statistic_filter = "Retail Sales Index Value Unadjusted", 
               nace_group_filter = "Motor trades (45)")

# STL DECOMPOSITION PLOT ----

rsi_stl_plot <- function(data, statistic_filter, nace_group_filter, log_transform = FALSE,
                         feature_set = c("observed", "season", "trend", "remainder", "seasadj")) {
  
  if(log_transform) {
    
    data <- data %>% 
      mutate(value = log(value))
    
  }
  
  g <- data %>%  
    filter(statistic == statistic_filter) %>% 
    filter(nace_group == nace_group_filter) %>%
    select(nace_group, date, value) %>% 
    group_by(nace_group) %>% 
    plot_stl_diagnostics(date, value, .interactive = FALSE, .line_color = "#333333", .message = FALSE, 
                         .feature_set = feature_set) +
    theme_msand() +
    theme(
      strip.text.x = element_text(margin = margin(c(8,0,8,0))),
      panel.spacing.y = unit(1.5, "lines"),
      panel.grid.minor = NULL, 
      panel.grid.major = element_line(colour = alpha("black", 0.15))
    ) +
    labs(title = "")
  
  ggplotly(g)
  
}

rsi_monthly_tbl %>% 
  rsi_stl_plot(statistic_filter = "Retail Sales Index Value Unadjusted", 
               nace_group_filter = "Motor trades (45)")


rsi_monthly_tbl %>% 
  rsi_clean_anomalies() %>% 
  filter(!(name == "value_clean" & (date < "2020-01-01" | date >= "2021-06-01"))) %>% 
  mutate(name = name %>% str_replace_all("_", " ") %>% str_to_title()) %>% 
  pivot_wider(names_from = name, values_from = value) %>%
  mutate(value = ifelse(!is.na(`Value Clean`), `Value Clean`, Observed)) %>% 
  select(-Observed, -`Value Clean`) %>% skimr::skim()





rsi_anomaly_cleaning_plot <- function(data, statistic_filter, nace_group_filter,
                                      facet_ncol = 2) {
  
  data <- data %>% 
    filter(statistic %in% statistic_filter) %>% 
    filter(nace_group %in% nace_group_filter) %>% 
    group_by(statistic, nace_group)
  
  
  if("name" %in% colnames(data)) {
    
    g <- data %>% 
      plot_time_series(
        date, value, 
        .color_var = name, 
        .smooth = FALSE, 
        .interactive = FALSE,
        .facet_ncol = facet_ncol
      ) +
      scale_color_manual(values = c("Observed" = "#333333", "Value Clean" = "#FAD25A")) 
    
  } else {
    
    g <- data %>% 
      plot_time_series(
        date, value, 
        .smooth      = FALSE, 
        .line_color  = "#333333",
        .interactive = FALSE,
        .facet_ncol  = facet_ncol
      )
    
  }
  
  g <- g +
    theme_msand() +
    theme(
      strip.text.x = element_text(margin = margin(c(8,0,8,0))),
      panel.spacing.y = unit(1.5, "lines"),
      panel.grid.minor = NULL, 
      panel.grid.major = element_line(colour = alpha("black", 0.15))
    ) +
    labs(title = "")
  
  ggplotly(g)
  
}

rsi_anomaly_cleaning_plot(rsi_monthly_tbl, statistic_filter = rsi_monthly_tbl$statistic, 
                          nace_group_filter = c("Motor trades (45)", "Department stores (4719)"))

dump(
  c(
    "rsi_time_series_summary", "rsi_acf_plot", "rsi_anomaly_plot", 
    "rsi_plot_time_series", "rsi_seasonality_plot", 
    "rsi_stl_plot", "rsi_anomaly_cleaning_plot"
  ),
  "00_scripts/02_visualizations.R"
)

