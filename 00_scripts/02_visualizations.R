rsi_time_series_summary <-
function(data, statistic_filter) {
  
  data %>%  
    filter(statistic %in% statistic_filter) %>% 
    group_by(statistic, nace_group) %>% 
    tk_summary_diagnostics() %>% 
    ungroup() %>% 
    select(statistic, nace_group, n.obs, start, end, scale) %>% 
    rename(`# OBS` = n.obs) %>% 
    rename_with(.fn = ~ str_to_upper(.x) %>% str_replace_all("_", " "))
  
}
rsi_acf_plot <-
function(data, statistic_filter, nace_group_filter, log_transform = FALSE) {
  
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
rsi_anomaly_plot <-
function(data, statistic_filter, nace_group_filter, alpha = 0.1,
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
rsi_seasonality_plot <-
function(data, statistic_filter, nace_group_filter, log_transform = FALSE, feature_set = c("month.lbl", "quarter", "year")) {
  
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
rsi_stl_plot <-
function(data, statistic_filter, nace_group_filter, log_transform = FALSE,
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
rsi_anomaly_cleaning_plot <-
function(data, statistic_filter, nace_group_filter,
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
