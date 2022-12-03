box_future_forecast_plot_ui <- function(id) {
  
  ns <- NS(id)
  uiOutput(ns("box_test_plot"))
  
}

box_future_forecast_plot_server <- function(id, data) {
  
  moduleServer(
    id,
    function(input, output, session) {
      
      output$box_test_plot <- renderUI({
        
        if(is.null(data())) {
          return()
        }
        
        ns <- session$ns
        
        choices_statistic <- data()$statistic %>% as.character() %>% unique()
        selected_statistic <- choices_statistic[1]
        
        choices_nace_group <- data()$nace_group %>% as.character() %>% unique()
        selected_nace_group <- choices_nace_group[1:6]
        
        box(
          title       = "Time Plot",
          # loadingState(),
          closable    = FALSE,
          width       = 12,
          solidHeader = FALSE,
          collapsible = TRUE,
          sidebar     = boxSidebar(
            id        = ns("data_preprocessing_sidebar"),
            width     = 60,
            startOpen = FALSE,
            inputId   = ns("sidebar_data_preprocessing_plot"),
            shinyWidgets::pickerInput(
              inputId  = ns("data_preprocessing_plot_statistic"),
              label    = "Statistic Selection",
              choices  = choices_statistic,
              selected = selected_statistic,
              multiple = TRUE,
              options  = pickerOptions(
                actionsBox         = TRUE,
                container          = "body",
                selectedTextFormat = "count > 1",
                dropdownAlignRight = TRUE
              )
            ),
            shinyWidgets::pickerInput(
              inputId  = ns("data_preprocessing_plot_group"),
              label    = "Group Selection",
              choices  = choices_nace_group,
              selected = selected_nace_group,
              multiple = TRUE,
              options  = pickerOptions(
                actionsBox         = TRUE,
                container          = "body",
                selectedTextFormat = "count > 1",
                dropdownAlignRight = TRUE
              )
            ),
            shinyWidgets::prettyToggle(
              inputId   = ns("test_plot_conf"),
              label_on  = "Show Confidence Interval",
              label_off = "No Confidence Interval",
              value     = FALSE),
            numericInput(
              inputId = ns("data_preprocessing_plot_facet_ncol"),
              label   = "Facet Columns",
              value   = 2,
              min     = 1
            )
          ),
          
          plotlyOutput(outputId = ns("test_plot"), height = "720px")
          
        )
        
      })
      
      
      
      # ** Plotly ----
      output$test_plot <- renderPlotly({
        
        req(input$data_preprocessing_plot_statistic)
        
        palette_plot <- c("#333333", palette_msand()) %>% unname()
        
        g <- data() %>% 
          filter(statistic %in% input$data_preprocessing_plot_statistic) %>% 
          filter(nace_group %in% input$data_preprocessing_plot_group) %>% 
          group_by(statistic, nace_group) %>% 
          plot_modeltime_forecast(
            .conf_interval_show = input$test_plot_conf, 
            .facet_ncol         = input$data_preprocessing_plot_facet_ncol,
            .interactive        = FALSE
          ) +
          scale_color_manual(values = palette_plot) +
          labs(title = "") +
          theme_msand() +
          theme(
            strip.text.x = element_text(margin = margin(c(8,0,8,0))),
            panel.spacing.y = unit(1.5, "lines"),
            panel.grid.minor = NULL, 
            panel.grid.major = element_line(colour = alpha("black", 0.15))
          )
        
        ggplotly(g)
        
      })
      
      # ** Box Sidebar
      
      observeEvent(input$sidebar_time_plot, {
        updateBoxSidebar(ns("time_sidebar"))
      })
      
    })
  
}
