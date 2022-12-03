box_anomaly_plot_ui <- function(id) {
  
  ns <- NS(id)
  uiOutput(ns("box_anomaly_plot"))
  
}

box_anomaly_plot_server <- function(id, data, statistic_filter) {
  
  moduleServer(
    id,
    function(input, output, session) {
      
      output$box_anomaly_plot <- renderUI({
        
        ns <- session$ns
        
        choices_nace_group  <- data %>% pull(nace_group) %>% as.character() %>% unique()
        selected_nace_group <- choices_nace_group[1:6]
        
        box(
          title    = "Anomaly Plot",
          loadingState(),
          closable = FALSE,
          width    = 12,
          solidHeader = FALSE,
          collapsible = TRUE,
          sidebar = boxSidebar(
            id    = ns("anomaly_sidebar"),
            width = 40,
            startOpen = FALSE,
            inputId = ns("sidebar_anomaly_plot"),
            shinyWidgets::pickerInput(
              inputId  = ns("anomaly_plot_group"),
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
              inputId    = ns("anomaly_plot_log_trans"),
              label_on   = "Log Trans",
              label_off  = "No Log Trans",
              value      = FALSE),
            shiny::sliderInput(
              inputId = ns("anomaly_plot_alpha"),
              label = "Anomaly Alpha", 
              value = 0.1, 
              min = 0, 
              max = 1
            ),
            numericInput(
              inputId = ns("anomaly_plot_facet_ncol"), 
              label = "Facet Columns",
              value = 2, 
              min = 1
            )
          ),
          plotlyOutput(outputId = ns("anomaly_plot"), height = "600px")
        )
      })
      
      # ** Plotly ----
      output$anomaly_plot <- renderPlotly({
        
        rsi_anomaly_plot(
          data              = data, 
          statistic_filter  = statistic_filter(), 
          alpha             = input$anomaly_plot_alpha,
          nace_group_filter = input$anomaly_plot_group, 
          log_transform     = input$anomaly_plot_log_trans
        )
        
      })
      
      observeEvent(input$sidebar_anomaly_plot, {
        updateBoxSidebar(ns("anomaly_sidebar"))
      })
      
    })
  
}
