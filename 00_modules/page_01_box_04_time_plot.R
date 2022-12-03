box_time_plot_ui <- function(id) {
  
  ns <- NS(id)
  uiOutput(ns("box_time_plot"))
  
}

box_time_plot_server <- function(id, data, statistic_filter) {
  
  moduleServer(
    id,
    function(input, output, session) {
      
      # * BOX - Time Plot ----
      # ** Box ----
      output$box_time_plot <- renderUI({
        
        ns <- session$ns
        
        choices <- data %>% pull(nace_group) %>% as.character() %>% unique()
        selected <- choices[1:6]
        
        box(
          title       = "Time Plot",
          loadingState(),
          closable    = FALSE,
          width       = 12,
          solidHeader = FALSE,
          collapsible = TRUE,
          sidebar     = boxSidebar(
            id        = ns("time_sidebar"),
            width     = 40,
            startOpen = FALSE,
            inputId   = ns("sidebar_time_plot"),
            shinyWidgets::pickerInput(
              inputId  = ns("time_plot_group"),
              label    = "Group Selection",
              choices  = choices,
              selected = selected,
              multiple = TRUE,
              options  = pickerOptions(
                actionsBox         = TRUE,
                container          = "body",
                selectedTextFormat = "count > 1",
                dropdownAlignRight = TRUE
              )
            ),
            shinyWidgets::prettyToggle(
              inputId   = ns("time_plot_log_trans"),
              label_on  = "Log Trans",
              label_off = "No Log Trans",
              value     = FALSE),
            shinyWidgets::prettyToggle(
              inputId   = ns("time_plot_smooth"),
              label_on  = "Show Smoother",
              label_off = "No Smoother",
              value     = TRUE),
            shiny::sliderInput(
              inputId = ns("time_plot_smooth_span"),
              label   = "Smooth Span",
              value   = 0.75,
              min     = 0.05,
              max     = 1
            ),
            numericInput(
              inputId = ns("time_plot_facet_ncol"),
              label   = "Facet Columns",
              value   = 2,
              min     = 1
            )
          ),
          
          plotlyOutput(outputId = ns("time_plot"), height = "600px")
          
        )
        
      })
      
      # ** Plotly ----
      output$time_plot <- renderPlotly({
        
        req(!is.null(statistic_filter()))
        
        rsi_plot_time_series(
          data              = data,
          log_transform     = input$time_plot_log_trans,
          statistic_filter  = statistic_filter(),
          nace_group_filter = input$time_plot_group,
          facet_ncol        = input$time_plot_facet_ncol,
          smooth            = input$time_plot_smooth,
          smooth_span       = input$time_plot_smooth_span
        )
        
      })
      
      # ** Box Sidebar
      
      observeEvent(input$sidebar_time_plot, {
        updateBoxSidebar(ns("time_sidebar"))
      })
      
    })
  
}
