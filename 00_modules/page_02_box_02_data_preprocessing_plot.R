box_data_preprocessing_plot_ui <- function(id) {
  
  ns <- NS(id)
  uiOutput(ns("box_data_preprocessing_plot"))
  
}

box_data_preprocessing_plot_server <- function(id, data, statistic, nace_group) {
  stopifnot(is.reactive(data))
  moduleServer(
    id,
    function(input, output, session) {
      
      # * BOX - Time Plot ----
      # ** Box ----
      output$box_data_preprocessing_plot <- renderUI({
        
        ns <- session$ns
        
        choices_statistic <- statistic 
        selected_statistic <- choices_statistic[1]
        
        choices_nace_group <- nace_group 
        selected_nace_group <- choices_nace_group[1:6]
        
        box(
          title       = "Data Preprocessing Plot",
          loadingState(),
          closable    = FALSE,
          width       = 12,
          solidHeader = FALSE,
          collapsible = TRUE,
          sidebar     = boxSidebar(
            id        = ns("data_preprocessing_sidebar"),
            width     = 40,
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
            numericInput(
              inputId = ns("data_preprocessing_plot_facet_ncol"),
              label   = "Facet Columns",
              value   = 2,
              min     = 1
            )
          ),
          
          plotlyOutput(outputId = ns("data_preprocessing_plot"), height = "600px")
          
        )
        
      })
      
      
      
      # ** Plotly ----
      output$data_preprocessing_plot <- renderPlotly({
        
        
        rsi_anomaly_cleaning_plot(
          data              = data(),
          statistic_filter  = input$data_preprocessing_plot_statistic,
          nace_group_filter = input$data_preprocessing_plot_group,
          facet_ncol        = input$data_preprocessing_plot_facet_ncol
        )
        
      })
      
      # ** Box Sidebar
      
      observeEvent(input$sidebar_data_preprocessing_plot, {
        updateBoxSidebar(ns("data_preprocessing_sidebar"))
      })
      
    })
  
}
