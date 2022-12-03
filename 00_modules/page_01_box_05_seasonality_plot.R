box_seasonality_plot_ui <- function(id) {
  
  ns <- NS(id)
  uiOutput(ns("box_seasonality_plot"))
  
}

box_seasonality_plot_server <- function(id, data, statistic_filter) {
  
  moduleServer(
    id,
    function(input, output, session) {
      
      output$box_seasonality_plot <- renderUI({
        
        ns <- session$ns
        
        choices_nace_group  <- data %>% pull(nace_group) %>% as.character() %>% unique()
        selected_nace_group <- choices_nace_group[1]
        
        box(
          title    = "Seasonality Plot",
          loadingState(),
          closable = FALSE,
          width    = 12,
          solidHeader = FALSE,
          collapsible = TRUE,
          sidebar = boxSidebar(
            id    = ns("seasonality_sidebar"),
            width = 40,
            startOpen = FALSE,
            inputId = ns("sidebar_seasonality_plot"),
            shinyWidgets::pickerInput(
              inputId  = ns("seasonality_plot_statistic"),
              label    = "Statistic Selection",
              choices  = statistic_filter(),
              selected = statistic_filter()[1],
              multiple = FALSE,
              options = pickerOptions(
                container = "body", 
                dropdownAlignRight = TRUE
              )
            ),
            shinyWidgets::pickerInput(
              inputId  = ns("seasonality_plot_group"),
              label    = "Group Selection",
              choices  = choices_nace_group,
              selected = selected_nace_group,
              multiple = FALSE,
              options = pickerOptions(
                container = "body", 
                dropdownAlignRight = TRUE
              )
            ),
            shinyWidgets::prettyToggle(
              inputId    = ns("seasonality_plot_log_trans"),
              label_on   = "Log Trans",
              label_off  = "No Log Trans",
              value      = FALSE
            ),
            shinyWidgets::pickerInput(
              inputId  = ns("seasonality_plot_features"),
              label    = "Feature Selection",
              choices  = c("month.lbl", "quarter", "year"),
              selected = c("month.lbl", "quarter", "year"),
              multiple = TRUE,
              options = pickerOptions(
                container = "body", 
                dropdownAlignRight = TRUE,
                actionsBox = TRUE, 
                selectedTextFormat = "count > 1"
              )
            )
          ),
          plotlyOutput(outputId = ns("seasonality_plot"), height = "600px")
        )
      })
      
      # ** Plotly ----
      output$seasonality_plot <- renderPlotly({
        
        req(input$seasonality_plot_statistic)
        
        rsi_seasonality_plot(
          data = data, 
          statistic_filter = input$seasonality_plot_statistic, 
          feature_set = input$seasonality_plot_features,
          nace_group_filter = input$seasonality_plot_group, 
          log_transform = input$seasonality_plot_log_trans
        )
        
      })
      
      observeEvent(input$sidebar_seasonality_plot, {
        updateBoxSidebar(ns("seasonality_sidebar"))
      })
      
    })
  
}
