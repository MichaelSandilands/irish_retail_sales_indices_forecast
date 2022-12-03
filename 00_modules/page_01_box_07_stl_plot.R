box_stl_plot_ui <- function(id) {
  
  ns <- NS(id)
  uiOutput(ns("box_stl_plot"))
  
}

box_stl_plot_server <- function(id, data, statistic_filter) {
  
  moduleServer(
    id,
    function(input, output, session) {
      
      output$box_stl_plot <- renderUI({
        
        ns <- session$ns
        
        choices_nace_group  <- data %>% pull(nace_group) %>% as.character() %>% unique()
        selected_nace_group <- choices_nace_group[1]
        
        box(
          title    = "STL Plot",
          loadingState(),
          closable = FALSE,
          width    = 12,
          solidHeader = FALSE,
          collapsible = TRUE,
          sidebar = boxSidebar(
            id    = ns("stl_sidebar"),
            width = 40,
            startOpen = FALSE,
            inputId = ns("sidebar_stl_plot"),
            shinyWidgets::pickerInput(
              inputId  = ns("stl_plot_statistic"),
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
              inputId  = ns("stl_plot_group"),
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
              inputId    = ns("stl_plot_log_trans"),
              label_on   = "Log Trans",
              label_off  = "No Log Trans",
              value      = FALSE
            ),
            shinyWidgets::pickerInput(
              inputId  = ns("stl_plot_features"),
              label    = "Feature Selection",
              choices  = c("observed", "season", "trend", "remainder", "seasadj"),
              selected = c("observed", "season", "trend", "remainder", "seasadj"),
              multiple = TRUE,
              options = pickerOptions(
                container = "body", 
                dropdownAlignRight = TRUE,
                actionsBox = TRUE, 
                selectedTextFormat = "count > 1"
              )
            )
          ),
          plotlyOutput(outputId = ns("stl_plot"), height = "600px")
        )
      })
      
      # ** Plotly ----
      output$stl_plot <- renderPlotly({
        
        req(input$stl_plot_statistic)
        
        rsi_stl_plot(
          data = data, statistic_filter = input$stl_plot_statistic,
          nace_group_filter = input$stl_plot_group, log_transform = input$stl_plot_log_trans, 
          feature_set = input$stl_plot_features
        )
        
      })
      
      observeEvent(input$sidebar_stl_plot, {
        updateBoxSidebar(ns("stl_sidebar"))
      })
      
    })
  
}
