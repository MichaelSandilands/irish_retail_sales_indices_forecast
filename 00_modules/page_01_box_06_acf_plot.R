box_acf_plot_ui <- function(id) {
  
  ns <- NS(id)
  uiOutput(ns("box_acf_plot"))
  
}

box_acf_plot_server <- function(id, data, statistic_filter) {
  
  moduleServer(
    id,
    function(input, output, session) {
      
      output$box_acf_plot <- renderUI({
        
        ns <- session$ns
        
        choices_nace_group  <- data %>% pull(nace_group) %>% as.character() %>% unique()
        selected_nace_group <- choices_nace_group[1]
        
        box(
          title    = "ACF Plot",
          loadingState(),
          closable = FALSE,
          width    = 12,
          solidHeader = FALSE,
          collapsible = TRUE,
          sidebar = boxSidebar(
            id    = ns("acf_sidebar"),
            width = 40,
            startOpen = FALSE,
            inputId = ns("sidebar_acf_plot"),
            shinyWidgets::pickerInput(
              inputId  = ns("acf_plot_statistic"),
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
              inputId  = ns("acf_plot_group"),
              label    = "Group Selection",
              choices  = choices_nace_group,
              selected = selected_nace_group,
              multiple = FALSE,
              options = pickerOptions(
                actionsBox         = TRUE,
                container          = "body",
                dropdownAlignRight = TRUE
              )
            ),
            shinyWidgets::prettyToggle(
              inputId    = ns("acf_plot_log_trans"),
              label_on   = "Log Trans",
              label_off  = "No Log Trans",
              value      = FALSE
            )
          ),
          plotlyOutput(outputId = ns("acf_plot"), height = "600px")
        )
      })
      
      # ** Plotly ----
      output$acf_plot <- renderPlotly({
        
        req(input$acf_plot_statistic)
        
        rsi_acf_plot(
          data = data, statistic_filter = input$acf_plot_statistic,
          nace_group_filter = input$acf_plot_group, log_transform = input$acf_plot_log_trans
        )
        
      })
      
      observeEvent(input$sidebar_acf_plot, {
        updateBoxSidebar(ns("acf_sidebar"))
      })
      
    })
  
}
