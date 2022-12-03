box_summary_tbl_ui <- function(id) {
  
  ns <- NS(id)
  uiOutput(ns("box_summary_tbl"))
  
}

box_summary_tbl_server <- function(id, data, statistic_filter) {
  
  moduleServer(
    id,
    function(input, output, session) {
      
      output$box_summary_tbl <- renderUI({
        
        ns <- session$ns
        
        box(
          title    = "Retail Sales Index Group Summary",
          loadingState(),
          closable = FALSE,
          width    = 12,
          solidHeader = FALSE,
          collapsible = TRUE,
          reactableOutput(outputId = ns("summary_tbl"), height = "600px")
        )
        
      })
      
      # ** Plotly ----
      output$summary_tbl <- renderReactable({
        
        reactable(
          data = rsi_time_series_summary(data, statistic_filter()),
          searchable = TRUE
        )
        
      })
      
    })
  
}