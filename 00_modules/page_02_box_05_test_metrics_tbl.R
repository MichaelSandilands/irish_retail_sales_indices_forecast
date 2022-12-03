box_test_metrics_ui <- function(id) {
  
  ns <- NS(id)
  uiOutput(ns("test_metrics_tbl"))
  
}

box_test_metrics_server <- function(id, data) {
  
  moduleServer(
    id,
    function(input, output, session) {
      
      output$test_metrics_tbl <- renderUI({
        
        if(is.null(data())) {
          return()
        }
        
        ns <- session$ns
        
        box(
          title    = "Test Results",
          closable = FALSE,
          width    = 12,
          solidHeader = FALSE,
          collapsible = TRUE,
          reactableOutput(outputId = ns("summary_tbl"))
        )
        
      })
      
      # ** Plotly ----
      output$summary_tbl <- renderReactable({
        
        req(!is.null(data()))
        
        reactable(
          data = data()
        )
        
      })
      
    })
  
}