box_summary_ui <- function(id) {
  
  ns <- NS(id)
  uiOutput(ns("box_summary"))
  
}

box_summary_server <- function(id, data) {
  
  moduleServer(
    id,
    function(input, output, session) {
      
      # * BOX - TS Summary Desc----
      # ** Box ----
      output$box_summary <- renderUI({
        
        num_groups <- data %>% distinct(statistic, nace_group) %>% nrow()
        
        box(
          style = "height:60px;",
          title = "Time Series Description",
          collapsible = TRUE,
          width = 12,
          solidHeader = FALSE,
          "Ireland's Retail Sales Index (Unadjusted)",
          tags$a(href="https://data.cso.ie/table/RSM05", "CSO Table Link", class = "pull-right"),
          footer = fluidRow(
            style = "height:80px;",
            column(
              width = 4,
              descriptionBlock(
                header = "CSO Connection",
                text = HTML("<span>RSM05</span>"),
                rightBorder = TRUE,
                marginBottom = FALSE
              )
            ),
            column(
              width = 4,
              descriptionBlock(
                header = "Scale",
                text = HTML(str_glue("<span>Month</span>")),
                rightBorder = TRUE,
                marginBottom = FALSE
              )
            ),
            column(
              width = 4,
              descriptionBlock(
                header = "Groups Detected",
                text = HTML(str_glue("<span>{num_groups}</span>")),
                rightBorder = FALSE,
                marginBottom = FALSE
              )
            )
          )
        )
        
      })
      
    })
  
}