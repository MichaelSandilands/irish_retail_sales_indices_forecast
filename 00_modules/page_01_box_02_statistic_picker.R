box_statistic_picker_ui <- function(id) {
  
  ns <- NS(id)
  uiOutput(ns("box_statistic"))
  
}

box_statistic_picker_server <- function(id, data) {
  
  moduleServer(
    id,
    function(input, output, session) {
      
      output$box_statistic <- renderUI({
        
        ns <- session$ns
        
        choices <- data %>% pull(statistic) %>% as.character() %>% unique()
        selected <- choices[1]
        
        box(
          style = "height:60px;",
          title = "Pick Statistic",
          collapsible = TRUE,
          width = 12, 
          solidHeader = FALSE,
          "Option to Filter Data to Contain Only One Statistic. (Value/Volume)",
          footer = fluidRow(
            style = "height:80px;",
            column(
              width = 12,
              checkboxGroupButtons(
                inputId   = ns("checkbox_statistic"),
                choices   = choices,
                selected  = selected, 
                direction = "vertical",
                checkIcon = list(
                  yes = icon("ok", lib = "glyphicon"),
                  no  = icon("remove", lib = "glyphicon")
                )
              )
            )
          )
        )
      })
      
      return(statistic_filter = reactive({ input$checkbox_statistic }))
      
    })
  
}
