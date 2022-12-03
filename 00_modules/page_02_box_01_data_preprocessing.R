box_data_preprocessing_ui <- function(id) {
  
  ns <- NS(id)
  uiOutput(ns("box_data_preprocessing"))
  
}

box_data_preprocessing_server <- function(id, data) {
  
  moduleServer(
    id,
    function(input, output, session) {
      
      output$box_data_preprocessing <- renderUI({
        
        ns <- session$ns
        
        box(
          title = "Data Preprocessing",
          collapsible = TRUE,
          width = 12, 
          solidHeader = FALSE,
          fluidRow(
            column(
              width = 6,
              h4("Log Transform Data?"),
              switchInput(
                inputId     = ns("log_transform_switch"), 
                onLabel     = "Yes", 
                offLabel    = "No",
                value       = FALSE, 
                handleWidth = 80, 
                labelWidth  = 1, 
                inline      = TRUE, 
                width       = "120px"
              )
            ),
            column(
              width = 6,
              h4("Clean Covid Anomalies?"),
              switchInput(
                inputId  = ns("clean_anomalies_switch"), 
                onLabel     = "Yes", 
                offLabel    = "No",
                value       = TRUE, 
                handleWidth = 80, 
                labelWidth  = 1, 
                inline      = TRUE, 
                width       = "120px"
              )
            ),
            column(
              width = 12,
              sliderInput(
                inputId = ns("clean_anomalies_slider"), 
                label   = "Choose How Aggressively the Anomalies are Cleaned",
                min     = 0, 
                max     = 1, 
                value   = 0.1
              )
            ),
            br(),
            hr(),
            br(),
            column(
              width = 4,
              actionButton(
                inputId = ns("apply_preproc"), 
                label   = "Apply", 
                icon    = icon("play")
              )
            )
          )
        )
      })
      
      processed_data <- eventReactive(input$apply_preproc, {
        
        req(input$clean_anomalies_slider)
        
        if(input$log_transform_switch) {
          
          data <- data %>% 
            mutate(value = log(value))
          
        }
        
        if(input$clean_anomalies_switch) {
          
          data <- data %>% 
            rsi_clean_anomalies(alpha = input$clean_anomalies_slider) %>% 
            filter(!(name == "value_clean" & (date < "2020-01-01" | date >= "2021-06-01"))) %>% 
            mutate(name = name %>% str_replace_all("_", " ") %>% str_to_title()) 
          
        }
        
        return(data)
        
        
      }, ignoreNULL = FALSE)
      
      return(
        list(
          log_transform_switch   = reactive({ input$log_transform_switch }),
          clean_anomalies_switch = reactive({ input$clean_anomalies_switch }),
          clean_anomalies_slider = reactive({ input$clean_anomalies_slider }),
          processed_data         = processed_data 
        )
      )
      
    })
  
}
