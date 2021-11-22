tabCorrelationUI <- function(id) {
  
  ns <- shiny::NS(id)
  
  bs4Dash::tabItem(
    tabName = "correlation",
    shiny::fluidRow(
      bs4Dash::box(
        title = "Input",
        width = 3,
        shiny::uiOutput(ns("select_features"))
      ),
      bs4Dash::box(
        width = 9,
        shiny::plotOutput(ns("plot"))
      )
    ),
    shiny::fluidRow(
      bs4Dash::box(
        width = 12,
        "stuff..."
      )
    )
  )
  
}

tabCorrelationServer <- function(id, data_df, description_df) {
  
  moduleServer(
    id,
    function(input, output, session) {
      
      numeric_df <- data_df %>% 
        dplyr::select(
          tidyselect::vars_select_helpers$where(is.numeric)
        )
      
      
      # Select which features appear in plot
      output$select_features <- shiny::renderUI({
        
        shinyWidgets::pickerInput(
          inputId = session$ns("selected_features"),
          label = "Select Features", 
          choices = names(numeric_df),
          selected = names(numeric_df)[1:10],
          multiple = TRUE
        )
        
      })
      
      
      # Create correlation matrix with selected features
      corr_dfr <- shiny::reactive({
        
        corr_df <- cor(
          numeric_df %>% 
            dplyr::select(input$selected_features),
          use = "na.or.complete"
        )
        
      })
      
      
      # Plot the matrix
      output$plot <- shiny::renderPlot({
        
        corrplot::corrplot(corr_dfr(), method = "number")
        
      })
      
      
    }
  )
  
}