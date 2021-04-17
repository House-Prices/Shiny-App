tabBivariateUI <- function(id) {
  
  ns <- shiny::NS(id)
  
  
  tabItem(
    tabName = "bivariate",
    shiny::fluidRow(
      box(
        title = "Input",
        width = 3,
        shiny::uiOutput(ns("select_variable_x")),
        shiny::uiOutput(ns("select_variable_y"))
      ),
      box(
        width = 9,
        shiny::plotOutput(ns("plot"))
      )
    )
  )
  
  
}


tabBivariateServer <- function(id, data_df) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      category_df <- data_df %>% 
        dplyr::select(
          tidyselect::vars_select_helpers$where(is.character)
        )
      
      numeric_df <- data_df %>% 
        dplyr::select(
          tidyselect::vars_select_helpers$where(is.numeric)
        )
      
      output$select_variable_x <- shiny::renderUI({
        
        shiny::selectInput(
          inputId = session$ns("selected_variable_x"),
          label = "Select Variable (X-Axis):",
          choices = colnames(data_df),
          selected = colnames(data_df)[1]
        )
          
      })
      
      output$select_variable_y <- shiny::renderUI({
        
        shiny::selectInput(
          inputId = session$ns("selected_variable_y"),
          label = "Select Variable (Y-Axis):",
          choices = colnames(data_df),
          selected = colnames(data_df)[2]
        )
        
      })
      
    }
  )
}