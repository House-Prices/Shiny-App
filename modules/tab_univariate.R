tabUnivariateUI <- function(id) {
  
  ns <- shiny::NS(id)
  
  
  shinydashboard::tabItem(
    tabName = "univariate",
    shiny::fluidRow(
      shinydashboard::box(
        width = 4,
        shiny::uiOutput(ns("select_variable"))
      ),
      shinydashboard::tabBox(
        id = ns("selected_tab"),
        width = 8,
        shiny::tabPanel(
          "Categorical",
          shiny::plotOutput(ns("plot_cat"))
        ),
        shiny::tabPanel(
          "Numeric",
          shiny::plotOutput(ns("plot_num"))
        )
      )
    )
  )
  
  
}


tabUnivariateServer <- function(id, data_df) {
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
      
      
      output$select_variable <- shiny::renderUI({
        
        if (input$selected_tab == "Categorical") {
          
          shiny::selectInput(
            inputId = session$ns("selected_cat_variable"),
            label = "Select Variable:",
            choices = colnames(category_df),
            selected = colnames(category_df)[1]
          )
          
        } else if (input$selected_tab == "Numeric") {
          
          shiny::selectInput(
            inputId = session$ns("selected_num_variable"),
            label = "Select Variable:",
            choices = colnames(numeric_df),
            selected = colnames(numeric_df)[1]
          )
          
        }
        
      })
      
      
      output$plot_cat <- shiny::renderPlot({
        
        req(input$selected_cat_variable)
        
        ggplot2::ggplot(
          data = category_df, 
          ggplot2::aes_string(x = input$selected_cat_variable)
        ) + 
          ggplot2::geom_bar()
        
      })
      
      
      output$plot_num <- shiny::renderPlot({
        
        req(input$selected_num_variable)
        
        ggplot2::ggplot(
          data = numeric_df, 
          ggplot2::aes_string(x = input$selected_num_variable)
        ) + 
          ggplot2::geom_density(fill = "grey")
        
      })
      
    }
  )
}