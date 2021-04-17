tabUnivariateUI <- function(id) {
  
  ns <- shiny::NS(id)
  
  
  tabItem(
    tabName = "univariate",
    shiny::fluidRow(
      box(
        title = "Input",
        width = 3,
        shiny::uiOutput(ns("select_variable"))
      ),
      tabBox(
        id = ns("selected_tab"),
        width = 9,
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
        
        t1 <- category_df %>% 
          count(.data[[input$selected_cat_variable]])
        
        p1 <- ggplot(
          data = category_df, 
          aes_string(x = input$selected_cat_variable)
        ) + 
          geom_bar() + 
          theme_minimal()
        
        
        p1 + gridExtra::tableGrob(t1, rows = NULL) + plot_layout(widths = c(3, 1))
        
      })
      
      
      output$plot_num <- shiny::renderPlot({
        
        req(input$selected_num_variable)
        
        p1 <- ggplot(
          data = numeric_df, 
          aes_string(x = input$selected_num_variable)
        ) + 
          geom_density(fill = "grey") + 
          theme_bw() + 
          labs(title = "Density Plot")
        
        p2 <- ggplot(
          data = numeric_df,
          aes_string(x = input$selected_num_variable)
        ) +
          geom_boxplot() +
          coord_flip() +
          theme_minimal() + 
          labs(title = "Box Plot")
        
        
        p1 + p2 + plot_layout(widths = c(3, 1))
        
      })
      
    }
  )
}