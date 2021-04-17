tabUnivariateUI <- function(id) {
  
  ns <- shiny::NS(id)
  
  
  tabItem(
    tabName = "univariate",
    shiny::fluidRow(
      box(
        title = "Input",
        width = 3,
        shiny::uiOutput(ns("select_variable")),
        shiny::textOutput(ns("variable_description"))
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


tabUnivariateServer <- function(id, data_df, description_df) {
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
            inputId = session$ns("selected_variable"),
            label = "Select Variable:",
            choices = colnames(category_df),
            selected = colnames(category_df)[1]
          )
          
        } else if (input$selected_tab == "Numeric") {
          
          shiny::selectInput(
            inputId = session$ns("selected_variable"),
            label = "Select Variable:",
            choices = colnames(numeric_df),
            selected = colnames(numeric_df)[1]
          )
          
        }
        
      })
      
      
      output$variable_description <- shiny::renderText({
        
        req(input$selected_variable)
        
        description_df %>% 
          filter(variable == input$selected_variable) %>% 
          pull(description)
        
      })
      
      
      output$plot_cat <- shiny::renderPlot({
        
        req(input$selected_variable)
        
        
        if (input$selected_variable %in% colnames(category_df)) {
          
          t1 <- category_df %>% 
            count(.data[[input$selected_variable]])
          
          p1 <- ggplot(
            data = category_df, 
            aes_string(x = input$selected_variable)
          ) + 
            geom_bar() +
            theme_minimal()
          
          
          p1 + gridExtra::tableGrob(t1, rows = NULL) + plot_layout(widths = c(3, 1))
        
        }
        
      })
      
      
      output$plot_num <- shiny::renderPlot({
        
        req(input$selected_variable)
        
        if (input$selected_variable %in% colnames(numeric_df)) {
          
          p1 <- ggplot(
            data = numeric_df, 
            aes_string(x = input$selected_variable)
          ) + 
            geom_density(fill = "grey") + 
            theme_bw() + 
            scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) +
            scale_x_continuous(labels = function(x) format(x, scientific = FALSE)) +
            labs(title = "Density Plot")
          
          p2 <- ggplot(
            data = numeric_df,
            aes_string(x = input$selected_variable)
          ) +
            geom_boxplot() +
            coord_flip() + 
            scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) +
            scale_x_continuous(labels = function(x) format(x, scientific = FALSE)) +
            theme_minimal() + 
            labs(title = "Box Plot")
          
          
          p1 + p2 + plot_layout(widths = c(3, 1))
          
        }
        
      })
      
    }
  )
}