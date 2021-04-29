tabUnivariateUI <- function(id) {
  
  ns <- shiny::NS(id)
  
  
  bs4Dash::tabItem(
    tabName = "univariate",
    shiny::fluidRow(
      bs4Dash::box(
        title = "Input",
        width = 3,
        shiny::uiOutput(ns("select_variable")),
        shiny::textOutput(ns("variable_description")),
        shiny::tags$head(
          shiny::tags$style(
            "#tab_univariate-variable_description {
              font-style: italic;
              color: gray;
            }"
          )
        )
      ),
      bs4Dash::tabBox(
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
        
        shiny::req(input$selected_variable)
        
        description_df %>% 
          dplyr::filter(variable == input$selected_variable) %>% 
          dplyr::pull(description)
        
      })
      
      
      output$plot_cat <- shiny::renderPlot({
        
        shiny::req(input$selected_variable)
        
        
        if (input$selected_variable %in% colnames(category_df)) {
          
          t1 <- category_df %>% 
            dplyr::count(.data[[input$selected_variable]])
          
          p1 <- ggplot2::ggplot(
            data = category_df, 
            ggplot2::aes_string(x = input$selected_variable)
          ) + 
            ggplot2::geom_bar() +
            ggplot2::theme_minimal()
          
          
          p1 + gridExtra::tableGrob(t1, rows = NULL) + patchwork::plot_layout(widths = c(3, 1))
        
        }
        
      })
      
      
      output$plot_num <- shiny::renderPlot({
        
        shiny::req(input$selected_variable)
        
        if (input$selected_variable %in% colnames(numeric_df)) {
          
          p1 <- ggplot2::ggplot(
            data = numeric_df, 
            ggplot2::aes_string(x = input$selected_variable)
          ) + 
            ggplot2::geom_density(fill = "grey") + 
            ggplot2::theme_bw() + 
            ggplot2::scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) +
            ggplot2::scale_x_continuous(labels = function(x) format(x, scientific = FALSE)) +
            ggplot2::labs(title = "Density Plot")
          
          p2 <- ggplot(
            data = numeric_df,
            ggplot2::aes_string(x = input$selected_variable)
          ) +
            ggplot2::geom_boxplot() +
            ggplot2::coord_flip() + 
            ggplot2::scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) +
            ggplot2::scale_x_continuous(labels = function(x) format(x, scientific = FALSE)) +
            ggplot2::theme_minimal() + 
            ggplot2::labs(title = "Box Plot")
          
          
          p1 + p2 + patchwork::plot_layout(widths = c(3, 1))
          
        }
        
      })
      
    }
  )
}