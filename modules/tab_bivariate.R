tabBivariateUI <- function(id) {
  
  ns <- shiny::NS(id)
  
  
  tabItem(
    tabName = "bivariate",
    shiny::fluidRow(
      box(
        title = "Input",
        width = 3,
        shiny::uiOutput(ns("select_variable_x")),
        shiny::uiOutput(ns("select_variable_y")),
        shiny::uiOutput(ns("select_plot_type"))
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
      
      available_plots <- shiny::reactive({
        
        x_var <- data_df %>% 
          dplyr::select(input$selected_variable_x) %>% 
          dplyr::pull()
        
        y_var <- data_df %>% 
          dplyr::select(input$selected_variable_y) %>% 
          dplyr::pull()
        
        if (is.numeric(x_var) && is.numeric(y_var)) {
          
          plot_types <- c("scatter")
          
        } else if (is.numeric(x_var) && is.character(y_var)) {
          
          plot_types <- c("box_plot")
          
        } else if (is.character(x_var) && is.numeric(y_var)) {
          
          plot_types <- c("box_plot")
          
        } else if (is.character(x_var) && is.character(y_var)) {
          
          plot_types <- c("NONE")
          
        }
        
        plot_types
        
      })
      
      output$select_plot_type <- shiny::renderUI({
        
        shiny::selectInput(
          inputId = session$ns("selected_plot_type"),
          label = "Select Plot:",
          choices = available_plots(),
          selected = available_plots()[0]
        )
        
      })
      
      
      output$plot <- shiny::renderPlot({
        
        p <- ggplot2::ggplot(
          data = data_df
        )
        
        if (input$selected_plot_type == "scatter") {
          
          p <- p + 
            ggplot2::geom_point(
              mapping = ggplot2::aes_string(
                x = input$selected_variable_x,
                y = input$selected_variable_y
              )
            ) + 
            theme_bw() +
            scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) +
            scale_x_continuous(labels = function(x) format(x, scientific = FALSE))
          
          
        } else if (input$selected_plot_type == "box_plot") {
          
          x_var <- data_df %>% 
            dplyr::select(input$selected_variable_x) %>% 
            dplyr::pull() %>% 
            typeof()
          
          if (x_var == "character") {
            p <- p + 
              ggplot2::geom_boxplot(
                ggplot2::aes_string(
                  x = input$selected_variable_y,
                  y = input$selected_variable_x
                )
              ) +
              scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) +
              theme_minimal()
          } else {
            p <- p + 
              ggplot2::geom_boxplot(
                ggplot2::aes_string(
                  x = input$selected_variable_x,
                  y = input$selected_variable_y
                )
              ) + 
              ggplot2::coord_flip() +
              scale_x_continuous(labels = function(x) format(x, scientific = FALSE)) +
              theme_minimal()
          }
          
          
        } else {
          
          p <- NULL
          
        }
        
        p
        
      })
      
    }
  )
}