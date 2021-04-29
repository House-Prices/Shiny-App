tabBivariateUI <- function(id) {
  
  ns <- shiny::NS(id)
  
  
  bs4Dash::tabItem(
    tabName = "bivariate",
    shiny::fluidRow(
      bs4Dash::box(
        title = "Input",
        width = 3,
        shiny::uiOutput(ns("select_variable_x")),
        shiny::textOutput(ns("variable_description_x")),
        shiny::tags$head(
          shiny::tags$style(
            "#tab_bivariate-variable_description_x {
              font-style: italic;
              color: gray;
            }"
          )
        ),
        shiny::br(),
        shiny::uiOutput(ns("select_variable_y")),
        shiny::textOutput(ns("variable_description_y")),
        shiny::tags$head(
          shiny::tags$style(
            "#tab_bivariate-variable_description_y {
              font-style: italic;
              color: gray;
            }"
          )
        ),
        shiny::br(),
        shiny::uiOutput(ns("select_plot_type"))
      ),
      bs4Dash::box(
        width = 9,
        shiny::plotOutput(ns("plot"))
      )
    )
  )
  
  
}


tabBivariateServer <- function(id, data_df, description_df) {
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
      
      variable_types <- data_df %>%
        purrr::map_chr(
          ~ typeof(.x)
        ) %>% 
        unname()
      
      output$select_variable_x <- shiny::renderUI({
        
        shinyWidgets::pickerInput(
          inputId = session$ns("selected_variable_x"),
          label = "Select Variable (X-Axis):",
          choices = colnames(data_df),
          selected = colnames(data_df)[1],
          choicesOpt = list(
            subtext = paste(variable_types),
            style = "color: red"
          )
        )
          
      })
      
      output$variable_description_x <- shiny::renderText({
        
        shiny::req(input$selected_variable_x)
        
        description_df %>% 
          dplyr::filter(variable == input$selected_variable_x) %>% 
          dplyr::pull(description)
        
      })
      
      output$select_variable_y <- shiny::renderUI({
        
        shinyWidgets::pickerInput(
          inputId = session$ns("selected_variable_y"),
          label = "Select Variable (Y-Axis):",
          choices = colnames(data_df),
          selected = colnames(data_df)[2],
          choicesOpt = list(
            subtext = paste(variable_types)
          )
        )
        
      })
      
      output$variable_description_y <- shiny::renderText({
        
        shiny::req(input$selected_variable_y)
        
        description_df %>% 
          dplyr::filter(variable == input$selected_variable_y) %>% 
          dplyr::pull(description)
        
      })
      
      available_plots <- shiny::reactive({
        
        shiny::req(input$selected_variable_x)
        shiny::req(input$selected_variable_y)
        
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
        
        shiny::req(input$selected_variable_x)
        shiny::req(input$selected_variable_y)
        shiny::req(input$selected_plot_type)
        
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
            ggplot2::theme_bw() +
            ggplot2::scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) +
            ggplot2::scale_x_continuous(labels = function(x) format(x, scientific = FALSE))
          
          
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
              ggplot2::scale_x_continuous(labels = function(x) format(x, scientific = FALSE)) +
              ggplot2::theme_minimal()
          } else {
            p <- p + 
              ggplot2::geom_boxplot(
                ggplot2::aes_string(
                  x = input$selected_variable_x,
                  y = input$selected_variable_y
                )
              ) + 
              ggplot2::coord_flip() +
              ggplot2::scale_x_continuous(labels = function(x) format(x, scientific = FALSE)) +
              ggplot2::theme_minimal()
          }
          
          
        } else {
          
          p <- NULL
          
        }
        
        p
        
      })
      
    }
  )
}
