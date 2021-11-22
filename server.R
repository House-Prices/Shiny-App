
server <- function(input, output, session) {
  
  tabUnivariateServer(
    "tab_univariate",
    data_df = data_list[["train"]],
    description_df = data_list[["description"]]
  )
  
  tabBivariateServer(
    "tab_bivariate",
    data_df = data_list[["train"]],
    description_df = data_list[["description"]]
  )
  
  tabCorrelationServer(
    "tab_correlation",
    data_df = data_list[["train"]],
    description_df = data_list[["description"]]
  )
  
  
  messageData <- data.frame(
    from = c("Admininstrator", "New User", "Support"),
    message = c(
      "Sales are steady this month.",
      "How do I register?",
      "The new server is ready."
    ),
    stringsAsFactors = FALSE
  )
  
  output$messageMenu <- bs4Dash::renderMenu({
    # Code to generate each of the messageItems here, in a list. This assumes
    # that messageData is a data frame with two columns, 'from' and 'message'.
    msgs <- apply(messageData, 1, function(row) {
      bs4Dash::messageItem(from = row[["from"]], message = row[["message"]])
    })
    
    # This is equivalent to calling:
    #   dropdownMenu(type="messages", msgs[[1]], msgs[[2]], ...)
    bs4Dash::dropdownMenu(type = "messages", .list = msgs)
  })
  
  
  # The currently selected tab from the first box
  output$tabset1Selected <- shiny::renderText({
    input$tabset1
  })
  
  output$progressBox <- bs4Dash::renderInfoBox({
    bs4Dash::infoBox(
      "Progress", paste0(25 + input$count, "%"), icon = shiny::icon("list"),
      color = "purple"
    )
  })
  output$approvalBox <- bs4Dash::renderInfoBox({
    bs4Dash::infoBox(
      "Approval", "80%", icon = shiny::icon("thumbs-up", lib = "glyphicon"),
      color = "lime"
    )
  })
  
  # Same as above, but with fill=TRUE
  output$progressBox2 <- bs4Dash::renderInfoBox({
    bs4Dash::infoBox(
      "Progress", paste0(25 + input$count, "%"), icon = shiny::icon("list"),
      color = "purple", fill = TRUE
    )
  })
  output$approvalBox2 <- bs4Dash::renderInfoBox({
    bs4Dash::infoBox(
      "Approval", "80%", icon = shiny::icon("thumbs-up", lib = "glyphicon"),
      color = "lime", fill = TRUE
    )
  })
}
