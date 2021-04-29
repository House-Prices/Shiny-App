
header <- bs4Dash::dashboardHeader()


sidebar <- bs4Dash::dashboardSidebar(
  bs4Dash::sidebarMenu(
    bs4Dash::menuItem("Univariate", tabName = "univariate", icon = icon("chart-bar")),
    bs4Dash::menuItem("Bivariate", tabName = "bivariate", icon = icon("chart-area")),
    bs4Dash::menuItem("Widgets", icon = icon("th"), tabName = "widgets",
             badgeLabel = "new"),
    bs4Dash::menuItem("Info Boxes", tabName = "infoboxes", icon = icon("box"))
  )
)


body <- bs4Dash::dashboardBody(
  bs4Dash::tabItems(
    tabUnivariateUI("tab_univariate"),
    tabBivariateUI("tab_bivariate"),
    bs4Dash::tabItem(tabName = "widgets",
            shiny::h2("Widgets tab content"),
            shiny::fluidRow(
              bs4Dash::tabBox(
                title = "First tabBox",
                # The id lets us use input$tabset1 on the server to find the current tab
                id = "tabset1", height = "250px",
                shiny::tabPanel("Tab1", "First tab content"),
                shiny::tabPanel("Tab2", "Tab content 2")
              ),
              bs4Dash::tabBox(
                side = "right", height = "250px",
                id = "tabset2",
                selected = "Tab3",
                shiny::tabPanel("Tab1", "Tab content 1"),
                shiny::tabPanel("Tab2", "Tab content 2"),
                shiny::tabPanel("Tab3", "Note that when side=right, the tab order is reversed.")
              )
            ),
            shiny::fluidRow(
              bs4Dash::tabBox(
                # Title can include an icon
                id = "tabset3",
                title = tagList(shiny::icon("gear"), "tabBox status"),
                shiny::tabPanel("Tab1",
                         "Currently selected tab from first box:",
                         shiny::verbatimTextOutput("tabset1Selected")
                ),
                shiny::tabPanel("Tab2", "Tab content 2")
              )
            )
    ),
    
    bs4Dash::tabItem(tabName = "infoboxes",
            # infoBoxes with fill=FALSE
            shiny::fluidRow(
              # A static infoBox
              bs4Dash::infoBox("New Orders", 10 * 2, icon = shiny::icon("credit-card")),
              # Dynamic infoBoxes
              bs4Dash::infoBoxOutput("progressBox"),
              bs4Dash::infoBoxOutput("approvalBox")
            ),
            
            # infoBoxes with fill=TRUE
            shiny::fluidRow(
              bs4Dash::infoBox("New Orders", 10 * 2, icon = shiny::icon("credit-card"), fill = TRUE, color = "indigo"),
              bs4Dash::infoBoxOutput("progressBox2"),
              bs4Dash::infoBoxOutput("approvalBox2")
            ),
            
            shiny::fluidRow(
              # Clicking this will increment the progress amount
              bs4Dash::box(width = 4, shiny::actionButton("count", "Increment progress"))
            )
    )
  )
)

# Put them together into a dashboardPage
bs4Dash::dashboardPage(
  header,
  sidebar,
  body
)
