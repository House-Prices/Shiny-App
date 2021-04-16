
header <- dashboardHeader(
  dropdownMenu(type = "messages",
               messageItem(
                 from = "Sales Dept",
                 message = "Sales are steady this month."
               ),
               messageItem(
                 from = "New User",
                 message = "How do I register?",
                 icon = icon("question"),
                 time = "13:45"
               ),
               messageItem(
                 from = "Support",
                 message = "The new server is ready.",
                 icon = icon("life-ring"),
                 time = "2014-12-01"
               )
  ),
  dropdownMenuOutput("messageMenu"),
  dropdownMenu(type = "tasks", badgeStatus = "success",
               taskItem(value = 90, #color = "green",
                        "Documentation"
               ),
               taskItem(value = 17, #color = "aqua",
                        "Project X"
               ),
               taskItem(value = 75, #color = "yellow",
                        "Server deployment"
               ),
               taskItem(value = 80, #color = "red",
                        "Overall project"
               )
  )
)

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Univariate", tabName = "univariate", icon = icon("dashboard")),
    menuItem("Widgets", icon = icon("th"), tabName = "widgets",
             badgeLabel = "new"),
    menuItem("Info Boxes", tabName = "infoboxes", icon = icon("box"))
  )
)

body <- dashboardBody(
  tabItems(
    tabUnivariateUI("tab_univariate"),
    
    tabItem(tabName = "widgets",
            h2("Widgets tab content"),
            fluidRow(
              tabBox(
                title = "First tabBox",
                # The id lets us use input$tabset1 on the server to find the current tab
                id = "tabset1", height = "250px",
                tabPanel("Tab1", "First tab content"),
                tabPanel("Tab2", "Tab content 2")
              ),
              tabBox(
                side = "right", height = "250px",
                id = "tabset2",
                selected = "Tab3",
                tabPanel("Tab1", "Tab content 1"),
                tabPanel("Tab2", "Tab content 2"),
                tabPanel("Tab3", "Note that when side=right, the tab order is reversed.")
              )
            ),
            fluidRow(
              tabBox(
                # Title can include an icon
                id = "tabset3",
                title = tagList(shiny::icon("gear"), "tabBox status"),
                tabPanel("Tab1",
                         "Currently selected tab from first box:",
                         verbatimTextOutput("tabset1Selected")
                ),
                tabPanel("Tab2", "Tab content 2")
              )
            )
    ),
    
    tabItem(tabName = "infoboxes",
            # infoBoxes with fill=FALSE
            fluidRow(
              # A static infoBox
              infoBox("New Orders", 10 * 2, icon = icon("credit-card")),
              # Dynamic infoBoxes
              infoBoxOutput("progressBox"),
              infoBoxOutput("approvalBox")
            ),
            
            # infoBoxes with fill=TRUE
            fluidRow(
              infoBox("New Orders", 10 * 2, icon = icon("credit-card"), fill = TRUE, color = "indigo"),
              infoBoxOutput("progressBox2"),
              infoBoxOutput("approvalBox2")
            ),
            
            fluidRow(
              # Clicking this will increment the progress amount
              box(width = 4, actionButton("count", "Increment progress"))
            )
    )
  )
)

# Put them together into a dashboardPage
dashboardPage(
  header,
  sidebar,
  body
)
