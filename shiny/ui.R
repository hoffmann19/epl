library(shiny)
library(shinydashboard)
shinyUI(
  dashboardPage(
    dashboardHeader(title = 'This is the header'),
    dashboardSidebar(
      sliderInput("bins","number of bins",1,10,7),
      selectInput("SelectTeam",label = h3("Select Team"),
                  choices = unique(combineddb$team),
                  selected = 1),
      menuItem("Histogram", tabName = 'hist'),
      menuSubItem("dashboard finance"),
      menuSubItem("Dashboard Sales"),
      menuItem("Detailed Analysis"),
      menuItem("Raw Data")
    ),
    dashboardBody(
      tabItems(
        tabItem(tabName = 'hist',
                fluidRow(
                  box(plotOutput("histogram"))
                ))
      )
    )
  )
  
)
