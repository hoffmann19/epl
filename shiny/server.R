library(shiny)
library(shinydashboard)
shinyServer(function(input,output){
  output$histogram = renderPlot({
    hist(combineddb$Goals, breaks = input$bins)
  })
}
  )