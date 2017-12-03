library(shiny)

#teamchoices = sort(unlist(as.list(unique(combineddb$team))))

ui <- fluidPage(
  dataTableOutput('mytable')
)

server = function(input, output) {
  output$mytable = renderDataTable({combineddb})
}

shinyApp(ui = ui, server = server)