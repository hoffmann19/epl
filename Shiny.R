#library(shiny)
# install.packages('DT')
#library(DT)
#library(ggplot2)
#teamchoices = sort(unlist(as.list(unique(combineddb$team))))
columnlist = sort(unlist(as.list(colnames(averages[,c(2:length(colnames(averages)))]))))

ui <- fluidPage(
  selectInput('x','Choose your X axis',choices = columnlist,selected = 'Goals'),
  selectInput('y','Choose your Y axis',choices = columnlist, selected = 'Passes'),
  dataTableOutput('mytable'),
  plotOutput('scatter')
)

server = function(input, output) {
  dataset <- reactive({
    totals[,c('team','points',input$x,input$y)]
  })
  p = reactive({ggplot(dataset(),aes_string(x=input$x, y=input$y))+
      # geom_point(size=5, shape = 16) +
      geom_text(aes(label = team, size = points)) +
      scale_size(range = c(4, 10)) +
      scale_color_gradient(high = "#0DCC30", low = "#84170E")})
  output$scatter = renderPlot(p())
}

shinyApp(ui = ui, server = server)