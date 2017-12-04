library(shiny)
install.packages('DT')
library(DT)
library(ggplot2)
#teamchoices = sort(unlist(as.list(unique(combineddb$team))))
columnlist = sort(unlist(as.list(colnames(Averages[,c(2:length(colnames(Averages)))]))))

ui <- fluidPage(
 selectInput('x','Choose your X axis',choices = columnlist,selected = 'Goals'),
 selectInput('y','Choose your Y axis',choices = columnlist, selected = 'Passes'),
 dataTableOutput('mytable'),
 plotOutput('scatter')
)

server = function(input, output) {
  dataset <- reactive({
    totals[,c('team',input$x,input$y)]
  })
  p = reactive({ggplot(dataset(),aes_string(x=input$x, y=input$y,color = input$y))+geom_point(size=5, shape = 16)+scale_color_gradient(low = "#0091ff", high = "#f0650e")})
  output$scatter = renderPlot(p())
}

shinyApp(ui = ui, server = server)