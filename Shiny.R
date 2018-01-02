library(shiny)
# install.packages('DT')
library(DT)
library(ggplot2)
#teamchoices = sort(unlist(as.list(unique(combineddb$team))))
colnames(totals) = make.names(colnames(totals))
columnlist = sort(unlist(as.list(colnames(totals[,c(2:length(colnames(totals)))]))))

ui <- fluidPage(
  selectInput('x','Choose your X axis',choices = columnlist,selected = 'Goals'),
  selectInput('y','Choose your Y axis',choices = columnlist, selected = 'Passes'),
  numericInput('clusters', 'Cluster count', 4, min = 1, max = 9),
  dataTableOutput('mytable'),
  plotOutput('scatter'),
  sliderInput('week','Select Gameweek', min = 1,max = 38,value = 1,step = 1),
  plotOutput('standings')
  
)

server = function(input, output) {
  dataset <- reactive({
    totals[,c('team','points',input$x,input$y)] 
  })
  kmean <- reactive({
    totals[,c(input$x,input$y)] 
  })
  p = reactive({ggplot(dataset(),aes_string(x=input$x, y=input$y))+
      #geom_point(size=5, shape = 16) +
      scale_color_hue(l=65, c=100)+
      geom_text(aes(label = team, size = points, color = factor(clusters()$cluster))) +
      scale_size(range = c(4, 10))})
  output$scatter = renderPlot(p())
  
  standingstable = reactive({
    combineddb[combineddb$game_week == input$week,c('team','cumu_points')]
  })
  
  b = reactive({ggplot(standingstable(),aes_string('cumu_points', 'team'))+
      #geom_point(size=5, shape = 16) +
      scale_color_hue(l=65, c=100)+
      geom_text(aes(label = team, color = team))})
  output$standings = renderPlot(b())
  
  clusters <- reactive({
    kmeans(dataset()[,3:4],input$clusters, 
           nstart = 20)
  })
  
}
shinyApp(ui = ui, server = server)
