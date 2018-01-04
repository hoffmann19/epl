# library(shiny)
# library(shinythemes)
# # install.packages('DT')
# library(DT)
# library(ggplot2)
# library(ggthemes)
# #install.packages("extrafont")
# library(extrafont)
# font_import(pattern="[H/h]umor")
# loadfonts()

#fonttable()[1:45,]
font = 'Comic Sans MS'
#teamchoices = sort(unlist(as.list(unique(combineddb$team))))
colnames(totals) = make.names(colnames(totals))
columnlist = sort(unlist(as.list(colnames(totals[,c(2:length(colnames(totals)))]))))


ui <- fluidPage(theme = shinytheme("superhero"),
                titlePanel(title=div(img(src="epl logo.png"),"detlef schrempf rules!"),windowTitle = 'EPL 720'),
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
      theme_solarized(light = FALSE)+
      theme(legend.position = "None",text=element_text(family=font, size=14))+
      #geom_point(size=5, shape = 16) +
      scale_color_hue(l=65, c=100)+
      geom_text(aes(label = team, size = points, family = font, color = factor(clusters()$cluster))) +
      scale_size(range = c(4, 10))})
  output$scatter = renderPlot(p())
  
  standingstable = reactive({
    #combineddb[order(combineddb[combineddb$game_week == input$week,c('team','cumu_points')],combineddb$cumu_points, decreasing = TRUE),]
    subset(combineddb, game_week ==input$week,c('team','cumu_points'))[order(subset(combineddb, game_week ==input$week)$cumu_points,decreasing = TRUE),]

  })

  b = reactive({ggplot(standingstable(), aes_string('team', 'cumu_points')) + 
      theme_solarized(light = FALSE)+
      geom_text(aes(label = team, color = cumu_points, family = font))+
      geom_hline(yintercept=standingstable()$cumu_points[4], colour = 'white',linetype = 8)+
      geom_hline(yintercept=standingstable()$cumu_points[18], colour = 'tomato',linetype = 8)+
      scale_colour_gradientn(colors = terrain.colors(10))+
      theme(legend.position = "None",text=element_text(family=font, size=14))+
      scale_y_continuous(breaks = seq(0,100, by=3)) +
      geom_segment(aes(x=team, 
                       xend= team,
                       y=0,
                       yend = cumu_points),
                   linetype="dashed", 
                   size=0.1) +   # Draw dashed lines
      labs(title="EPL Standings", 
           subtitle="2017-18") +
      xlab('Team') + ylab('Points')+
      coord_flip(ylim = c(0, 100))
  })
  
  output$standings = renderPlot(b())
  
  clusters <- reactive({
    kmeans(dataset()[,3:4],input$clusters, 
           nstart = 20)
  })
  
}
shinyApp(ui = ui, server = server)