#packages
library(shiny)
library(shinythemes)
library(shinydashboard)
library(dplyr)
library(ggplot2)

#grabbing data
df <- combineddb %>% 
  select(team, 
         game_week,gameweek_points, 
         team_standing_rank,
         opp_standing_rank,
         gameweek_points, 
         win_flag, 
         close_standing_flag) %>%
  
  filter(close_standing_flag != 0) %>%
  
  group_by(team) %>%
  
  summarise(total_points_earned = sum(gameweek_points), 
            close_matches_played  = n(), 
            points_per_close_game = (total_points_earned/ close_matches_played)) %>%
  
  arrange(-points_per_close_game)

#variables
columnlist = sort(unlist(as.list(colnames(df[,c(2:length(colnames(df)))]))))
font = 'serif'

summary(df)

ui <- fluidPage(
  theme = shinytheme('united'),
  titlePanel(title=div(img(src="epl logo.png"),"detlef schrempf rules!"),windowTitle = 'EPL 720'),
  sidebarPanel(
    selectInput('x','Choose your X axis',choices = columnlist ,selected = 'total_points_earned'),
    selectInput('y','Choose your Y axis',choices = columnlist, selected = 'close_matches_played')
  ),
  mainPanel(
    plotOutput("scatplot"),
    br(), br(),
    tableOutput("results")
))


server <- function(input, output) {
  
  
}

shinyApp(ui = ui, server = server)