library(rvest)
library(tidyverse)
#install.packages('stringr')
library(stringr)

#finding all gamecodes.
list_of_month <- read_html('http://scores.nbcsports.com/epl/fixtures.asp')
list_of_month <- as.integer(list_of_month %>% html_node('select') %>% html_nodes('option') %>% html_attr("value"))

base_url <- 'http://scores.nbcsports.com/epl/fixtures.asp?month='

master_list_of_games <- list()
for(i in list_of_month){
  list_of_games <- read_html(paste0(base_url, i))
  list_of_games <- list_of_games %>% html_nodes('.shsNamD') %>% html_node('a') %>% html_attr('href')
  list_of_games <- data.frame(list_of_games[grepl('boxscore',list_of_games)])
  colnames(list_of_games) <- 'x'
  master_list_of_games <-  rbind(list_of_games, master_list_of_games)
}
front_of_url <- "http://scores.nbcsports.com"
back_of_url <- "&show=pstats&ref="
master_list_of_games$x <- paste0(front_of_url, master_list_of_games$x, back_of_url)
data.matrix(master_list_of_games)
rm(list_of_games)
listofURL <- master_list_of_games[['x']]

##scraper to put valid urls data into final df
count=0
for (i in listofURL){
  url = listofURL[count+1]
  population <- url %>%
    read_html() %>%
    html_nodes(xpath='//*[@id="shsIFBBoxPlayerStats1"]/table[2]') %>%
    html_table(fill=TRUE)
  population <- population[[1]]
  db= population[3:18,1:3]
  db = setNames(data.frame(t(db[,-2])), db[,2])
  colnames(db)[1] <- "team"
  db$team = str_sub(db$team, start= -3)
  db$date = str_sub(
    str_split_fixed(string = url, pattern = "gamecode=", n= 2)[,2], 
    end = 8)
  db$Opponent[1] = db$team[2]
  db$Opponent[2] = db$team[1]
  db$GoalsAllowed[1] = as.integer(as.character(db$Goals[2]))
  db$GoalsAllowed[2] = as.integer(as.character(db$Goals[1]))
  db$GameID = paste0(db$team[1],'v',db$team[2],db$date[1])
  db$home_away = if_else(db$team==db$team[1],'home','away')
  if(count==0){ 
    combineddb = db }
  else {
    combineddb= rbind(combineddb,db) }
  count = count+1
  print(count)
}

#removing unnecessary tables
rm(db, population)

#making copy of combineddb to test
# test = combineddb

#converting ball possession to decimal
combineddb$`Ball Possession` = as.numeric(sub("%","",combineddb$`Ball Possession`))/100

#converting all factors to numeric
indx = sapply(combineddb, is.factor)
combineddb[indx] = lapply(combineddb[indx], function(x) as.numeric(as.character(x)))
str(combineddb)

#adding columns
combineddb$win_flag = if_else(combineddb$Goals> combineddb$GoalsAllowed, true = 1, false = 0)
combineddb$gameweek_points = if_else(combineddb$Goals>combineddb$GoalsAllowed, true = 3,
                                     if_else(combineddb$Goals == combineddb$GoalsAllowed, true = 1, false = 0))
combineddb$goal_conversion_total_shots = combineddb$Goals/combineddb$Shots
combineddb$goal_conversion_shots_on_goal = combineddb$Goals/combineddb$`Shots on Goal`
combineddb$unassisted_goals = combineddb$Goals- combineddb$Assists
combineddb$passes_per_goal = combineddb$Passes/combineddb$Goals
combineddb$fouls_per_yellow_card = combineddb$`Fouls Committed`/combineddb$`Cautions/Yellow Cards`
combineddb$fouls_per_red_card = combineddb$`Fouls Committed`/combineddb$`Red Cards`
combineddb$goal_difference = combineddb$Goals- combineddb$GoalsAllowed

#adding gameweek - scraper doesn't necessarily pull in sequential order
combineddb = combineddb %>% group_by(team) %>%
  mutate(game_week = rank(date)) %>%
  arrange(date)

#making team unique ids by gameweek
combineddb$team_gameweek_id = paste(combineddb$team, combineddb$game_week, sep = "-")
combineddb$opp_team_gameweek_id = paste(combineddb$Opponent, combineddb$game_week, sep = "-")

#adding cumulative points
combineddb = combineddb %>% group_by(team) %>% 
  mutate(cumu_points = cumsum(gameweek_points))

#adding cumulative goal difference
combineddb = combineddb %>% group_by(team) %>% 
  mutate(cumu_goal_difference = cumsum(goal_difference))

#adding cumulative goal difference
combineddb = combineddb %>% group_by(team) %>% 
  mutate(cumu_goals = cumsum(Goals))

#new table with standings by gameweek
standings = combineddb[,c("team",
                          "game_week",
                          "cumu_points",
                          "cumu_goal_difference",
                          "cumu_goals")]

#sorting by game week asc, cumu_points desc, cumu_goal_difference
standings = standings[
  order( standings$game_week, 
         -standings$cumu_points, 
         -standings$cumu_goal_difference,
         -standings$cumu_goals),
  ]
#making uniqueid in standings table
standings$team_gameweek_id = paste(standings$team, standings$game_week, sep="-")

#row_num for standings table
standings$rank = row_number(standings$game_week)

#ranking to get actual standing rank for each gameweek
standings$standing_rank <- ave(standings$rank, standings$game_week, FUN = seq_along)

#grab only standings
standings = standings[,c("team_gameweek_id",
                         "standing_rank")]

#adding gameweek standing to combineddb
combineddb = left_join(combineddb, standings, by= "team_gameweek_id")
combineddb = rename(combineddb, team_standing_rank = standing_rank)
combineddb = left_join(combineddb, standings, by = c("opp_team_gameweek_id" = "team_gameweek_id"))
combineddb = rename(combineddb, opp_standing_rank = standing_rank)

#add difference in standings
combineddb$standing_difference = as.numeric(combineddb$opp_standing_rank) - as.numeric(combineddb$team_standing_rank)

#close game <= 3 places
combineddb$close_standing_flag =if_else(abs(combineddb$standing_difference) <= 3, 1, 0)

#against top 6 flag
combineddb$against_top_6_flag = if_else(combineddb$opp_standing_rank <=6, 1,0)

#value of point- need to think on this more...
combineddb$value_point = combineddb$standing_difference/ combineddb$gameweek_points
# creating an averages tables
averages = aggregate(. ~ team, combineddb[,1:16], mean)
totalpoints = aggregate(.~ team,combineddb[,c('team','gameweek_points')],sum)
totalgoalsallowed = aggregate(.~ team,combineddb[,c('team','GoalsAllowed')],sum)
totals = aggregate(. ~ team, combineddb[,1:16], sum)
totals$points = totalpoints$gameweek_points
totals$Goals.Allowed = totalgoalsallowed$GoalsAllowed
totals = inner_join(totals,teamlookup,by = c('team' = 'Abbreviation' ))
totals = totals[c(1,19,2:19)]
remove(totalgoalsallowed)
remove(totalpoints)


#making lag variables for each gameweek id
lag_lookup = combineddb[,c("team_gameweek_id",
                           "team",
                           "gameweek_points",
                           "game_week"
)]
lag_lookup$team_gameweek_id_lag1 = paste0(lag_lookup$team,"-",lag_lookup$game_week-1)
lag_lookup$team_gameweek_id_lag2 = paste0(lag_lookup$team,"-",lag_lookup$game_week-2)
lag_lookup$team_gameweek_id_lag3 = paste0(lag_lookup$team,"-",lag_lookup$game_week-3)

lag_lookup = lag_lookup[,c("team_gameweek_id","team_gameweek_id_lag1","team_gameweek_id_lag2","team_gameweek_id_lag3")]

#unique id and game_week points
team_and_points = combineddb[,c("team_gameweek_id",
                                "gameweek_points"
)]

#finding points for each week, last 3 weeks
library(dplyr)
lag_lookup = left_join(lag_lookup, team_and_points, by = c("team_gameweek_id_lag1" = "team_gameweek_id"))
colnames(lag_lookup)[5]="points_lag1"
lag_lookup = left_join(lag_lookup, team_and_points, by = c("team_gameweek_id_lag2" = "team_gameweek_id"))
colnames(lag_lookup)[6]="points_lag2"
lag_lookup = left_join(lag_lookup, team_and_points, by = c("team_gameweek_id_lag3" = "team_gameweek_id"))
colnames(lag_lookup)[7]="points_lag3"

#unique id and opponent standing
team_and_opp_rank = combineddb[,c("team_gameweek_id",
                                  "opp_standing_rank"
)]

#finding standing difference for each week, last 3 weeks
lag_lookup = left_join(lag_lookup, team_and_opp_rank, by = c("team_gameweek_id_lag1" = "team_gameweek_id"))
colnames(lag_lookup)[8]="opponent_standing_lag1"
lag_lookup = left_join(lag_lookup, team_and_opp_rank, by = c("team_gameweek_id_lag2" = "team_gameweek_id"))
colnames(lag_lookup)[9]="opponent_standing_lag2"
lag_lookup = left_join(lag_lookup, team_and_opp_rank, by = c("team_gameweek_id_lag3" = "team_gameweek_id"))
colnames(lag_lookup)[10]="opponent_standing_lag3"

#only grabbing columns we want
lag_lookup = lag_lookup[,c("team_gameweek_id",
                           "points_lag1",
                           "points_lag2",
                           "points_lag3",
                           "opponent_standing_lag1",
                           "opponent_standing_lag2",
                           "opponent_standing_lag3")]
rm(team_and_opp_rank,team_and_points)

#joining combineddb
combineddb = left_join(combineddb,lag_lookup, by= c("team_gameweek_id"))
# loading team lookup and fixtures
teamlookup = read.csv('TeamLookup.csv')
fixtures = read.csv('EPL-Fixtures-2017-18-Excel.csv')

# exporting data
#setwd("/Users/admin/Dropbox/dataprojects/epl/exports")
#write.csv(combineddb, file = paste0(Sys.Date(),"_df.csv"))

beepr::beep(sound=1)
