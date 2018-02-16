library(rvest)
library(tidyverse)
library(stringr)
library(dplyr)

#finding all team numbers
base_url <- read_html('http://scores.nbcsports.com/epl/teams.asp')
team_list <- base_url %>% html_nodes('.shsNumD') %>% html_node('a') %>% html_attr('href')
team_list <- paste0('http://scores.nbcsports.com', team_list)

#crawl through all urls
master_df <- list()
for(url_of_team in team_list){
  df <- data.frame((read_html(url_of_team) %>% html_table())[[4]])
  colnames(df) <- df[1,]
  df <- df[c(3:nrow(df)-1),]
  master_df <- rbind(master_df, df)
}

#cleaner names
colnames(master_df)[1] <- 'player'
colnames(master_df) <- tolower(colnames(master_df))

library(dplyr)

#change data type
str(master_df)
numeric_cols <- colnames(master_df)[3:ncol(master_df)]
master_df[numeric_cols] <- sapply(master_df[numeric_cols], as.numeric)
str(master_df)

#convert all na's to 0
master_df[is.na(master_df)] <- 0

#finding stats per minutes played
metrics <- colnames(master_df)[5:ncol(master_df)]
master_df[metrics] <- master_df[metrics] / master_df$mp

#KNN
library(RANN)
library(tidyr)
knn <- nn2(data = master_df[metrics], k = 4)
master_df[3:ncol(master_df)]
master_df$player_id <- 1:nrow(master_df)
player_lookup <- master_df %>% select(player_id, player)
idx <- data.frame(knn$nn.idx)
dist <- data.frame(knn$nn.dists)

#transforming data
l <- gather(data = idx, key = idx$X1)
l$player_id <- 1:nrow(master_df)
l <- l %>% select(matched_rank = `idx$X1`, player_idx = value, matched_player_idx = player_id)
head(l)
l$matched_rank <- gsub('X',replacement = '',x = l$matched_rank)
unique(l$matched_rank)
head(l)
g<- left_join(l, player_lookup, by=c('player_idx'='player_id'))
g<- left_join(g, player_lookup, by= c("matched_player_idx"="player_id"))
g <- g %>% 
  select(player_idx,player_name = player.x ,matched_player_idx, matched_player_name = player.y,matched_rank) %>%
  filter(matched_rank != 1) %>%
  arrange(player_idx, matched_rank)
g

subset(g, g$player_name == 'Ashley Young')
