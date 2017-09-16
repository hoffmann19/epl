#install.packages("rvest")
library("rvest")
library(data.table)
library(stringr)
library(dplyr)
library(stringi)

Sys.time()
minutes = 0
count=0
z=0
system.time(
  for(i in 1:135) #number of iterations
  {
    starttime = Sys.time()
    url = "http://scores.nbcsports.com/epl/boxscore.asp?gamecode=2017091610039&show=pstats&ref="
    population <- url %>%
      read_html() %>%
      html_nodes(xpath='//*[@id="shsIFBBoxPlayerStats1"]/table[2]') %>%
      html_table(fill=TRUE)
    if(length(population)==0)next
    population <- population[[1]]
    db= population[3:18,1:3]
    db = setNames(data.frame(t(db[,-2])), db[,2])
    colnames(db)[1] <- "team"
    # db$team = str_sub(db$team, start= -3)
    # db$date = str_sub(
    #   str_split_fixed(string = url, pattern = "gamecode=", n= 2)[,2], 
    #   end = 8)
    # db$Opponent[1] = db$team[2]
    # db$Opponent[2] = db$team[1]
    # db$GameID = paste0(db$team[1],'v',db$team[2],db$date[1])
    # db$home_away = if_else(db$team==db$team[1],'home','away')
    db$minute = minutes
    if(count==0){ 
      combineddb = db }
    else {
      combineddb= rbind(combineddb,db) }
    count = count+1
    print(count)
    # Sys.sleep(runif(1, min = 0, max = 0.8)) #not necessary 
    Sys.sleep(60) #interval
    endtime = Sys.time()
    print(starttime)
    print(endtime)
    print(minutes)
    minutes = minutes+1
    
    # beepr::beep(sound = 4)
  })
