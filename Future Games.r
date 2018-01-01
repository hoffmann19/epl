#basic little loop to simulate game outcome
outcome = data.frame()
for(i in 1:nrow(fixtures)){
  print(i)
  temptable = fixtures[i,]
  #temptable = inner_join(temptable,totals, by=c("team"= "team"))
  matchnumber = match(temptable$HOME.TEAM[1], totals$Team.Name)
  temptable$homematchscore = (((totals$Goals[matchnumber]-totals$Goals.Allowed[matchnumber])*.10 +totals$points[matchnumber])*1.2)
  #temptable$homegoals.allowed = totals$Goals.Allowed[matchnumber]
  matchnumber = match(temptable$AWAY.TEAM[1], totals$Team.Name)
  #temptable$awaygoals.allowed = totals$Goals.Allowed[matchnumber]
  temptable$awaymatchscore = (((totals$Goals[matchnumber]-totals$Goals.Allowed[matchnumber])*.10 +totals$points[matchnumber])*.85)
  drawodds = abs(abs(temptable$awaymatchscore - temptable$homematchscore)-100)*.007
  temptable$outcome = sample(c("home","away","draw"), size=1, replace=TRUE, prob=c((temptable$homematchscore[1]*.01),(temptable$awaymatchscore[1]*.01),drawodds))
  outcome = rbind(outcome,temptable)
}
#write.csv(outcome,'testoutput.csv')

