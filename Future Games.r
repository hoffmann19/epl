for(i in 1:1){
  temptable = fixtures[i,]
  #temptable = inner_join(temptable,totals, by=c("team"= "team"))
  matchnumber = match(temptable$HOME.TEAM[1], totals$Team.Name)
  temptable$homematchscore = (((totals$Goals[matchnumber]-totals$Goals.Allowed[matchnumber])*.10 +totals$points[matchnumber])*1.2)
  #temptable$homegoals.allowed = totals$Goals.Allowed[matchnumber]
  matchnumber = match(temptable$AWAY.TEAM[1], totals$Team.Name)
  #temptable$awaygoals.allowed = totals$Goals.Allowed[matchnumber]
  temptable$awaymatchscore = (((totals$Goals[matchnumber]-totals$Goals.Allowed[matchnumber])*.10 +totals$points[matchnumber])*.85)
  temptable$homeprob = (temptable$homematchscore- temptable$awaymatchscore)
}


set.seed(1)
sample(c("win","loss","draw"), size=10, replace=TRUE, prob=c(0.2,0.6,0.2))

team =4
totals$Team.Name[team][1]
testscore = ((totals$Goals[team]-totals$Goals.Allowed[team])*.6+totals$points[team])
testscore
