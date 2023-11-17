library(dplyr)
library(tidyr)
library(ggplot2)

setwd("~/Documents/PhD 2020/BeHo:ZiBA study/Data analysis/Tube test data analysis/R_input_files")
tube_test = read.csv("Tube test results.csv", header = TRUE)

## dividing the data by cage ##

cage5M = data.frame()
cage4M = data.frame()
cage6M = data.frame()
cage1M = data.frame()
cage12F = data.frame()
cage15F = data.frame()
cage16F = data.frame()
cage14F = data.frame()
#  c("Name","Test.code","Test.number","Winner", "Looser", "Winner.position","Duration (seconds)", "Notes", "Video file","Date"))

for (i in 1:nrow(tube_test)){
  if (substr(tube_test$Test.code[i],0,3)== "05M"){
    cage5M= rbind(cage5M,c(tube_test[i,]))
  }
  if (substr(tube_test$Test.code[i],0,3)== "06M"){
    cage6M= rbind(cage6M,c(tube_test[i,]))
  }
  if (substr(tube_test$Test.code[i],0,3)== "04M"){
    cage4M= rbind(cage4M,c(tube_test[i,]))
  }
  if (substr(tube_test$Test.code[i],0,3)== "01M"){
    cage1M= rbind(cage1M,c(tube_test[i,]))
  }
  if (substr(tube_test$Test.code[i],0,3)== "14F"){
    cage14F= rbind(cage14F,c(tube_test[i,]))
  }
  if (substr(tube_test$Test.code[i],0,3)== "15F"){
    cage15F= rbind(cage15F,c(tube_test[i,]))
  }
  if (substr(tube_test$Test.code[i],0,3)== "12F"){
    cage12F= rbind(cage12F,c(tube_test[i,]))
  }
  if (substr(tube_test$Test.code[i],0,3)== "16F"){
    cage16F= rbind(cage16F,c(tube_test[i,]))
  }
}

## dividing by treatment ## I removed the empty wins

heat = data.frame()
cold = data.frame()
diet = data.frame()
antibio = data.frame()
fmt = data.frame()
opt = data.frame()


for (x in 1:nrow(tube_test)){
  if (substr(tube_test$Test.code[x],5,7)== "OPT" & tube_test$Winner[x]!=""){
    opt= rbind(opt,c(tube_test[x,]))
  }
  if (substr(tube_test$Test.code[x],5,8)== "HEAT" & tube_test$Winner[x]!=""){
    heat= rbind(heat,c(tube_test[x,]))
  }
  if (substr(tube_test$Test.code[x],5,8)== "COLD" & tube_test$Winner[x]!=""){
    cold= rbind(cold,c(tube_test[x,]))
  }
  if (substr(tube_test$Test.code[x],5,8)== "DIET" & tube_test$Winner[x]!=""){
    diet= rbind(diet,c(tube_test[x,]))
  }
  if (substr(tube_test$Test.code[x],5,8)== "ANTI" & tube_test$Winner[x]!=""){
    antibio= rbind(antibio,c(tube_test[x,]))
  }
  if (substr(tube_test$Test.code[x],5,7)== "FMT" & tube_test$Winner[x]!=""){
    fmt= rbind(fmt,c(tube_test[x,]))
  }
}

## wins counts per mouse in each treatment ##

wins_in_opt <- count(opt, opt$Winner)
names(wins_in_opt)[1]<-"Winner"
wins_in_opt_tot <- merge(wins_in_opt,opt,)

wins_in_heat <- count(heat, heat$Winner)
names(wins_in_heat)[1]<-"Winner"
wins_in_heat_tot <- merge(wins_in_heat,heat,)

wins_in_cold <- count(cold, cold$Winner)
names(wins_in_cold)[1]<-"Winner"
wins_in_cold_tot <- merge(wins_in_cold,cold,)

wins_in_diet <- count(diet, diet$Winner)
names(wins_in_diet)[1]<-"Winner"
wins_in_diet_tot <- merge(wins_in_diet,diet,)

wins_in_anti <- count(antibio, antibio$Winner)
names(wins_in_anti)[1]<-"Winner"
wins_in_anti_tot <- merge(wins_in_anti,antibio,)

wins_in_fmt <- count(fmt, fmt$Winner)
names(wins_in_fmt)[1]<-"Winner"
wins_in_fmt_tot <- merge(wins_in_fmt,fmt,)

winner_stats <- Reduce(function(x,y) merge(x = x, y = y, by = "Winner", all = TRUE), 
                       list(wins_in_opt,wins_in_heat,wins_in_cold,wins_in_diet,wins_in_anti,wins_in_fmt))
colnames(winner_stats)<-c("Winner","Wins in opt","Wins in heat","Wins in cold","Wins in diet","Wins in anti","Wins in fmt")
winner_stats$Totals <- rowSums(winner_stats[2:7], na.rm = TRUE)

winner_stats[is.na(winner_stats)] <- 0  #to replace the NA with 0 to plot it
par(mar=c(3,3,3,3))
barplot(colMeans(winner_stats[,2:7]), main= "Mean wins per treatment",las=2, ylab= "n of wins", cex.axis = 0.5)

plot(unlist(winner_stats[1,2:7]), type="l",ylim = c(0,20), col=1,main="wins per mouse per treatment", ylab= "n of wins")

for (y in 2:nrow(winner_stats)){
  vec<-unlist(winner_stats[y,2:7])
  lines(vec, col=y%/%5+1)
  par(mar=c(1,1,1,1))
}

lines(colMeans(winner_stats[,2:7]), col="red", lwd=4, lty=2)


##now I do the same cage by cage##

heat12F = data.frame()
cold12F = data.frame()
diet12F = data.frame()
antibio12F = data.frame()
fmt12F = data.frame()
opt12F = data.frame()

for (x in 1:nrow(cage12F)){
  if (substr(cage12F$Test.code[x],5,7)== "OPT" & cage12F$Winner[x]!=""){
    opt12F= rbind(opt12F,c(cage12F[x,]))
  }
  if (substr(cage12F$Test.code[x],5,8)== "HEAT" & cage12F$Winner[x]!=""){
    heat12F= rbind(heat12F,c(cage12F[x,]))
  }
  if (substr(cage12F$Test.code[x],5,8)== "COLD" & cage12F$Winner[x]!=""){
    cold12F= rbind(cold12F,c(cage12F[x,]))
  }
  if (substr(cage12F$Test.code[x],5,8)== "DIET" & cage12F$Winner[x]!=""){
    diet12F= rbind(diet12F,c(cage12F[x,]))
  }
  if (substr(cage12F$Test.code[x],5,8)== "ANTI" & cage12F$Winner[x]!=""){
    antibio12F= rbind(antibio12F,c(cage12F[x,]))
  }
  if (substr(cage12F$Test.code[x],5,7)== "FMT" & cage12F$Winner[x]!=""){
    fmt12F= rbind(fmt12F,c(cage12F[x,]))
  }
}

## wins counts per mouse in each treatment for cage 12F ##

wins_in_opt12F <- count(opt12F, opt12F$Winner)
names(wins_in_opt12F)[1]<-"Winner"
wins_in_opt12F_tot <- merge(wins_in_opt12F,opt12F,)

wins_in_heat12F <- count(heat12F, heat12F$Winner)
names(wins_in_heat12F)[1]<-"Winner"
wins_in_heat12F_tot <- merge(wins_in_heat12F,heat12F,)

wins_in_cold12F <- count(cold12F, cold12F$Winner)
names(wins_in_cold12F)[1]<-"Winner"
wins_in_cold12F_tot <- merge(wins_in_cold12F,cold12F,)

wins_in_diet12F <- count(diet12F, diet12F$Winner)
names(wins_in_diet12F)[1]<-"Winner"
wins_in_diet12F_tot <- merge(wins_in_diet12F,diet12F,)

wins_in_anti12F <- count(antibio12F, antibio12F$Winner)
names(wins_in_anti12F)[1]<-"Winner"
wins_in_anti12F_tot <- merge(wins_in_anti12F,antibio12F,)

wins_in_fmt12F <- count(fmt12F, fmt12F$Winner)
names(wins_in_fmt12F)[1]<-"Winner"
wins_in_fmt12F_tot <- merge(wins_in_fmt12F,fmt12F,)


winner_stats_C12F <- Reduce(function(x,y) merge(x = x, y = y, by = "Winner", all = TRUE), 
                       list(wins_in_opt12F,wins_in_heat12F,wins_in_cold12F,wins_in_diet12F,wins_in_anti12F,wins_in_fmt12F))
colnames(winner_stats_C12F)<-c("Winner","Wins in opt12F","Wins in heat12F","Wins in cold12F","Wins in diet12F","Wins in anti12F","Wins in fmt12F")
winner_stats_C12F$Totals <- rowSums(winner_stats_C12F[2:7], na.rm = TRUE)

winner_stats_C12F[is.na(winner_stats_C12F)] <- 0

par(mar=c(8, 3, 3, 1)) #setting that works for me/Luisa
barplot(colMeans(winner_stats_C12F[,2:7]), main= "Mean wins per treatment in Cage 12F",las=2, ylab= "n of wins", cex.axis = 0.5)

plot(unlist(winner_stats_C12F[1,2:7]), ylim = c(0,20), type="l", col=1 ,main="wins per mouse per treatment cage 12F", xlab= "treatment",ylab= "n of wins",xaxt = "n", yaxp = c(0, 20, 4))
axis(1,at =c(1,2,3,4,5,6),labels=c("Wins in opt12F","Wins in heat12F","Wins in cold12F","Wins in diet12F","Wins in anti12F","Wins in fmt12F"))


vec<-c()
for (z in 2:nrow(winner_stats_C12F)){
  vec<-unlist(winner_stats_C12F[z,2:7])
  lines(vec, col=z)
}
lines(colMeans(winner_stats_C12F[,2:7]), col=z+1, lwd=3, lty=2)


## divide the opt treatment by replicates ##

optR1 = data.frame()
optR2 = data.frame()
optR3 = data.frame()
optR4 = data.frame()

for (x in 1:nrow(opt)){
  if (substr(opt$Test.code[x],9,10)== "R1" & opt$Winner[x]!=""){
    optR1= rbind(optR1,c(opt[x,]))
  }
  if (substr(opt$Test.code[x],9,10)== "R2" & opt$Winner[x]!=""){
    optR2= rbind(optR2,c(opt[x,]))
  }
  if (substr(opt$Test.code[x],9,10)== "R3" & opt$Winner[x]!=""){
    optR3= rbind(optR3,c(opt[x,]))
  }
  if (substr(opt$Test.code[x],9,10)== "R4" & opt$Winner[x]!=""){
    optR4= rbind(optR4,c(opt[x,]))
  }
}


