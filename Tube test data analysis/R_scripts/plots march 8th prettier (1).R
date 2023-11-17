## Plotting script 2.0 (prettier and tidyer) ##

setwd("~/Documents/PhD 2020/ZiBA:BeHo 2020/Data analysis/Tube test data analysis/R_input_files")
tube_test_wins = read.csv("Mice-full-overview.csv", header = TRUE)

# placing NA for dead mice (C8 mouse 3065, C16 mouse 3183, C7 mouse 2730, C3 mouse 3083, C1 mouse 4176)

tube_test_wins[76, c(32:45,48:49)] = NA
tube_test_wins[38, c(32:45,48:49)] = NA
tube_test_wins[32, c(32:45,48:49)] = NA
tube_test_wins[14, c(32:45,48:49)] = NA
tube_test_wins[4,c(28:45,47:49)] = NA


par(mar=c(6,4,4,11), xpd=TRUE)
## Loop creating plots for the absolute wins per cage in the 4 replicates of OPT

opt_wins <- tube_test_wins[,c(1,5, 11:17)]

## For saving the plots ##
#pdf("Results/OPT R1-R4.pdf", width = 7, height = 10)
#par(mar=c(6,4,4,9), xpd=TRUE, mfrow = c(2,1))

i<-1
while (i!=81){
  matplot(t(opt_wins[c(i:(i+4)),c(3:6)]),type="b",xaxt = "n",xlab = "Replicate", ylab= "n of wins", ylim = c(0,4.5), pch=20)
  axis(1,at =c(1,2,3,4),labels=c("R1","R2","R3","R4"))
  nn <- 5
  legend(x= 4.3, y=3, opt_wins$Name[i:(i+4)], col=seq_len(nn), cex=0.6,fill=seq_len(nn),  bty= "n", xpd=TRUE)    
  title(paste("Absolute wins of ", opt_wins$Cage[i], " in OPT per replicate"))
  i<-i+5
}
#dev.off()

 
## Loop creating plots for the absolute wins per cage in the 5 replicates of FMT ##

fmt_wins <- tube_test_wins[,c(1,5, 36:38,40:41)]

#pdf("Results/FMT R1-R5.pdf", width = 7, height = 10)
#par(mar=c(6,4,4,9), xpd=TRUE, mfrow=c(2,1))
i<-1
while (i!=81){
  matplot(t(fmt_wins[c(i:(i+4)),c(3:7)]),type="b",xaxt = "n",xlab = "Replicate", ylab= "n of wins", ylim = c(0,4.5), pch=20)
  axis(1,at =c(1,2,3,4,5),labels=c("R1","R2","R3","R4","R5"))
  nn <- 5
  legend(x= 5.3, y=3, fmt_wins$Name[i:(i+4)], col=seq_len(nn), cex=0.6, fill=seq_len(nn), bty= "n", xpd=TRUE)    
  title(paste("Absolute wins of ", fmt_wins$Cage[i], " in FMT per replicate"))
  i<-i+5
}
#dev.off()

## Loop creating plots for the absolute wins per cage in the last 3 replicates of FMT ##

#pdf("Results/FMT R3-R5.pdf", width = 7, height = 10)
#par(mar=c(6,4,4,9), xpd=TRUE, mfrow=c(2,1))
i<-1
while (i!=81){
  matplot(t(fmt_wins[c(i:(i+4)),c(5:7)]),type="b",xaxt = "n",xlab = "Replicate", ylab= "n of wins", ylim = c(0,4.5), pch=20)
  axis(1,at =c(1,2,3),labels=c("R3","R4","R5"))
  nn <- 5
  legend(x=3.2, y=3, fmt_wins$Name[i:(i+4)], col=seq_len(nn),cex=0.6,fill=seq_len(nn), bty= "n", xpd= TRUE)    
  title(paste("Absolute wins of ", fmt_wins$Cage[i], " in FMT, replicates R3, R4, R5"))
  i<-i+5
}
#dev.off()

## averages of wins per treatment  ##

avg_wins <- tube_test_wins[,c(1,5, 15, 22, 46:49)]
avg_wins$Cage <- factor(avg_wins$Cage, levels=unique(avg_wins$Cage)) #to keep the order of the cages
avg_wins_by_cage= split(avg_wins, avg_wins$Cage)

par(mar=c(5,4,4,9), xpd=TRUE)

#pdf("Results/Treatments.pdf", width = 7, height = 10)
#par(mar=c(6,4,4,9), xpd=TRUE, mfrow=c(2,1))

for (i in avg_wins_by_cage){
  matplot(t(i[,c(3:8)]),type="b", ,xaxt = "n", ylab="Mean wins",pch=20, ylim=c(0,4))
  axis(1,at =c(1,2,3,4,5,6),labels=c("OPT", "HEAT", "COLD", "DIET", "ANTI", "FMT"))
  nn <- 5
  legend(x= 6.4, y=3, cex=0.55, i$Name, col=seq_len(nn), fill=seq_len(nn), bty= "n", xpd=TRUE)    
  title(paste("Average wins of each mouse of ", i$Cage[1], " per treatment"))
}
#dev.off()
