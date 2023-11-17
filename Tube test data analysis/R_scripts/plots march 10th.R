## Plotting script for missing treatments ##

setwd("~/Documents/PhD 2020/BeHo:ZiBA study/Data analysis/Tube test data analysis")
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
pdf(paste("R_plots/","OPT rep wins per cage.pdf"), width = 10, height = 7)
#par(mar=c(6,4,4,9), xpd=TRUE, mfrow=c(2,1)) #to fit two graphs/cages in one pic

i<-1
while (i!=81){
  matplot(t(opt_wins[c(i:(i+4)),c(3:6)]),type="b",xaxt = "n",xlab = "Replicate", ylab= "n of wins", ylim = c(0,4.5), pch=20)
  axis(1,at =c(1,2,3,4),labels=c("R1","R2","R3","R4"))
  nn <- 5
  legend(x= 4.3, y=3, opt_wins$Name[i:(i+4)], col=seq_len(nn), cex=0.6,fill=seq_len(nn),  bty= "n", xpd=TRUE)    
  title(paste("Absolute wins of ", opt_wins$Cage[i], " in OPT per replicate"))
  i<-i+5
}
dev.off()


## Loop creating plots for the absolute wins per cage in the replicates of HEAT

heat_wins <- tube_test_wins[,c(1,5, 18:23)]

## For saving the plots ##
pdf(paste("R_plots/","HEAT rep wins per cage.pdf"), width = 10, height = 7)
#par(mar=c(6,4,4,9), xpd=TRUE, mfrow=c(2,1)) #to fit two graphs/cages in one pic

i<-1
while (i!=81){
  matplot(t(heat_wins[c(i:(i+4)),c(3:5)]),type="b",xaxt = "n",xlab = "Replicate", ylab= "n of wins", ylim = c(0,4.5), pch=20)
  axis(1,at =c(1,2,3),labels=c("R1","R2","R3"))
  nn <- 5
  legend(x= 3.2, y=3, heat_wins$Name[i:(i+4)], col=seq_len(nn), cex=0.6,fill=seq_len(nn),  bty= "n", xpd=TRUE)    
  title(paste("Absolute wins of ", heat_wins$Cage[i], " in HEAT per replicate"))
  i<-i+5
}
dev.off()


## Loop creating plots for the absolute wins per cage in the replicates of COLD ##

cold_wins <- tube_test_wins[,c(1,5, c(24:27, 46))]

## For saving the plots ##
pdf(paste("R_plots/","COLD rep wins per cage.pdf"), width = 10, height = 7)
#par(mar=c(6,4,4,9), xpd=TRUE, mfrow=c(2,1)) #to fit two graphs/cages in one pic

i<-1
while (i!=81){
  matplot(t(cold_wins[c(i:(i+4)),c(3:5)]),type="b",xaxt = "n",xlab = "Replicate", ylab= "n of wins", ylim = c(0,4.5), pch=20)
  axis(1,at =c(1,2,3),labels=c("R1","R2","R3"))
  nn <- 5
  legend(x= 3.2, y=3, cold_wins$Name[i:(i+4)], col=seq_len(nn), cex=0.6,fill=seq_len(nn),  bty= "n", xpd=TRUE)    
  title(paste("Absolute wins of ", cold_wins$Cage[i], " in COLD per replicate"))
  i<-i+5
}
dev.off()


## Loop creating plots for the absolute wins per cage in the replicates of DIET ##

diet_wins <- tube_test_wins[,c(1,5,c(28:31, 47))]

## For saving the plots ##
pdf(paste("R_plots/","DIET rep wins per cage.pdf"), width = 10, height = 7)
#par(mar=c(6,4,4,9), xpd=TRUE, mfrow=c(2,1)) #to fit two graphs/cages in one pic

i<-1
while (i!=81){
  matplot(t(diet_wins[c(i:(i+4)),c(3:5)]),type="b",xaxt = "n",xlab = "Replicate", ylab= "n of wins", ylim = c(0,4.5), pch=20)
  axis(1,at =c(1,2,3),labels=c("R1","R2","R3"))
  nn <- 5
  legend(x= 3.2, y=3, diet_wins$Name[i:(i+4)], col=seq_len(nn), cex=0.6,fill=seq_len(nn),  bty= "n", xpd=TRUE)    
  title(paste("Absolute wins of ", diet_wins$Cage[i], " in DIET per replicate"))
  i<-i+5
}
dev.off()


## Loop creating plots for the absolute wins per cage in the replicates of ANTI ##

anti_wins <- tube_test_wins[,c(1,5,c(32:35, 48))]

## For saving the plots ##
pdf(paste("R_plots/","ANTI rep wins per cage.pdf"), width = 10, height = 7)
#par(mar=c(6,4,4,9), xpd=TRUE, mfrow=c(2,1)) #to fit two graphs/cages in one pic

i<-1
while (i!=81){
  matplot(t(anti_wins[c(i:(i+4)),c(3:5)]),type="b",xaxt = "n",xlab = "Replicate", ylab= "n of wins", ylim = c(0,4.5), pch=20)
  axis(1,at =c(1,2,3),labels=c("R1","R2","R3"))
  nn <- 5
  legend(x= 3.2, y=3, anti_wins$Name[i:(i+4)], col=seq_len(nn), cex=0.6,fill=seq_len(nn),  bty= "n", xpd=TRUE)    
  title(paste("Absolute wins of ", anti_wins$Cage[i], " in ANTI per replicate"))
  i<-i+5
}
dev.off()


## Loop creating plots for the absolute wins per cage in the replicates of FMT ##

FMT_wins <- tube_test_wins[,c(1,5,c(38,40,41,45,49))]

## For saving the plots ##
pdf(paste("R_plots/","FMT rep wins per cage.pdf"), width = 10, height = 7)
#par(mar=c(6,4,4,9), xpd=TRUE, mfrow=c(2,1)) #to fit two graphs/cages in one pic

i<-1
while (i!=81){
  matplot(t(anti_wins[c(i:(i+4)),c(3:5)]),type="b",xaxt = "n",xlab = "Replicate", ylab= "n of wins", ylim = c(0,4.5), pch=20)
  axis(1,at =c(1,2,3),labels=c("R1","R2","R3"))
  nn <- 5
  legend(x= 3.2, y=3, anti_wins$Name[i:(i+4)], col=seq_len(nn), cex=0.6,fill=seq_len(nn),  bty= "n", xpd=TRUE)    
  title(paste("Absolute wins of ", anti_wins$Cage[i], " in ANTI per replicate"))
  i<-i+5
}
dev.off()



#############################################

### Looking into correlations ###
#Used this website here among others: http://www.sthda.com/english/wiki/correlation-test-between-two-variables-in-r#compute-correlation-in-r

tube_test_wins_raw = read.csv("Mice-full-overview.csv", header = TRUE)
tube_test_wins_raw <- tube_test_wins_raw[order(tube_test_wins_raw$Name),]

plotting_all <- tube_test_wins_raw[,c(1,9,15,22,46:50)]
plot(plotting_all)
plotting_all_matrix <- data.matrix(plotting_all) #turn the dataframe into a matrix
correlations <- cor(plotting_all_matrix)
plot(correlations) #this plot makes no sense. Shows no useful info.

library(Hmisc)
rescor<-rcorr(plotting_all_matrix, type = c("pearson","spearman"))
rescor$r
rescor$P

# ++++++++++++++++++++++++++++
# flattenCorrMatrix
# ++++++++++++++++++++++++++++
# cormat : matrix of the correlation coefficients
# pmat : matrix of the correlation p-values
flattenCorrMatrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut],
    p = pmat[ut]
  )
}

flattenCorrMatrix(rescor$r, rescor$P)

library("PerformanceAnalytics")
chart.Correlation(plotting_all_matrix[,c(3:8)], histogram=TRUE, pch=19) #method = pearson by default
chart.Correlation(plotting_all_matrix[,c(3:8)], histogram=TRUE, pch=19, method="spearman")




