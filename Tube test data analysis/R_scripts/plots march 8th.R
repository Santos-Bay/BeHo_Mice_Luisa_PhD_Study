## csv file with all the tube test statistics calculated by airtable ##


setwd("~/Documents/PhD 2020/ZiBA:BeHo 2020/Data analysis/Tube test data analysis")
tube_test_wins = read.csv("Mice-full-overview.csv", header = TRUE)

## Plotting the tube test replicates of OPT (acclimation) ##

opt_wins <- tube_test_wins[,c(1,5,11:17)]

#Cage01M (saving the plot as a pdf too)
pdf(paste("R_plots/","Wins test.pdf"), width = 10, height = 7)
matplot(t(opt_wins[c(1:5),c(3:6)]),type="l", ,xaxt = "n")
axis(1,at =c(1,2,3,4),labels=c("R1","R2","R3","R4"))
nn <- 5
legend("bottomright", inset=.02, opt_wins$Name[1:5], col=seq_len(nn),cex=0.5,fill=seq_len(nn))    
dev.off() 

#Cage02M
matplot(t(opt_wins[c(6:10),c(3:6)]),type="l", ,xaxt = "n")
axis(1,at =c(1,2,3,4),labels=c("R1","R2","R3","R4"))
nn <- 5
legend("bottomright", inset=.02, opt_wins$Name[6:10], col=seq_len(nn),cex=0.5,fill=seq_len(nn))    

#Cage03M
matplot(t(opt_wins[c(11:15),c(3:6)]),type="l", ,xaxt = "n")
axis(1,at =c(1,2,3,4),labels=c("R1","R2","R3","R4"))
nn <- 5
legend("bottomright", inset=.02, opt_wins$Name[11:15], col=seq_len(nn),cex=0.5,fill=seq_len(nn))    

#Cage04M
matplot(t(opt_wins[c(16:20),c(3:6)]),type="l", ,xaxt = "n")
axis(1,at =c(1,2,3,4),labels=c("R1","R2","R3","R4"))
nn <- 5
legend("bottomright", inset=.02, opt_wins$Name[16:20], col=seq_len(nn),cex=0.5,fill=seq_len(nn))    

#Cage05M
matplot(t(opt_wins[c(21:25),c(3:6)]),type="l", ,xaxt = "n")
axis(1,at =c(1,2,3,4),labels=c("R1","R2","R3","R4"))
nn <- 5
legend("bottomright", inset=.02, opt_wins$Name[21:25], col=seq_len(nn),cex=0.5,fill=seq_len(nn))    

#Cage06M
matplot(t(opt_wins[c(26:30),c(3:6)]),type="l", ,xaxt = "n")
axis(1,at =c(1,2,3,4),labels=c("R1","R2","R3","R4"))
nn <- 5
legend("bottomright", inset=.02, opt_wins$Name[26:30], col=seq_len(nn),cex=0.5,fill=seq_len(nn))    

#Cage07M
matplot(t(opt_wins[c(31:35),c(3:6)]),type="l", ,xaxt = "n")
axis(1,at =c(1,2,3,4),labels=c("R1","R2","R3","R4"))
nn <- 5
legend("bottomright", inset=.02, opt_wins$Name[31:35], col=seq_len(nn),cex=0.5,fill=seq_len(nn))    

#Cage08M
matplot(t(opt_wins[c(36:40),c(3:6)]),type="l", ,xaxt = "n")
axis(1,at =c(1,2,3,4),labels=c("R1","R2","R3","R4"))
nn <- 5
legend("bottomright", inset=.02, opt_wins$Name[36:40], col=seq_len(nn),cex=0.5,fill=seq_len(nn))    

#Cage09F
matplot(t(opt_wins[c(41:45),c(3:6)]),type="l", ,xaxt = "n")
axis(1,at =c(1,2,3,4),labels=c("R1","R2","R3","R4"))
nn <- 5
legend("bottomright", inset=.02, opt_wins$Name[41:45], col=seq_len(nn),cex=0.5,fill=seq_len(nn))    
#Cage10F
matplot(t(opt_wins[c(46:50),c(3:6)]),type="l", ,xaxt = "n")
axis(1,at =c(1,2,3,4),labels=c("R1","R2","R3","R4"))
nn <- 5
legend("bottomright", inset=.02, opt_wins$Name[46:50], col=seq_len(nn),cex=0.5,fill=seq_len(nn))    
#Cage11F
matplot(t(opt_wins[c(51:55),c(3:6)]),type="l", ,xaxt = "n")
axis(1,at =c(1,2,3,4),labels=c("R1","R2","R3","R4"))
nn <- 5
legend("bottomright", inset=.02, opt_wins$Name[51:55], col=seq_len(nn),cex=0.5,fill=seq_len(nn))    
#Cage12F
matplot(t(opt_wins[c(56:60),c(3:6)]),type="l", ,xaxt = "n")
axis(1,at =c(1,2,3,4),labels=c("R1","R2","R3","R4"))
nn <- 5
legend("bottomright", inset=.02, opt_wins$Name[56:60], col=seq_len(nn),cex=0.5,fill=seq_len(nn))    
#Cage13F
matplot(t(opt_wins[c(61:65),c(3:6)]),type="l", ,xaxt = "n")
axis(1,at =c(1,2,3,4),labels=c("R1","R2","R3","R4"))
nn <- 5
legend("bottomright", inset=.02, opt_wins$Name[61:65], col=seq_len(nn),cex=0.5,fill=seq_len(nn))    

#Cage14F
matplot(t(opt_wins[c(66:70),c(3:6)]),type="l", ,xaxt = "n")
axis(1,at =c(1,2,3,4),labels=c("R1","R2","R3","R4"))
nn <- 5
legend("bottomright", inset=.02, opt_wins$Name[66:70], col=seq_len(nn),cex=0.5,fill=seq_len(nn))    

#Cage15F
matplot(t(opt_wins[c(71:75),c(3:6)]),type="l", ,xaxt = "n")
axis(1,at =c(1,2,3,4),labels=c("R1","R2","R3","R4"))
nn <- 5
legend("bottomright", inset=.02, opt_wins$Name[71:75], col=seq_len(nn),cex=0.5,fill=seq_len(nn))    

#Cage16F
matplot(t(opt_wins[c(76:80),c(3:6)]),type="l", ,xaxt = "n")
axis(1,at =c(1,2,3,4),labels=c("R1","R2","R3","R4"))
nn <- 5
legend("bottomright", inset=.02, opt_wins$Name[76:80], col=seq_len(nn),cex=0.5,fill=seq_len(nn))    


## FMT replicates ##


fmt_wins <- tube_test_wins[,c(1,5, 36:38,40:41)]
#Cage01M FMT
matplot(t(fmt_wins[c(1:5),c(3:7)]),type="l", ,xaxt = "n")
axis(1,at =c(1,2,3,4,5),labels=c("R1","R2","R3","R4","R5"))
nn <- 5
legend("bottomright", inset=.02, fmt_wins$Name[1:5], col=seq_len(nn),cex=0.5,fill=seq_len(nn))    

#Cage02M FMT
matplot(t(fmt_wins[c(6:10),c(3:7)]),type="l", ,xaxt = "n")
axis(1,at =c(1,2,3,4,5),labels=c("R1","R2","R3","R4","R5"))
nn <- 5
legend("bottomright", inset=.02, fmt_wins$Name[6:10], col=seq_len(nn),cex=0.5,fill=seq_len(nn))    

#Cage03M FMT
matplot(t(fmt_wins[c(11:15),c(3:7)]),type="l", ,xaxt = "n")
axis(1,at =c(1,2,3,4,5),labels=c("R1","R2","R3","R4","R5"))
nn <- 5
legend("bottomright", inset=.02, fmt_wins$Name[11:15], col=seq_len(nn),cex=0.5,fill=seq_len(nn))    

#Cage04M FMT
matplot(t(fmt_wins[c(16:20),c(3:7)]),type="l", ,xaxt = "n")
axis(1,at =c(1,2,3,4,5),labels=c("R1","R2","R3","R4","R5"))
nn <- 5
legend("bottomright", inset=.02, fmt_wins$Name[16:20], col=seq_len(nn),cex=0.5,fill=seq_len(nn))    

#Cage05M FMT
matplot(t(fmt_wins[c(21:25),c(3:7)]),type="l", ,xaxt = "n")
axis(1,at =c(1,2,3,4,5),labels=c("R1","R2","R3","R4","R5"))
nn <- 5
legend("bottomright", inset=.02, fmt_wins$Name[21:25], col=seq_len(nn),cex=0.5,fill=seq_len(nn))    

#Cage06M FMT
matplot(t(fmt_wins[c(26:30),c(3:7)]),type="l", ,xaxt = "n")
axis(1,at =c(1,2,3,4,5),labels=c("R1","R2","R3","R4","R5"))
nn <- 5
legend("bottomright", inset=.02, fmt_wins$Name[26:30], col=seq_len(nn),cex=0.5,fill=seq_len(nn))    

#Cage07M FMT
matplot(t(fmt_wins[c(31:35),c(3:7)]),type="l", ,xaxt = "n")
axis(1,at =c(1,2,3,4,5),labels=c("R1","R2","R3","R4","R5"))
nn <- 5
legend("bottomright", inset=.02, fmt_wins$Name[31:35], col=seq_len(nn),cex=0.5,fill=seq_len(nn))    

#Cage08M FMT
matplot(t(fmt_wins[c(36:40),c(3:7)]),type="l", ,xaxt = "n")
axis(1,at =c(1,2,3,4,5),labels=c("R1","R2","R3","R4","R5"))
nn <- 5
legend("bottomright", inset=.02, fmt_wins$Name[36:40], col=seq_len(nn),cex=0.5,fill=seq_len(nn))    

#Cage09F FMT
matplot(t(fmt_wins[c(41:45),c(3:7)]),type="l", ,xaxt = "n")
axis(1,at =c(1,2,3,4,5),labels=c("R1","R2","R3","R4","R5"))
nn <- 5
legend("bottomright", inset=.02, fmt_wins$Name[41:45], col=seq_len(nn),cex=0.5,fill=seq_len(nn))    
#Cage10F FMT
matplot(t(fmt_wins[c(46:50),c(3:7)]),type="l", ,xaxt = "n")
axis(1,at =c(1,2,3,4,5),labels=c("R1","R2","R3","R4","R5"))
nn <- 5
legend("bottomright", inset=.02, fmt_wins$Name[46:50], col=seq_len(nn),cex=0.5,fill=seq_len(nn))    
#Cage11F FMT
matplot(t(fmt_wins[c(51:55),c(3:7)]),type="l", ,xaxt = "n")
axis(1,at =c(1,2,3,4,5),labels=c("R1","R2","R3","R4","R5"))
nn <- 5
legend("bottomright", inset=.02, fmt_wins$Name[51:55], col=seq_len(nn),cex=0.5,fill=seq_len(nn))    
#Cage12F FMT
matplot(t(fmt_wins[c(56:60),c(3:7)]),type="l", ,xaxt = "n")
axis(1,at =c(1,2,3,4,5),labels=c("R1","R2","R3","R4","R5"))
nn <- 5
legend("bottomright", inset=.02, fmt_wins$Name[56:60], col=seq_len(nn),cex=0.5,fill=seq_len(nn))    
#Cage13F FMT
matplot(t(fmt_wins[c(61:65),c(3:7)]),type="l", ,xaxt = "n")
axis(1,at =c(1,2,3,4,5),labels=c("R1","R2","R3","R4","R5"))
nn <- 5
legend("bottomright", inset=.02, fmt_wins$Name[61:65], col=seq_len(nn),cex=0.5,fill=seq_len(nn))    

#Cage14F FMT
matplot(t(fmt_wins[c(66:70),c(3:7)]),type="l", ,xaxt = "n")
axis(1,at =c(1,2,3,4,5),labels=c("R1","R2","R3","R4","R5"))
nn <- 5
legend("bottomright", inset=.02, fmt_wins$Name[66:70], col=seq_len(nn),cex=0.5,fill=seq_len(nn))    

#Cage15F FMT
matplot(t(fmt_wins[c(71:75),c(3:7)]),type="l", ,xaxt = "n")
axis(1,at =c(1,2,3,4,5),labels=c("R1","R2","R3","R4","R5"))
nn <- 5
legend("bottomright", inset=.02, fmt_wins$Name[71:75], col=seq_len(nn),cex=0.5,fill=seq_len(nn))    

#Cage16F FMT
matplot(t(fmt_wins[c(76:80),c(3:7)]),type="l", ,xaxt = "n")
axis(1,at =c(1,2,3,4,5),labels=c("R1","R2","R3","R4","R5"))
nn <- 5
legend("bottomright", inset=.02, fmt_wins$Name[76:80], col=seq_len(nn),cex=0.5,fill=seq_len(nn))    






#Cage01M FMT last 3 replicates
matplot(t(fmt_wins[c(1:5),c(5:7)]),type="l", ,xaxt = "n")
axis(1,at =c(1,2,3),labels=c("R3","R4","R5"))
nn <- 5
legend("bottomright", inset=.02, fmt_wins$Name[1:5], col=seq_len(nn),cex=0.5,fill=seq_len(nn))    

#Cage02M FMT last 3 replicates
matplot(t(fmt_wins[c(6:10),c(5:7)]),type="l", ,xaxt = "n")
axis(1,at =c(1,2,3),labels=c("R3","R4","R5"))
nn <- 5
legend("bottomright", inset=.02, fmt_wins$Name[6:10], col=seq_len(nn),cex=0.5,fill=seq_len(nn))    

#Cage03M FMT last 3 replicates
matplot(t(fmt_wins[c(11:15),c(5:7)]),type="l", ,xaxt = "n")
axis(1,at =c(1,2,3),labels=c("R3","R4","R5"))
nn <- 5
legend("bottomright", inset=.02, fmt_wins$Name[11:15], col=seq_len(nn),cex=0.5,fill=seq_len(nn))    

#Cage04M FMT last 3 replicates
matplot(t(fmt_wins[c(16:20),c(5:7)]),type="l", ,xaxt = "n")
axis(1,at =c(1,2,3),labels=c("R3","R4","R5"))
nn <- 5
legend("bottomright", inset=.02, fmt_wins$Name[16:20], col=seq_len(nn),cex=0.5,fill=seq_len(nn))    

#Cage05M FMT last 3 replicates
matplot(t(fmt_wins[c(21:25),c(5:7)]),type="l", ,xaxt = "n")
axis(1,at =c(1,2,3),labels=c("R3","R4","R5"))
nn <- 5
legend("bottomright", inset=.02, fmt_wins$Name[21:25], col=seq_len(nn),cex=0.5,fill=seq_len(nn))    

#Cage06M FMT last 3 replicates
matplot(t(fmt_wins[c(26:30),c(5:7)]),type="l", ,xaxt = "n")
axis(1,at =c(1,2,3),labels=c("R3","R4","R5"))
nn <- 5
legend("bottomright", inset=.02, fmt_wins$Name[26:30], col=seq_len(nn),cex=0.5,fill=seq_len(nn))    

#Cage07M FMT last 3 replicates
matplot(t(fmt_wins[c(31:35),c(5:7)]),type="l", ,xaxt = "n")
axis(1,at =c(1,2,3),labels=c("R3","R4","R5"))
nn <- 5
legend("bottomright", inset=.02, fmt_wins$Name[31:35], col=seq_len(nn),cex=0.5,fill=seq_len(nn))    

#Cage08M FMT last 3 replicates
matplot(t(fmt_wins[c(36:40),c(5:7)]),type="l", ,xaxt = "n")
axis(1,at =c(1,2,3),labels=c("R3","R4","R5"))
nn <- 5
legend("bottomright", inset=.02, fmt_wins$Name[36:40], col=seq_len(nn),cex=0.5,fill=seq_len(nn))    

#Cage09F FMT last 3 replicates
matplot(t(fmt_wins[c(41:45),c(5:7)]),type="l", ,xaxt = "n")
axis(1,at =c(1,2,3),labels=c("R3","R4","R5"))
nn <- 5
legend("bottomright", inset=.02, fmt_wins$Name[41:45], col=seq_len(nn),cex=0.5,fill=seq_len(nn))    
#Cage10F FMT last 3 replicates
matplot(t(fmt_wins[c(46:50),c(5:7)]),type="l", ,xaxt = "n")
axis(1,at =c(1,2,3),labels=c("R3","R4","R5"))
nn <- 5
legend("bottomright", inset=.02, fmt_wins$Name[46:50], col=seq_len(nn),cex=0.5,fill=seq_len(nn))    
#Cage11F FMT last 3 replicates
matplot(t(fmt_wins[c(51:55),c(5:7)]),type="l", ,xaxt = "n")
axis(1,at =c(1,2,3),labels=c("R3","R4","R5"))
nn <- 5
legend("bottomright", inset=.02, fmt_wins$Name[51:55], col=seq_len(nn),cex=0.5,fill=seq_len(nn))    
#Cage12F FMT last 3 replicates
matplot(t(fmt_wins[c(56:60),c(5:7)]),type="l", ,xaxt = "n")
axis(1,at =c(1,2,3),labels=c("R3","R4","R5"))
nn <- 5
legend("bottomright", inset=.02, fmt_wins$Name[56:60], col=seq_len(nn),cex=0.5,fill=seq_len(nn))    
#Cage13F FMT last 3 replicates
matplot(t(fmt_wins[c(61:65),c(5:7)]),type="l", ,xaxt = "n")
axis(1,at =c(1,2,3),labels=c("R3","R4","R5"))
nn <- 5
legend("bottomright", inset=.02, fmt_wins$Name[61:65], col=seq_len(nn),cex=0.5,fill=seq_len(nn))    

#Cage14F FMT last 3 replicates
matplot(t(fmt_wins[c(66:70),c(5:7)]),type="l", ,xaxt = "n")
axis(1,at =c(1,2,3),labels=c("R3","R4","R5"))
nn <- 5
legend("bottomright", inset=.02, fmt_wins$Name[66:70], col=seq_len(nn),cex=0.5,fill=seq_len(nn))    

#Cage15F FMT last 3 replicates
matplot(t(fmt_wins[c(71:75),c(5:7)]),type="l", ,xaxt = "n")
axis(1,at =c(1,2,3),labels=c("R3","R4","R5"))
nn <- 5
legend("bottomright", inset=.02, fmt_wins$Name[71:75], col=seq_len(nn),cex=0.5,fill=seq_len(nn))    

#Cage16F FMT last 3 replicates
matplot(t(fmt_wins[c(76:80),c(5:7)]),type="l", ,xaxt = "n")
axis(1,at =c(1,2,3),labels=c("R3","R4","R5"))
nn <- 5
legend("bottomright", inset=.02, fmt_wins$Name[76:80], col=seq_len(nn),cex=0.5,fill=seq_len(nn))    



## averages of wins per treatment  ##

avg_wins <- tube_test_wins[,c(1,5, 15, 22, 46:49)]

cage5M = data.frame()
cage4M = data.frame()
cage6M = data.frame()
cage1M = data.frame()
cage2M = data.frame()
cage3M = data.frame()
cage7M = data.frame()
cage8M = data.frame()
cage9F = data.frame()
cage10F = data.frame()
cage11F = data.frame()
cage13F = data.frame()
cage12F = data.frame()
cage15F = data.frame()
cage16F = data.frame()
cage14F = data.frame()

for (i in 1:nrow(avg_wins)){
  if (substr(avg_wins$Name[i],2,4)== "05M"){
    cage5M= rbind(cage5M,c(avg_wins[i,]))
  }
  if (substr(avg_wins$Name[i],2,4)== "06M"){
    cage6M= rbind(cage6M,c(avg_wins[i,]))
  }
  if (substr(avg_wins$Name[i],2,4)== "04M"){
    cage4M= rbind(cage4M,c(avg_wins[i,]))
  }
  if (substr(avg_wins$Name[i],2,4)== "01M"){
    cage1M= rbind(cage1M,c(avg_wins[i,]))
  }
  if (substr(avg_wins$Name[i],2,4)== "14F"){
    cage14F= rbind(cage14F,c(avg_wins[i,]))
  }
  if (substr(avg_wins$Name[i],2,4)== "15F"){
    cage15F= rbind(cage15F,c(avg_wins[i,]))
  }
  if (substr(avg_wins$Name[i],2,4)== "12F"){
    cage12F= rbind(cage12F,c(avg_wins[i,]))
  }
  if (substr(avg_wins$Name[i],2,4)== "16F"){
    cage16F= rbind(cage16F,c(avg_wins[i,]))
  }
  if (substr(avg_wins$Name[i],2,4)== "02M"){
    cage2M= rbind(cage2M,c(avg_wins[i,]))
  }
  if (substr(avg_wins$Name[i],2,4)== "03M"){
    cage3M= rbind(cage3M,c(avg_wins[i,]))
  }
  if (substr(avg_wins$Name[i],2,4)== "07M"){
    cage7M= rbind(cage7M,c(avg_wins[i,]))
  }
  if (substr(avg_wins$Name[i],2,4)== "08M"){
    cage8M= rbind(cage8M,c(avg_wins[i,]))
  }
  if (substr(avg_wins$Name[i],2,4)== "09F"){
    cage9F= rbind(cage9F,c(avg_wins[i,]))
  }
  if (substr(avg_wins$Name[i],2,4)== "10F"){
    cage10F= rbind(cage10F,c(avg_wins[i,]))
  }
  if (substr(avg_wins$Name[i],2,4)== "11F"){
    cage11F= rbind(cage11F,c(avg_wins[i,]))
  }
  if (substr(avg_wins$Name[i],2,4)== "13F"){
    cage13F= rbind(cage13F,c(avg_wins[i,]))
  }
}

#Plot average wins w/in replicates per mouse across treatments

#Cage1M 
matplot(t(cage1M[,c(3:8)]),type="l", ,xaxt = "n")
axis(1,at =c(1,2,3,4,5,6),labels=c("OPT", "HEAT", "COLD", "DIET", "ANTI", "FMT"))
nn <- 5
legend("bottomright", inset=.02, cage1M$Name, col=seq_len(nn),cex=0.5,fill=seq_len(nn))    

#Cage2M 
matplot(t(cage2M[,c(3:8)]),type="l", ,xaxt = "n")
axis(1,at =c(1,2,3,4,5,6),labels=c("OPT", "HEAT", "COLD", "DIET", "ANTI","FMT"))
nn <- 5
legend("bottomright", inset=.02, cage2M$Name, col=seq_len(nn),cex=0.5,fill=seq_len(nn))    

#Cage3M 
matplot(t(cage3M[,c(3:8)]),type="l", ,xaxt = "n")
axis(1,at =c(1,2,3,4,5,6),labels=c("OPT", "HEAT", "COLD", "DIET", "ANTI","FMT"))
nn <- 5
legend("bottomright", inset=.02, cage3M$Name, col=seq_len(nn),cex=0.5,fill=seq_len(nn))    

#Cage4M 
matplot(t(cage4M[,c(3:8)]),type="l", ,xaxt = "n")
axis(1,at =c(1,2,3,4,5,6),labels=c("OPT", "HEAT", "COLD", "DIET", "ANTI", "FMT"))
nn <- 5
legend("bottomright", inset=.02, cage4M$Name, col=seq_len(nn),cex=0.5,fill=seq_len(nn))    

#Cage5M 
matplot(t(cage5M[,c(3:8)]),type="l", ,xaxt = "n")
axis(1,at =c(1,2,3,4,5,6),labels=c("OPT", "HEAT", "COLD", "DIET", "ANTI", "FMT"))
nn <- 5
legend("bottomright", inset=.02, cage5M$Name, col=seq_len(nn),cex=0.5,fill=seq_len(nn))    

#Cage6M 
matplot(t(cage6M[,c(3:8)]),type="l", ,xaxt = "n")
axis(1,at =c(1,2,3,4,5,6),labels=c("OPT", "HEAT", "COLD", "DIET", "ANTI", "FMT"))
nn <- 5
legend("bottomright", inset=.02, cage6M$Name, col=seq_len(nn),cex=0.5,fill=seq_len(nn))    

#Cage7M 
matplot(t(cage7M[,c(3:8)]),type="l", ,xaxt = "n")
axis(1,at =c(1,2,3,4,5,6),labels=c("OPT", "HEAT", "COLD", "DIET", "ANTI", "FMT"))
nn <- 5
legend("bottomright", inset=.02, cage7M$Name, col=seq_len(nn),cex=0.5,fill=seq_len(nn))    

#Cage8M 
matplot(t(cage8M[,c(3:8)]),type="l", ,xaxt = "n")
axis(1,at =c(1,2,3,4,5,6),labels=c("OPT", "HEAT", "COLD", "DIET", "ANTI", "FMT"))
nn <- 5
legend("bottomright", inset=.02, cage8M$Name, col=seq_len(nn),cex=0.5,fill=seq_len(nn))    

#Cage9F 
matplot(t(cage9F[,c(3:8)]),type="l", ,xaxt = "n")
axis(1,at =c(1,2,3,4,5,6),labels=c("OPT", "HEAT", "COLD", "DIET", "ANTI", "FMT"))
nn <- 5
legend("bottomright", inset=.02, cage9F$Name, col=seq_len(nn),cex=0.5,fill=seq_len(nn))    

#Cage10F 
matplot(t(cage10F[,c(3:8)]),type="l", ,xaxt = "n")
axis(1,at =c(1,2,3,4,5,6),labels=c("OPT", "HEAT", "COLD", "DIET", "ANTI", "FMT"))
nn <- 5
legend("bottomright", inset=.02, cage10F$Name, col=seq_len(nn),cex=0.5,fill=seq_len(nn))    

#Cage11F 
matplot(t(cage11F[,c(3:8)]),type="l", ,xaxt = "n")
axis(1,at =c(1,2,3,4,5,6),labels=c("OPT", "HEAT", "COLD", "DIET", "ANTI", "FMT"))
nn <- 5
legend("bottomright", inset=.02, cage11F$Name, col=seq_len(nn),cex=0.5,fill=seq_len(nn))    

#Cage12F 
matplot(t(cage12F[,c(3:8)]),type="l", ,xaxt = "n")
axis(1,at =c(1,2,3,4,5,6),labels=c("OPT", "HEAT", "COLD", "DIET", "ANTI", "FMT"))
nn <- 5
legend("bottomright", inset=.02, cage12F$Name, col=seq_len(nn),cex=0.5,fill=seq_len(nn))    

#Cage13F 
matplot(t(cage13F[,c(3:8)]),type="l", ,xaxt = "n")
axis(1,at =c(1,2,3,4,5,6),labels=c("OPT", "HEAT", "COLD", "DIET", "ANTI", "FMT"))
nn <- 5
legend("bottomright", inset=.02, cage13F$Name, col=seq_len(nn),cex=0.5,fill=seq_len(nn))    

#Cage14F 
matplot(t(cage14F[,c(3:8)]),type="l", ,xaxt = "n")
axis(1,at =c(1,2,3,4,5,6),labels=c("OPT", "HEAT", "COLD", "DIET", "ANTI", "FMT"))
nn <- 5
legend("bottomright", inset=.02, cage14F$Name, col=seq_len(nn),cex=0.5,fill=seq_len(nn))    

#Cage15F 
matplot(t(cage15F[,c(3:8)]),type="l", ,xaxt = "n")
axis(1,at =c(1,2,3,4,5,6),labels=c("OPT", "HEAT", "COLD", "DIET", "ANTI", "FMT"))
nn <- 5
legend("bottomright", inset=.02, cage15F$Name, col=seq_len(nn),cex=0.5,fill=seq_len(nn))    

#Cage16F 
matplot(t(cage16F[,c(3:8)]),type="l", ,xaxt = "n")
axis(1,at =c(1,2,3,4,5,6),labels=c("OPT", "HEAT", "COLD", "DIET", "ANTI", "FMT"))
nn <- 5
legend("bottomright", inset=.02, cage16F$Name, col=seq_len(nn),cex=0.5,fill=seq_len(nn))    

