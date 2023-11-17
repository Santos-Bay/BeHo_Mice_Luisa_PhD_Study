## scatter plots (correlation) visualization ##
library(tidyverse)
library(reshape2)
library(tidyr)
library(effects) 
library(ggplot2) 
library(sjPlot)

setwd("~/Documents/PhD 2020/BeHo:ZiBA study/Data analysis/Tube test data analysis/R_input_files")
tube_test_wins = read.csv("Mice-full-overview.csv", header = TRUE)

###### TRANSFORM DATAFRAME FOR STATISTICS #######
# placing NA for dead mice (C8 mouse 3065, C16 mouse 3183, C7 mouse 2730, C3 mouse 3083, C1 mouse 4176)
tube_test_wins[76, c(32:45,48:49)] = NA
tube_test_wins[38, c(32:45,48:49)] = NA
tube_test_wins[32, c(32:45,48:49)] = NA
tube_test_wins[14, c(32:45,48:49)] = NA
tube_test_wins[4,c(28:45,47:49)] = NA

#Removing irrelevant columns and saving it into a new df
tube_test_80 <- tube_test_wins [,-c(5,8,10,15:17,21:23, 27, 31, 35:37, 39, 42:50)]

#Renaming columns without the many dots
names(tube_test_80)[2] <- "Arrive_weight"
names(tube_test_80)[5] <- "Sex"
names(tube_test_80)[6] <- "Exp_group"

#Transform the dataframe from a wide to a long format
tube_test_80 <- melt(tube_test_80, id.vars = c("Name","Arrive_weight", "Phenotype", "Crate", "Sex", "Exp_group","Cage.ID"))

#Renaming the columns created by melt()
names(tube_test_80)[8] <- "Treatments"
names(tube_test_80)[9] <- "Wins_per_rep"
names(tube_test_80)[7] <- "Cage_ID"
names(tube_test_80)[4] <- "Crate_ID"

#Add a new column with the replicate info and then rename the rows of column Treatments to only the treatments 
Replicates <- c(rep("R1",80), rep("R2",80), rep("R3",80),rep("R4",80),rep(c(rep("R1",80), rep("R2",80), rep("R3",80)),4),rep("R3",80), rep("R4",80), rep("R5",80))
#add the column "Replicates" to the dataframe tube_test_80
tube_test_80 <- cbind(tube_test_80,Replicates)
#Changing the name of the factors created after transforming the dataframe. There are 19 in total corresponding to the no of reps in total.
levels(tube_test_80$Treatments) <- c("OPT","OPT","OPT","OPT","HEAT","HEAT","HEAT","COLD","COLD","COLD","DIET","DIET","DIET","ANTI","ANTI","ANTI","FMT","FMT","FMT")
levels(tube_test_80$Exp_group) <- c("Treatment", "Control") #does the glm or lm model then know how to differ from the experimental and control animals tube test wins? Ask A & O this.

#Move the no of wins column (all of it) to the first column
colnames(tube_test_80)
tube_test_80 <- tube_test_80[, c(9,8,10,1:7)]

#Creating new factors to start statistics. A factor instead of num, chr or int account for variation between the "catagories"
tube_test_80 <- transform(tube_test_80,Replicates=as.factor(Replicates),
                          Phenotype=as.factor(Phenotype),
                          Crate=as.factor(Crate_ID),
                          Sex=as.factor(Sex),
                          Exp_group=as.factor(Exp_group),
                          Cage_ID=as.factor(Cage_ID))
str(tube_test_80) #check if the command worked
head(tube_test_80)

#Remove mice that are not among the 40 I'm working with:
#I have mice from cage 3-6 (males) and 10-13 (females)
cages_to_remove <- c("01M","02M","07M","08M","09F","14F","15F","16F")
condition <- tube_test_80$Cage_ID %in% cages_to_remove
tube_test_40 <- tube_test_80[!condition, ]
# Remove rows with missing values (NA) in any column if needing the df like this later
cleaned_tube_test_40 <- drop_na(tube_test_40)

str(tube_test_40)
head(tube_test_40)

















###################### MARCH 9th 2022 CORRELATIONS ####################
#The commands below belong to the R script called "plots march 9th - w correlations". 

setwd("~/Documents/PhD 2020/BeHo:ZiBA study/Data analysis/Tube test data analysis/R_input_files")
tube_test_wins = read.csv("Mice-full-overview.csv", header = TRUE)

# placing NA for dead mice (C8 mouse 3065, C16 mouse 3183, C7 mouse 2730, C3 mouse 3083, C1 mouse 4176)
#tidy up by using vector so just one line

tube_test_wins[76, 32:45] = NA
tube_test_wins[76,48:49] = NA
tube_test_wins[38, 32:45] = NA
tube_test_wins[38,48:49] = NA
tube_test_wins[32, 32:45] = NA
tube_test_wins[32,48:49] = NA
tube_test_wins[14, 32:45] = NA
tube_test_wins[14,48:49] = NA
tube_test_wins[4,28:45] = NA
tube_test_wins[4,47:49] = NA

opt_wins <- tube_test_wins[,c(1,8,9, 11:17)] #we added TagIDs (col 8) & CageIDs (col 9) 
opt_wins <- opt_wins[order(opt_wins$Name),]

#par(mar=c(1,1,1,1), xpd=TRUE)

#matrix_opt <- data.matrix(opt_wins)

#plot(x=1:4, y = matrix_opt[1,1:4])

# Violin Plots for all mice across replicates in OPT
library(vioplot)
x1 <- opt_wins$Wins.OPT.R1
x2 <- opt_wins$Wins.OPT.R2
x3 <- opt_wins$Wins.OPT.R3
x4 <- opt_wins$Wins.OPT.R4
vioplot(x1, x2, x3, x4, names=c("R1","R2","R3","R4"),  col=2:5)
title("Violin Plots of OPT replicates wins")


fmt_wins <- tube_test_wins[,c(1,5, 36:38,40:41)]
x1 <- fmt_wins$Wins.FMT.R1
x2 <- fmt_wins$Wins.FMT.R2
x3 <- fmt_wins$Wins.FMT.R3
x4 <- fmt_wins$Wins.FMT.R4
x5 <- fmt_wins$Wins.FMT.R5
vioplot(x1, x2, x3, x4, x5, names=c("R1","R2","R3","R4","R5"),  col=2:6)
title("Violin Plots of FMT replicates wins")


# Violin plots for all mice per treatment
avg_wins <- tube_test_wins[,c(1,5, 15, 22, 46:49)]
x1 <- avg_wins$Rank.of.Wins.OPT.Avg
x2 <- avg_wins$Rank.of.Wins.HEAT.Avg
x3 <- avg_wins$Rank.of.wins.COLD.Avg
x4 <- avg_wins$Rank.of.wins.DIET.Avg
x5 <- avg_wins$Rank.of.wins.ANTI.Avg
x6 <- avg_wins$Rank.of.wins.FMTs.3.5.Avg
vioplot(x1, x2, x3, x4, x5, x6, names=c("OPT", "HEAT", "COLD", "DIET", "ANTI", "FMT"),  col=2:7)
title("Violin Plots of average wins per treatment")

avg_wins$Cage <- factor(avg_wins$Cage, levels=unique(avg_wins$Cage)) #to keep the order of the cages

avg_wins_by_cage= split(avg_wins, avg_wins$Cage)

#par(mar=c(5,4,4,9), xpd=TRUE)
for (i in avg_wins_by_cage){
  matplot(t(i[,c(3:8)]),type="b", ,xaxt = "n", ylab="Mean wins", pch=20, ylim= c(0,4))
  axis(1,at =c(1,2,3,4,5,6),labels=c("OPT", "HEAT", "COLD", "DIET", "ANTI", "FMT"))
  nn <- 5
  legend(x= 6.5, y=3, cex=0.5, i$Name, col=seq_len(nn), fill=seq_len(nn), bty= "n")    
}


#par(mar=c(1,1,1,1), xpd=TRUE)
plot(avg_wins)


### Looking into correlations ###
#Used this website here among others: http://www.sthda.com/english/wiki/correlation-test-between-two-variables-in-r#compute-correlation-in-r

tube_test_wins_raw = read.csv("Mice-full-overview.csv", header = TRUE)
tube_test_wins_raw <- tube_test_wins_raw[order(tube_test_wins_raw$Name),]

plotting_all <- tube_test_wins_raw[,c(1,9,15,22,46:50)]
plot(plotting_all)
plotting_all_matrix <- data.matrix(plotting_all) #turn the dataframe into a matrix
correlations <- cor(plotting_all_matrix)
plot(correlations)

### Testing cor between OPT and Heat ###
library("ggpubr")
g <- ggscatter(plotting_all, x = "Rank.of.Wins.OPT.Avg", y = "Rank.of.Wins.HEAT.Avg", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "OPT wins for all mice", ylab = "HEAT wins for all mice", color = "Rank.of.Wins.OPT.Avg")
#save plot
pdf(paste("R_plots/","Correlations between mean OPT & HEAT wins.pdf"), width = 10, height = 7)
g
dev.off() 
#test if the two variables are following a normal distribution
shapiro.test(plotting_all$Rank.of.Wins.OPT.Avg) #p-value = 0.005641
shapiro.test(plotting_all$Rank.of.Wins.HEAT.Avg) #p-value = 0.0005415
#They are not normal distributed (>0.05), which is expected since we only have mean test results of 0-4 wins 
ggqqplot(plotting_all$Rank.of.Wins.OPT.Avg, ylab = "OPT")
ggqqplot(plotting_all$Rank.of.Wins.HEAT.Avg, ylab = "HEAT")
#We can now clearly see that the data is not normal distributed
hist(plotting_all_matrix, xlab="Name", ylab="Average.rank..wins..for.all.treatments", main="Histrogram of mean wins across all treatments") 
#Pearson correlation test (can be used on non-normal distributed data)
res <- cor.test(plotting_all$Rank.of.Wins.OPT.Avg,plotting_all$Rank.of.Wins.HEAT.Avg, 
                method = "pearson")
res #we can conclude that OPT and HEAT are significantly correlated 
#with a correlation coefficient of 0.8048101
#and with a p-value of < 2.368999e-19
res$p.value
res$estimate
#About Pearson's test: It does not assume normality although it does assume finite variances and finite covariance. When the variables are bivariate normal, Pearson's correlation provides a complete description of the association.

res2 <- cor.test(plotting_all$Rank.of.Wins.OPT.Avg,plotting_all$Rank.of.Wins.HEAT.Avg, method="kendall")
res2 #we can conclude that OPT and HEAT are significantly correlated
res2$p.value
#with a correlation coefficient of 0.6690054
#and with a p-value of 1.883407e-16
#Kendall's test can be used on non-normal distributed data too
#The Kendall rank correlation coefficient or Kendall’s tau statistic is used to estimate a rank-based measure of association. 

res3 <-cor.test(plotting_all$Rank.of.Wins.OPT.Avg,plotting_all$Rank.of.Wins.HEAT.Avg, method = "spearman")
res3
res3$p.value
#with a correlation coefficient of 0.798846
#and with a p-value of 6.782516e-19
#Spearman’s test can be used on non-normal distributed data too
#Spearman’s rho statistic is also used to estimate a rank-based measure of association. 

### Testing cor between OPT and FMT ###
g2 <- ggscatter(plotting_all, x = "Rank.of.Wins.OPT.Avg", y = "Rank.of.wins.FMTs.3.5.Avg", 
               add = "reg.line", conf.int = TRUE, 
               cor.coef = TRUE, cor.method = "pearson",
               xlab = "OPT wins for all mice", ylab = "HEAT wins for all mice", color = "Rank.of.wins.FMTs.3.5.Avg")
#save plot
pdf(paste("R_plots/","Correlations between mean OPT & HEAT wins.pdf"), width = 10, height = 7)
g2
dev.off() 

shapiro.test(plotting_all$Rank.of.wins.FMTs.3.5.Avg) #p-value = 0.0001104
res4 <- cor.test(plotting_all$Rank.of.Wins.OPT.Avg,plotting_all$Rank.of.wins.FMTs.3.5.Avg, 
                method = "pearson")
res4 #correlation (R) = 0.6361253
