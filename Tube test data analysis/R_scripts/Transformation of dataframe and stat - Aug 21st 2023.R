###### LOADING LIBRARIES #####
library(tidyverse)
library(reshape2)
library(tidyr)
library(effects) #recommended by Aoife to plot regression lines and models created
library(ggplot2) #recommended by Aoife to plot regression lines and models created
library(sjPlot) #recommended by Aoife to plot regression lines and models created
library(vioplot)


###### SET WORK SPACE AND READ IN FILE ######
setwd("~/Documents/PhD 2020/BeHo:ZiBA study/Data analysis/Tube test data analysis")
tube_test_wins = read.csv("R_input_files/Mice-full-overview.csv", header = TRUE)


###### TRANSFORM DATAFRAME FOR STATISTICS #######
# placing NA for dead mice (C8 mouse 3065, C16 mouse 3183, C7 mouse 2730, C3 mouse 3083, C1 mouse 4176)
tube_test_wins[76, c(32:45,48:49)] = NA
tube_test_wins[38, c(32:45,48:49)] = NA
tube_test_wins[32, c(32:45,48:49)] = NA
tube_test_wins[14, c(32:45,48:49)] = NA
tube_test_wins[4,c(28:45,47:49)] = NA

#Removing irrelevant columns
tube_test_80 <- tube_test_wins [,-c(5,8,10,15:17,21:23, 27, 31, 35:37, 39, 42:50)]
#tube_test_80 <- tube_test_wins [,-c(5,8,10,39,42:44)] #I would like it changed into this over time, but for now I can't. It will ruin the "Replicates" and "levels" commands.

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

######## QUICK VISUALIZATION OF MY DATA (THE DEPENDENT VARIABLE) #####
#Histogram
hist(tube_test_80$Wins_per_rep) #why gaps?
hist(tube_test_40$Wins_per_rep) #why gaps?
#I can't figure out why the plots looks weird with spaces/gaps in between the bars?

#So far this is the solution:
hist(tube_test_80$Wins_per_rep, breaks=seq(0,4,by=0.2), ylim = c(0,400), xlab="Dominance wins", main="Histogram showing frequency of wins for all tube tests for all 80 mice")
hist(tube_test_40$Wins_per_rep, breaks=seq(0,4,by=0.2), ylim = c(0,200),xlab="Dominance wins", main="Histogram showing frequency of wins for all tube tests for the 40 specific mice")

# figure out how to make this work. Not working atm.
#pdf("R_plots/Histogram of freq of wins for all 80 mice.pdf", width = 7, height = 10)
#hist_80 <- hist(tube_test_80$Wins_per_rep, breaks=seq(0,4,by=0.2), ylim = c(0,400), xlab="Dominance wins", main="Histogram showing frequency of wins for all tube tests for all 80 mice")
#hist_80
#dev.off()
 


######## STATISTICS - using glm (Generalized Linear Models) and lm (Multiple Linear Regression Models) ####
glimpse(tube_test_40) #to have a quick look at the data with the 40 specific mice
glimpse(tube_test_80) #to have a quick look at the data with all 80 mice

### lm (Multiple Linear Regression Models)
#ALL 80 MICE
#Because the Mice IDs are not of relevance here, we exclude the Name column, but test all other variables/coefficients/predictors
lm_result_all <- lm(Wins_per_rep ~ .-Name,data=tube_test_80)
summary(lm_result_all)
plot(lm_result_all)
#Here we are for example only running the lm test with the treatments and sex as independent variables
lm_result1 <- lm(Wins_per_rep ~ Treatments + Sex,data=tube_test_80)
summary(lm_result1)
plot(lm_result1)

#SPCEIFIC 40 MICE ONLY
lm_result_all <- lm(Wins_per_rep ~ .-Name,data=tube_test_40)
summary(lm_result_all)
plot(lm_result_all)
#The lm model results of 80 vs. 40 mice differ

### glm (Generalized Linear Models) using the poisson model
#ALL 80 MICE
#Because the Mice IDs are not of relevance here, we exclude the Name column, but test all other variables/coefficients/predictors
glm_result <- glm(Wins_per_rep ~ .-Name,data=tube_test_80,family = poisson())
summary(glm_result)
plot(glm_result)
#Here we are for example only running the glm test with the treatments and sex as independent variables
glm_result2 <- glm(Wins_per_rep ~ Treatments + Sex,data=tube_test_80,family = poisson())
summary(glm_result2)
plot(glm_result2)
#glm_result3 <- glm(Wins_per_rep ~ Treatments+Sex+Cage_ID+Replicates,data=tube_test_80,family = poisson())
#summary(glm_result3)
#plot(glm_result3)
#glm_result4 <- glm(Wins_per_rep ~ Treatments+Sex+Cage_ID+Replicates+Exp_group,data=tube_test_80,family = poisson())
#summary(glm_result4)
#plot(glm_result4)

#SPECIFIC 40 MICE ONLY
glm_result <- glm(Wins_per_rep ~ .-Name,data=tube_test_40,family = poisson())
summary(glm_result)
plot(glm_result)
#The glm model results of 80 vs. 40 mice differ


#However, other families may be a better fit for my data, and thus, I read about them using ?glm()
#I also used ChatGPT to help me understand the definition of each family 
#Turns out the Quasipoisson could fit my data better than poisson
#So to find out if this is the case, I have to explore my data
#If the  variance is larger than the mean in my dataset (=overdispersion), then quasipoisson is a better fit
#You only calculate the variance (sd) and mean of the dependent variable, which in my case is number of wins

#First testing this with all 80 mice
dependent_variable_wins <- tube_test_80$Wins_per_rep
mean_value <- mean(dependent_variable_wins,na.rm=TRUE)
sd_value <- sd(dependent_variable_wins,na.rm=TRUE)

coefficient_dispersion <- (sd_value / mean_value) * 100
coefficient_variation <- (sd_value / mean_value) * 100

print(coefficient_dispersion) #69.62755
print(coefficient_variation) #69.62755
#The results of the 40 specific mice (tube_test_40) were: 68.8126 and 68.8126
#In this case, the standard deviation (sd) = variance is equal to the mean, resulting in the coefficients being the same.
#So since my data is count data with no overdispersion, the Poisson family might be suitable indeed.
#When the standard deviation is equal to the mean, it implies that the data values are evenly distributed around the mean (which is clear when visualising the number of wins in a histogram). 
#This can occur when the data is symmetrically distributed or when there is a specific relationship between the values in your dataset. The latter is my data (0,1,2,3 or 4 wins)

#I looked at papers doing tube test analyses and one named "social hierarchy position in female mice is associated with plasma corticosterone levels and hypothalamic gene expression" (2019) by Williamson et al. uses a Poisson-lognormal distribution test.
#I want to see if the Poisson-lognormal distribution test fits my data better than regular poisson

#TEST ON SPECIFIC 40 MICE
# Fit Poisson regression model
#poisson_model <- glm(Wins_per_rep ~ . - Name, data = cleaned_tube_test_40, family = "poisson")
poisson_model <- glm(Wins_per_rep ~ Treatments + Cage_ID, data = cleaned_tube_test_40, family = "poisson")

# Fit Poisson-lognormal regression model with log-transformed response
#log_count_var <- log(cleaned_tube_test_40$Wins_per_rep)
#cleaned_tube_test_40$log_wins <- log_count_var
#head(cleaned_tube_test_40)
#poisson_lognormal_model <- glm(log_wins ~ . - Name, data = cleaned_tube_test_40, family = "gaussian") #does not work
#poisson_lognormal_model <- glm(log_wins ~ Treatments + Cage_ID, data = cleaned_tube_test_40, family = "gaussian") #does not work
#cleaned_tube_test_40 <- within(cleaned_tube_test_40, rm(log_wins))
#poisson_lognormal_model <- glm(log(Wins_per_rep) ~ . - Name, data = cleaned_tube_test_40, family = "gaussian") #does not work
#poisson_lognormal_model <- glm(log(Wins_per_rep) ~ Treatments + Cage_ID, data = cleaned_tube_test_40, family = "gaussian") #does not work




###### ASKING SIMPLE QS AND ANSWERING THEM ######
#1. What is the hierarchy per cage for each treatments? Is the hierarchy stable?
#This question is answered March 8th 2022 in the R file "plots march 8th prettier (1)"
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



#2. Is there an overall hierarchy (despite of treatments)? For example one mouse continuously winning
total_avg_wins <- tube_test_wins[,c(1,5,6,50)]
total_avg_wins_40 <- total_avg_wins[-c(1:10,31:45,66:80),] #removing irrelevant rows/cages
            ### Note to self: change column names of total_avg_wins_40 e.g. Average.rank..wins..for.all.treatments
#total_avg_wins$Cage <- factor(total_avg_wins$Cage, levels=unique(total_avg_wins$Cage)) #to keep the order of the cages
#total_avg_wins_by_cage= split(total_avg_wins, total_avg_wins$Cage)

#Does this take individuality/mouse ID into account? YES! It does. Look at notes on my Mac.
avg_wins <- tube_test_wins[,c(1,5, 15, 22, 46:49)]
avg_wins_40 <- avg_wins[-c(1:10,31:45,66:80),] #removing irrelevant cages
OPT <- avg_wins_40$Rank.of.Wins.OPT.Avg
HEAT <- avg_wins_40$Rank.of.Wins.HEAT.Avg
COLD <- avg_wins_40$Rank.of.wins.COLD.Avg
DIET <- avg_wins_40$Rank.of.wins.DIET.Avg
ANTI <- avg_wins_40$Rank.of.wins.ANTI.Avg
FMT <- avg_wins_40$Rank.of.wins.FMTs.3.5.Avg
vioplot(OPT, HEAT, COLD, DIET, ANTI, FMT, names=c("OPT", "HEAT", "COLD", "DIET", "ANTI", "FMT"),  col=2:7)
title("Violin Plots showing the distribution of 40 mice's mean wins per treatment")
#if the domiance rank/hierarchy is stable, then the median of the means would be the same, and the violins would look the same too)

# Create the basic ggplot object
#pdf("R_plots/mean_total_win_per_mouse.pdf", width = 10, height = 7)
plot <- ggplot(total_avg_wins, aes(x = Name, y = Average.rank..wins..for.all.treatments, fill = Cage)) +
  geom_bar(stat = "identity") +
  labs(x = "Name", y = "Average Wins", title = "Average Wins for All Treatments") +
  scale_fill_discrete(guide = "none") +  # Remove legend
  theme_minimal() +  # Set a transparent background
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
#show plot
plot
#dev.off() 

# Reorder the barplot, so that the order is based on number of wins per mouse
#pdf("R_plots/sorted_mean_total_win_per_mouse.pdf", width = 10, height = 7)
reordered_plot <- ggplot(total_avg_wins, aes(x = reorder(Name, Average.rank..wins..for.all.treatments), y = Average.rank..wins..for.all.treatments, fill = Cage)) +
  geom_bar(stat = "identity") +
  labs(x = "Name", y = "Average Wins", title = "Average Wins for All Treatments - ordered") +
  scale_fill_discrete(guide = "none") +  # Remove legend
  theme_minimal() +  # Set a transparent background
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
#show plot
reordered_plot
#dev.off()


### Plots with just the 40 individuals
#pdf("R_plots/mean_total_win_per_mouse_40.pdf", width = 10, height = 7)
plot_40 <- ggplot(total_avg_wins_40, aes(x = Name, y = Average.rank..wins..for.all.treatments, fill = Cage)) +
  geom_bar(stat = "identity") +
  labs(x = "Name", y = "Average Wins", title = "Average Wins for All Treatments Cage 3-6M and 10-13F") +
  scale_fill_discrete(guide = "none") +  # Remove legend
  theme_minimal() +  # Set a transparent background
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
plot_40
#dev.off() 

# Reorder the barplot, so that the order is based on number of wins per mouse
#pdf("R_plots/sorted_mean_total_win_per_mouse_40.pdf", width = 10, height = 7)
reordered_plot_40 <- ggplot(total_avg_wins_40, aes(x = reorder(Name, Average.rank..wins..for.all.treatments), y = Average.rank..wins..for.all.treatments, fill = Cage)) +
  geom_bar(stat = "identity") +
  labs(x = "Name", y = "Average Wins", title = "Sorted Average Wins for All Treatments Cage 3-6M and 10-13F") +
  scale_fill_discrete(guide = "none") +  # Remove legend
  theme_minimal() +  # Set a transparent background
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
reordered_plot_40
#dev.off() 

#3. Is dominance different between sex?
avg_wins <- tube_test_wins[,c(1,5, 15, 22, 46:49)]
f_avg_wins <- avg_wins[-c(1:40),] #removing irrelevant cages
OPT <- f_avg_wins$Rank.of.Wins.OPT.Avg
HEAT <- f_avg_wins$Rank.of.Wins.HEAT.Avg
COLD <- f_avg_wins$Rank.of.wins.COLD.Avg
DIET <- f_avg_wins$Rank.of.wins.DIET.Avg
ANTI <- f_avg_wins$Rank.of.wins.ANTI.Avg
FMT <- f_avg_wins$Rank.of.wins.FMTs.3.5.Avg
vioplot(OPT, HEAT, COLD, DIET, ANTI, FMT, names=c("OPT", "HEAT", "COLD", "DIET", "ANTI", "FMT"),  col=2:7)
title("Violin Plots showing the distribution of females mean wins per treatment")

m_avg_wins <- avg_wins[-c(41:80),] #removing irrelevant cages
OPT <- m_avg_wins$Rank.of.Wins.OPT.Avg
HEAT <- m_avg_wins$Rank.of.Wins.HEAT.Avg
COLD <- m_avg_wins$Rank.of.wins.COLD.Avg
DIET <- m_avg_wins$Rank.of.wins.DIET.Avg
ANTI <- m_avg_wins$Rank.of.wins.ANTI.Avg
FMT <- m_avg_wins$Rank.of.wins.FMTs.3.5.Avg
vioplot(OPT, HEAT, COLD, DIET, ANTI, FMT, names=c("OPT", "HEAT", "COLD", "DIET", "ANTI", "FMT"),  col=2:7)
title("Violin Plots showing the distribution of males mean wins per treatment")

m_avg_wins_20 <- avg_wins[c(11:30),] #keeping relevant male cages
f_avg_wins_20 <- avg_wins[c(46:65),] #keeping relevant female cages
OPT_m <- m_avg_wins$Rank.of.Wins.OPT.Avg
OPT_f <- f_avg_wins$Rank.of.Wins.OPT.Avg
HEAT_m <- m_avg_wins$Rank.of.Wins.HEAT.Avg
HEAT_f <- f_avg_wins$Rank.of.Wins.HEAT.Avg
COLD_m <- m_avg_wins$Rank.of.wins.COLD.Avg
COLD_f <- f_avg_wins$Rank.of.wins.COLD.Avg
DIET_m <- m_avg_wins$Rank.of.wins.DIET.Avg
DIET_f <- f_avg_wins$Rank.of.wins.DIET.Avg
ANTI_m <- m_avg_wins$Rank.of.wins.ANTI.Avg
ANTI_f <- f_avg_wins$Rank.of.wins.ANTI.Avg
FMT_m <- m_avg_wins$Rank.of.wins.FMTs.3.5.Avg
FMT_f <- f_avg_wins$Rank.of.wins.FMTs.3.5.Avg
vioplot(OPT_m,OPT_f,HEAT_m,HEAT_f,COLD_m,COLD_f,DIET_m,DIET_f,ANTI_m,ANTI_f,FMT_m,FMT_f,names=c("OPT male", "OPT female","HEAT male","HEAT female","COLD male","COLD female","DIET male","DIET female","ANTI male","ANTI female","FMT male","FMT female"),  col=2:13)
vioplot(OPT_m,OPT_f,HEAT_m,HEAT_f,COLD_m,COLD_f,DIET_m,DIET_f,ANTI_m,ANTI_f,FMT_m,FMT_f,names=c("OPT male", "OPT female","HEAT male","HEAT female","COLD male","COLD female","DIET male","DIET female","ANTI male","ANTI female","FMT male","FMT female"))
title("Violin Plots showing the distribution of males' and females' mean wins per treatment")

#Create the basic ggplot object
plot_40_sex <- ggplot(total_avg_wins_40, aes(x = Name, y = Average.rank..wins..for.all.treatments, fill = Cage)) +
  geom_bar(stat = "identity") +
  labs(x = "Name", y = "Average Wins", title = "Average Wins for All Treatments Cage 3-6M and 10-13F") +
  scale_fill_discrete(guide = "none") +  # Remove legend
  theme_minimal() +  # Set a transparent background
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  facet_wrap(~ Sex..from.Cage., ncol = 1)  # Create separate plots for each sex
#show it
plot_40_sex

#4. How's the data distribution (frequency of number of wins across all treatments)? How's the reliability of the tube test rank? 
#Histograms! Did this further up in the script

#5.How's the distribution/pattern of number of wins among replicates? How's the reliability of the tube test replicates?
opt_wins <- tube_test_wins[,c(1,8,9, 11:17)] #we added TagIDs (col 8) & CageIDs (col 9) 
opt_wins_40 <- opt_wins[-c(1:10,31:45,66:80),] #removing irrelevant mice
x1 <- opt_wins_40$Wins.OPT.R1
x2 <- opt_wins_40$Wins.OPT.R2
x3 <- opt_wins_40$Wins.OPT.R3
x4 <- opt_wins_40$Wins.OPT.R4
vioplot(x1, x2, x3, x4, names=c("R1","R2","R3","R4"),  col=2:5)
title("Violin Plots of OPT replicates wins")


fmt_wins <- tube_test_wins[,c(1,5, 36:38,40:41)]
fmt_wins_40 <- fmt_wins[-c(1:10,31:45,66:80),] #removing irrelevant mice
x1 <- fmt_wins_40$Wins.FMT.R1
x2 <- fmt_wins_40$Wins.FMT.R2
x3 <- fmt_wins_40$Wins.FMT.R3
x4 <- fmt_wins_40$Wins.FMT.R4
x5 <- fmt_wins_40$Wins.FMT.R5
vioplot(x1, x2, x3, x4, x5, names=c("R1","R2","R3","R4","R5"),  col=2:6)
title("Violin Plots of FMT replicates wins")









###### CODES USED TO FIND ERRORS (can be deleted again) #######
# Check for missing values in the entire data frame
any_missing <- any(is.na(tube_test_40))
print(any_missing) #output is TRUE (=missing values) or FALSE (=no missing values)
# Check for missing values in a specific column
missing_in_column <- sum(is.na(tube_test_40$Wins_per_rep))
print(missing_in_column) # six places in this column are missing values (NA)
# Find the indices of missing values in a specific column
missing_indices <- which(is.na(tube_test_40$Wins_per_rep))
print(missing_indices) # column location: 524 564 604 644 684 724. It is mouse 3083 from cage 3 that died = NA under wins.
#how to find a specific value within a column:
specific_value <- 0.5 #for exmaple 0.5 is the value I want to look for here.
filtered_rows <- subset(tube_test_80, Wins_per_rep == specific_value)
filtered_rows
# Check unique values in the column
unique_values <- unique(tube_test_80$Wins_per_rep)
unique_values #output: 2  3  0  4  1 NA






#### CORRELATIONS BETWEEN REPLICATES PER TREATMENT ####
library("PerformanceAnalytics")
chart.Correlation(tube_test_40[,c(1:3)], histogram=TRUE, pch=19) #command not working yet



