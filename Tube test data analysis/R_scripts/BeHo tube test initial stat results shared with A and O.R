###### LOADING LIBRARIES #####
library(tidyverse)
library(reshape2)
library(tidyr)
library(effects)
library(ggplot2)

###### SET WORK SPACE AND READ IN FILE ######
setwd("~/Documents/PhD 2020/BeHo:ZiBA study/Data analysis/Tube test data analysis")
tube_test_wins = read.csv("R_input_files/Mice-full-overview.csv", header = TRUE)


###### TRANSFORM DATAFRAME FOR STATISTICS #######
#Placing NAs for dead mice (C8 mouse 3065, C16 mouse 3183, C7 mouse 2730, C3 mouse 3083, C1 mouse 4176)
tube_test_wins[76, c(32:45,48:49)] = NA
tube_test_wins[38, c(32:45,48:49)] = NA
tube_test_wins[32, c(32:45,48:49)] = NA
tube_test_wins[14, c(32:45,48:49)] = NA
tube_test_wins[4,c(28:45,47:49)] = NA

#Removing irrelevant columns
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
str(tube_test_80)

#Remove mice that are not among the 40 I'm working with:
#I have mice from cage 3-6 (males) and 10-13 (females)
cages_to_remove <- c("01M","02M","07M","08M","09F","14F","15F","16F")
condition <- tube_test_80$Cage_ID %in% cages_to_remove
tube_test_40 <- tube_test_80[!condition, ]
# Remove rows with missing values (NA) in any column if needing the df like this later
cleaned_tube_test_40 <- drop_na(tube_test_40)

str(tube_test_40)


######## QUICK VISUALIZATION OF MY DATA (THE DEPENDENT VARIABLE) ##### 
hist(tube_test_80$Wins_per_rep, breaks=seq(0,4,by=0.2), ylim = c(0,400), xlab="Dominance rank", main="Histogram showing frequency of wins for all tube tests for all 80 mice")
hist(tube_test_40$Wins_per_rep, breaks=seq(0,4,by=0.2), ylim = c(0,200),xlab="Dominance rank", main="Histogram showing frequency of wins for all tube tests for the 40 specific mice")


######## STATISTICS - using glm (Generalized Linear Models) and lm (Multiple Linear Regression Models) ####
glimpse(tube_test_40) 
glimpse(tube_test_80)

### lm (Multiple Linear Regression Models)
#ALL 80 MICE
#Because the Mice IDs are not of relevance here, we exclude the Name column, but test all other variables
lm_result_all <- lm(Wins_per_rep ~ .-Name,data=tube_test_80)
summary(lm_result_all)
plot(lm_result_all)

#SPCEIFIC 40 MICE ONLY
lm_result_all <- lm(Wins_per_rep ~ .-Name,data=tube_test_40)
summary(lm_result_all)
plot(lm_result_all)

### glm (Generalized Linear Models) using the poisson model
#ALL 80 MICE
#Because the Mice IDs are not of relevance here, we exclude the Name column, but test all other variables
glm_result <- glm(Wins_per_rep ~ .-Name,data=tube_test_80,family = poisson())
summary(glm_result)
plot(glm_result)

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
