###### Tracking data of BeHo
###### Began this analysis in summer 2021
###### Antton Alberdi, Ostaizka Aizpurua, Tom Gilbert, Adam Koziol & I, Luisa Nielsen, are involved in the project
###### The script is created by Luisa


####
# Load libraries
####
library(plyr) #to count categories/observations


####
# Set directory
####
setwd("~/Documents/PhD 2020/ZiBA:BeHo 2020/Data analysis/Tube test data analysis")


####
# Read in file
####
tt_results <- read.csv("Tube test results.csv", header=TRUE, sep=",")
#data <- na.omit(data) #to remove NA's from the data set
tt_results <- tt_results[,c("Name","Test.code","Test.number","Winner", "Looser","Winner.position", "Duration..seconds.")] #I select only the columns mentioned here. The rest are discarded.
head(tt_results) #control that the file looks as suppose to
str(tt_results) #check out what type of object it is

####
# Plot data to visualise the distribution
####

#Visualising the distribution of duration/time of the tube tests

#to see the statistics of the time (min, max, mean etc.)
sum_tt_duration <- summary(tt_results$Duration..seconds.) 
sum_tt_duration
cor(tt_results[,c(4,7)]) #both variables should be numeric for the correlation function to work I think?

#save the stat output
capture.output(sum_tt_duration,file = paste("R_stats/","Stats of time across all tube tests.txt"))

#to visualise the distribution of duration (time)
tt_time <- tt_results$Duration..seconds.
hist_tt_time <- hist(tt_time, 
                     xlab="Seconds", 
                     ylab="Frequency", 
                     main="Histrogram of how long it took a mouse to win across all tube tests", 
                     xlim=c(0,50), 
                     ylim=c(0,2500))

#save the output
pdf(paste("R_plots/","Histogram of time to win tube test.pdf"))
hist_tt_time <- hist(tt_time, 
                     xlab="Seconds", 
                     ylab="Frequency", 
                     main="Histrogram of how long it took a mouse to win across all tube tests", 
                     xlim=c(0,50), 
                     ylim=c(0,2500)) #write here which plot you want saved as output.Can only be done this way as of right now. Have to change to ggplot
dev.off() 

#Visualising the distribution of the rest? of the tube tests
winner_count_tt_results <- count(tt_results$Winner)
str(winner_count_tt_results) #dataframe with int and chr
winner_count_tt_results <- winner_count_tt_results[2:81,] #keeping the rows 2-81, and thus removing row 1 = NA (number of tube tests NOT performed/skipped)
winnner_count_tt_results_22 <- as.numeric[,c(2)]
barplot(winner_count_tt_results$freq, names.arg = winner_count_tt_results$x, las=2, cex.names=0.5)
#Other commands saved that can be run (don't remember their purpose)
df <- melt(winner_count_tt_results,  id.vars = 'freq', variable.name = 'series')
bar <- ggplot(df, aes(freq,value))
bar + geom_bar()








####
# Read in file to divide the results according to treatment
####
tt_winner_data <- read.csv("Tube test results of winners.csv", header=TRUE, sep=",")
head()
