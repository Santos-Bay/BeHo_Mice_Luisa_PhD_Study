##Check if normally distributed when having both wins AND losses ##

setwd("~/Documents/PhD 2020/BeHo:ZiBA study/Data analysis/Tube test data analysis")
winslosses = read.csv("Mice-wins-losses-joined.csv", header = TRUE)

# just getting the controls
controls_OPT_HEAT <- winslosses[c(17:21,62:66),c(1,53,54)]

winslosses <- winslosses[,c(1,53:62)] #total win minus loss AND total losses
winslosses <- winslosses[,c(1,2,3,8:11)] #total win minus loss
winslosses_matrix <- data.matrix(winslosses) 

library("PerformanceAnalytics")
chart.Correlation(winslosses_matrix[,2:], histogram=TRUE, pch=19) #pearson correlation

hist(winslosses_matrix)

#chart.Correlation(winslosses, histogram=TRUE, pch=19)

# summing them together to see if with more data the distribution converges to a normal
OPT_HEATwinslosses$new <- rowSums(OPT_HEATwinslosses)
hist(OPT_HEATwinslosses[,3])

hist(rowSums(controls_OPT_HEAT))
