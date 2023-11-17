### Looking into correlations with Total WINS minus total LOSSES per treatment ###
#Used this website here among others: http://www.sthda.com/english/wiki/correlation-test-between-two-variables-in-r#compute-correlation-in-r
#We do this to investigate if we have a normal distribution using just two catagories: wins & losses

setwd("~/Documents/PhD 2020/BeHo:ZiBA study/Data analysis/Tube test data analysis")
tube_test_wins_raw = read.csv("R_input_files/Mice-full-overview.csv", header = TRUE)
#tube_test_wins_raw <- tube_test_wins_raw[order(tube_test_wins_raw$Name),]

tube_test_wins_raw[76, c(32:45,48:49)] = NA
tube_test_wins_raw[38, c(32:45,48:49)] = NA
tube_test_wins_raw[32, c(32:45,48:49)] = NA
tube_test_wins_raw[14, c(32:45,48:49)] = NA
tube_test_wins_raw[4,c(28:45,47:49)] = NA

plotting_rep_treat <- tube_test_wins_raw[,c(1,9,11:14,18:20,24:26,28:30,32:34,38,40,41)]
plotting_rep_treat_matrix <- data.matrix(plotting_rep_treat)
correlations_rep <- cor(plotting_rep_treat_matrix)
correlations_rep

plotting_all <- tube_test_wins_raw[,c(1,9,15,22,46:50)]
plot(plotting_all)
plotting_all_matrix <- data.matrix(plotting_all) #turn the dataframe into a matrix
correlations <- cor(plotting_all_matrix)
plot(correlations) #this plot makes no sense. Has to be modified to other x-axis and y-axis

library(Hmisc)
rescor<-rcorr(plotting_all_matrix, type = c("pearson","spearman"))
rescor$r #show the correlation outputs, I think
rescor$P #show the P-value outputs, I think
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
pdf(paste("R_plots/","Correlations between treatments by pearson with NAs.pdf"), width = 10, height = 7)
chart.Correlation(plotting_all_matrix[,c(3:8)], histogram=TRUE, pch=19) #method = pearson by default
dev.off()
pdf(paste("R_plots/","Correlations between treatments by spearman with NAs.pdf"), width = 10, height = 7)
chart.Correlation(plotting_all_matrix[,c(3:8)], histogram=TRUE, pch=19, method="spearman")
dev.off()

#All replicates compared
pdf(paste("R_plots/","Correlations between all replicates by pearson with NAs.pdf"), width = 20, height = 14)
chart.Correlation(plotting_rep_treat_matrix[,c(3:21)], histogram=TRUE, pch=19) #method = pearson by default
dev.off()
pdf(paste("R_plots/","Correlations between all replicates by spearman with NAs.pdf"), width = 20, height = 14)
chart.Correlation(plotting_rep_treat_matrix[,c(3:21)], histogram=TRUE, pch=19, method="spearman")
dev.off()

#All replicates for OPT
pdf(paste("R_plots/","Correlations between OPT replicates by pearson.pdf"), width = 10, height = 7)
chart.Correlation(plotting_rep_treat_matrix[,c(3:6)], histogram=TRUE, pch=19) #method = pearson by default
dev.off()
chart.Correlation(plotting_rep_treat_matrix[,c(3:6)], histogram=TRUE, pch=19, method="spearman")
#All replicates for HEAT
chart.Correlation(plotting_rep_treat_matrix[,c(7:9)], histogram=TRUE, pch=19) #method = pearson by default
chart.Correlation(plotting_rep_treat_matrix[,c(7:9)], histogram=TRUE, pch=19, method="spearman")
#All replicates for COLD
chart.Correlation(plotting_rep_treat_matrix[,c(10:12)], histogram=TRUE, pch=19) #method = pearson by default
chart.Correlation(plotting_rep_treat_matrix[,c(10:12)], histogram=TRUE, pch=19, method="spearman")
#All replicates for DIET
chart.Correlation(plotting_rep_treat_matrix[,c(13:15)], histogram=TRUE, pch=19) #method = pearson by default
chart.Correlation(plotting_rep_treat_matrix[,c(13:15)], histogram=TRUE, pch=19, method="spearman")
#All replicates for ANTI
chart.Correlation(plotting_rep_treat_matrix[,c(16:18)], histogram=TRUE, pch=19) #method = pearson by default
chart.Correlation(plotting_rep_treat_matrix[,c(16:18)], histogram=TRUE, pch=19, method="spearman")
#All replicates for FMT3-5
chart.Correlation(plotting_rep_treat_matrix[,c(19:21)], histogram=TRUE, pch=19) #method = pearson by default
chart.Correlation(plotting_rep_treat_matrix[,c(19:21)], histogram=TRUE, pch=19, method="spearman")

