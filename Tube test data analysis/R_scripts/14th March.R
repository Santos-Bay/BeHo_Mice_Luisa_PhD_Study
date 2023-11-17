## Heatmap ####

setwd("~/Documents/PhD 2020/ZiBA:BeHo 2020/Data analysis/Tube test data analysis")
tube_test_wins = read.csv("Mice-full-overview.csv", header = TRUE)

# placing NA for dead mice (C8 mouse 3065, C16 mouse 3183, C7 mouse 2730, C3 mouse 3083, C1 mouse 4176)

tube_test_wins[76, c(32:45,48:49)] = NA
tube_test_wins[38, c(32:45,48:49)] = NA
tube_test_wins[32, c(32:45,48:49)] = NA
tube_test_wins[14, c(32:45,48:49)] = NA
tube_test_wins[4,c(28:45,47:49)] = NA

tube_test_wins_raw = read.csv("Mice-full-overview.csv", header = TRUE)
tube_test_wins_raw <- tube_test_wins_raw[order(tube_test_wins_raw$Name),]

plotting_all <- tube_test_wins_raw[,c(1,9,15,22,46:49)]
plot(plotting_all)
plotting_all_matrix <- data.matrix(plotting_all) #turn the dataframe into a matrix
correlations <- cor(plotting_all_matrix[,c(3:8)])

library(corrplot)
heatmap(plotting_all_matrix[,c(3:8)], Rowv=NA, Colv = NA)
heatmap(correlations,Rowv=NA, Colv = NA)
par(mar=c(1,1,1,1), xpd=TRUE)
corrplot(correlations, method="color", tl.cex = 0.45, addCoef.col = "gray")
