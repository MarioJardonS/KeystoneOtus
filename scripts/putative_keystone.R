#!/usr/bin/env Rscript
## -----------------------------------------------------------------------------------------------------------------------


args = commandArgs(trailingOnly=TRUE)

data <- paste0("../results/otus_by_centrality/" , args[1] , "_" , args[2] , "_centrality_measures.csv")

data <- read.csv(data , row.names = 1)

hdeg <- which(data$degrees >= quantile(data$degrees , probs = seq(0, 1, 0.33))[3])
hclose <- which(data$closeness >= quantile(data$closeness , probs = seq(0, 1, 0.33))[3])
lbetween <- which(data$betweenness <= quantile(data$betweenness , probs = seq(0, 1, 0.33))[2])

results_1 <- intersect(hdeg,hclose)
results_1 <- intersect(results_1 , lbetween)

data_report_1 <- data[results_1,]

write.csv(data_report_1 , paste0("../results/central_otus/", args[1] , "_" , args[2] , "_keystone_otus.csv" ) , row.names = TRUE)
