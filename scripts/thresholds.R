#!/usr/bin/env Rscript
## -----------------------------------------------------------------------------------------------------------------------


args = commandArgs(trailingOnly=TRUE)

data <- paste0("../results/central_otus/", args[1] , "_Rhizosphere_keystone_otus.csv")

data <- read.csv(data , row.names = 1, header = TRUE)

cat(args[1])

degree <- min(as.vector(data$degrees))
cat(paste0("& geq " , degree  ))

closeness <- min(as.vector(data$closeness))
cat(paste0("& geq " , closeness))

betweenness <- max(as.vector(data$betweennes))
cat(paste0("& leq" , betweenness , "\n"))
