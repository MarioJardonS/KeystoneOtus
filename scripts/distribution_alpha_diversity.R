#!/usr/bin/env Rscript

library(vegan)
library(ggplot2)

args = commandArgs(trailingOnly=TRUE)

#Datos de comunidad y normalizacion
community <- paste0( "../data/tables/" , args[1] , "_" , args[2] , ".csv" )
community <- read.csv(community , header = 1, row.names = 1)
for (i in 1:dim(community)[2]){
  
  community[ , i] <- as.integer(as.vector(community[ , i]))
  community[ , i] <- community[ , i]/sum(community[ , i ])
}

present <- c()

for(i in 1:dim(community)[1]){
  if(sum(community[i ,]) > 0){
    #if(length(community[i ,][ community[i , ] > 1 ]) > 1){
    present <- c(present , i)
  }
}

community <- community[present, ]

diversity_community <- c()

for (i in 1:dim(community)[2]){
  diversity_i <- diversity(as.vector(community[ , i]) )
  diversity_i <- diversity_i/log(dim(community)[1])
  diversity_community <- c(diversity_community , diversity_i)
}

positive_correlations <- c()
negative_correlations <- c()

for (i in 1:dim(community)[1]){
  t <- cor.test(as.numeric(as.vector(community[i ,])) , diversity_community , alternative = "greater" , method = "spearman")
  if (t$p.value < 0.05){
    positive_correlations <- c(positive_correlations , t$estimate)
  }else{
    t <- cor.test(as.numeric(as.vector(community[i ,])) , diversity_community , alternative = "less" , method = "spearman")
    if (t$p.value < 0.05){
      negative_correlations <- c(negative_correlations , t$estimate)
    }
  }
}

print(args[1])
print(length(positive_correlations))
print(summary(positive_correlations))
print(length(negative_correlations))
print(summary(negative_correlations))


