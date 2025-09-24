#!/usr/bin/env Rscript

library(vegan)
library(ggplot2)
library(statip)
args = commandArgs(trailingOnly=TRUE)

#Datos de comunidad y normalizacion
community <- paste0( "../data/tables/" , args[1] )
community <- read.table(community , header = 1, row.names = 1)
for (i in 1:dim(community)[2]){
  
  community[ , i] <- as.integer(as.vector(community[ , i]))
  community[ , i] <- community[ , i]/sum(community[ , i ])
}


size <- args[2]

diversity_community <- c()

for (i in 1:dim(community)[2]){
  diversity_community <- c(diversity_community , diversity(as.vector(community[ , i]) ))
}



abundance_subsamples <- list()
cv_subsamples <- c()


for ( i in 1:1000){
 random_i <- sample(row.names(community) , size = size )
 community_i <- community[random_i , ]
 
 cv_i <- c()
 for (j in 1:size){ 
   cv_i <- c(cv_i  , cv(as.numeric(community_i[j , ])))
   }
 abundance_subsamples[[i]] <- colSums(community_i)
 cv_subsamples <- c(cv_subsamples ,sum(cv_i))
 
 
 
}

cor_to_diversity <- c()
relevant <- c()
for ( i in 1:1000){
  cor_to_diversity_i <- cor.test(abundance_subsamples[[i]] , diversity_community , alternative = "greater" , method = "spearman")
  cor_to_diversity <- c(cor_to_diversity , cor_to_diversity_i[["estimate"]])
  if (cor_to_diversity_i[["p.value"]] < 0.05){
    relevant <- c(relevant , i)
    
  }
}

print(length(relevant))

print(cor.test( cor_to_diversity[relevant] , cv_subsamples[relevant] , alternative = "less" , method = "spearman"  ))
