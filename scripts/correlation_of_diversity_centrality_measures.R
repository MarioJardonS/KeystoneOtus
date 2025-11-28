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

#Trabajo con subconjunto "clave"

key_otus <- paste0( "../results/otus_by_centrality/" , args[1] ,"_" , args[2] , "_centrality_measures.csv" )
key_otus <- read.csv(key_otus , row.names = 1)

umbral <- min( sort( key_otus[ , args[3]]  , decreasing = TRUE) [1:50] )
umbral <- which( key_otus[ , args[3]] > umbral )

key_otus <- row.names(key_otus)[umbral]
key_otus
key_otus <- community[ key_otus ,   ]
#  print(key_otus)





abundace_key_otus <- colSums(key_otus)
#print(abundace_key_otus)
#random <- list()

#for ( i in 1:10000){
 # random_i <- sample(row.names(community) , size = dim(key_otus)[1] )
  #random_i <- community[random_i , ]
  #random[[i]] <- colSums(random_i)
#}



diversity_community <- c()

for (i in 1:dim(community)[2]){
  diversity_i <- diversity(as.vector(community[ , i]) )
  diversity_i <- diversity_i/log(dim(community)[1])
  diversity_community <- c(diversity_community , diversity_i)
}

df <- data.frame(
  x_var = abundace_key_otus,
  y_var = diversity_community
)


ggplot(data = df, aes(x = x_var, y = y_var)) +
  geom_point(size = 5)

ggsave(paste0("../results/figures/correlation_diversity_" , args[1] , "_" , args[2] , "_" , args[3] , ".png") , plot = last_plot() , device = "png")



#print(cor(abundace_key_otus , diversity_community , method = "spearman"))
print(paste0(args[1] , "_" , args[2] , "_" , args[3]))
print(cor.test(abundace_key_otus , diversity_community , alternative = "greater" , method = "spearman"))
#distribution <- c()

#for (i in 1:10000){
#  distribution <- c(distribution , cor(abundace_key_otus , random[[i]]))
  
#}

#print(mean(distribution))
#print(sd(distribution))
