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
    present <- c(present , i)
}
}

community <- community[present, ]



#Trabajo con subconjunto "clave"


key_otus <- paste0( "../results/central_otus/" , args[1] , "_" , args[2] ,"_keystone_otus.csv" )
key_otus <- read.csv(key_otus , row.names = 1)
#print(row.names(key_otus))
key_otus <- community[ row.names(key_otus) ,   ]
  





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

ggsave(paste0("../results/figures/correlation_diversity_" , args[1] , "_" , args[2] , "_keystone.png") , plot = last_plot() , device = "png")



#print(cor(abundace_key_otus , diversity_community , method = "spearman"))
print(paste0(args[1] , "_" , args[2] , "_keystone"))
print(cor.test(abundace_key_otus , diversity_community , alternative = "greater" , method = "spearman"))
#distribution <- c()

#for (i in 1:10000){
#  distribution <- c(distribution , cor(abundace_key_otus , random[[i]]))
  
#}

#print(mean(distribution))
#print(sd(distribution))
