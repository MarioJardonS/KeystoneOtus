#!/usr/bin/env Rscript

library(vegan)
library(ggplot2)

args = commandArgs(trailingOnly=TRUE)

#Datos de comunidad y normalizacion
community <- paste0( "../data/tables/" , args[1]  )
community <- read.table(community , header = 1, row.names = 1)

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

#Trabajo con subconjunto "clave"

key_otus <- paste0( "../results/otus_by_centrality/" , args[2]  )
key_otus <- read.csv(key_otus , row.names = 1)

umbral <- min( sort( key_otus[ , args[3]]  , decreasing = TRUE) [1:50] )
umbral <- which( key_otus[ , args[3]] > umbral )

key_otus <- row.names(key_otus)[umbral]
key_otus
key_otus <- community[ key_otus ,   ]


#print(row.names(key_otus))
key_otus <- community[ row.names(key_otus) ,   ]






abundance_key_otus <- colSums(key_otus)
#abundance_key_otus <- sort(abundance_key_otus , decreasing = TRUE)
#abundance_key_otus
#print(abundace_key_otus)
#random <- list()

#for ( i in 1:10000){
# random_i <- sample(row.names(community) , size = dim(key_otus)[1] )
#random_i <- community[random_i , ]
#random[[i]] <- colSums(random_i)
#}



beta_diversity_community <- vegdist(as.matrix(t(community)) , method = "bray")
beta_diversity_community <- as.matrix(beta_diversity_community)
#head(beta_diversity_community)

composition_change <- c()
product_of_abundance <- c()

for (i in 1:(length(abundance_key_otus) - 1)){
  for (j in (i+1):length(abundance_key_otus)){
   if (i != j){
     composition_change <- c(composition_change , beta_diversity_community[i ,j ]  )
     product_of_abundance <- c(product_of_abundance , abundance_key_otus[i]*abundance_key_otus[j] ) 
    }
  }
}



df <- data.frame(
  x_var = product_of_abundance,
  y_var = composition_change
)


ggplot(data = df, aes(x = x_var, y = y_var)) +
  geom_point(size = 5)

ggsave(paste0("../results/figures/correlation_beta_diversity_" , args[4] , "_" , args[3] , ".png") , plot = last_plot() , device = "png")



#print(cor(abundace_key_otus , diversity_community , method = "spearman"))
print(paste0(args[1] , "_" , args[2] , "_keystone"))
print(cor.test(product_of_abundance , composition_change , alternative = "less" , method = "spearman"))
#distribution <- c()

#for (i in 1:10000){
#  distribution <- c(distribution , cor(abundace_key_otus , random[[i]]))

#}

#print(mean(distribution))
#print(sd(distribution))
