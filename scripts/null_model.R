#!/usr/bin/env Rscript

library(statip)
library(vegan)
library(ggplot2)

args = commandArgs(trailingOnly=TRUE)

cat(paste0(args[1]) , "," )

#Datos de comunidad y normalizacion
community <- paste0( "../data/tables/" , args[1] , "_" , args[2] , ".tsv" )
community <- read.table(community , header = 1, row.names = 1 , sep = "\t")
#community <- paste0( "../data/tables/" , args[1] , "_" , args[2] , ".csv" )
#community <- read.csv(community , header = 1, row.names = 1)
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
key_otus <- community[ row.names(key_otus) ,   ]

degree <- paste0( "../results/central_otus/" , args[1] , "_" , args[2] ,"_high_degree_otus.csv" )
degree <- read.csv(degree , row.names = 1)
degree <- community[ row.names(degree), ]

closeness <- paste0( "../results/central_otus/" , args[1] , "_" , args[2] ,"_high_closeness_otus.csv" )
closeness <- read.csv(closeness , row.names = 1)
closeness <- community[ row.names(closeness), ]


betweenness <- paste0( "../results/central_otus/" , args[1] , "_" , args[2] ,"_high_betweenness_otus.csv" )
betweenness <- read.csv(betweenness , row.names = 1)
betweenness <- community[ row.names(betweenness), ]
  
diversity_community <- c()

for (i in 1:dim(community)[2]){
  diversity_i <- diversity(as.vector(community[ , i]) )
  diversity_i <- diversity_i/log(dim(community)[1])
  diversity_community <- c(diversity_community , diversity_i)
}


beta_diversity_community <- vegdist(as.matrix(t(community)) , method = "bray")
beta_diversity_community <- as.matrix(beta_diversity_community)
#head(beta_diversity_community)

  
#eleccion de centroide

excentricidad <- c()

for (i in 1:(length(diversity_community))){
  excen_i <- sum(beta_diversity_community[  , i ])
  excentricidad <- c(excentricidad , excen_i)
}

centroide <- which(excentricidad == min(excentricidad))
centroide <- centroide[1]

convergence <- beta_diversity_community[ , centroide]







abundance <- c()

for (i in 1:dim(community)[1]){
  abundance_i <- sum(community[ i , ])
  abundance <- c(abundance , abundance_i)
}

names(abundance) <- row.names(community)



abundance_key_otus <- abundance[row.names(key_otus)]

umbral_max_k <- max(abundance_key_otus)
umbral_min_k <- min(abundance_key_otus)

umbral_k <- which( umbral_min_k < abundance & abundance < umbral_max_k)
cat(paste0(as.character(length(umbral_k)/length(abundance) ) , ","  ) )


abundance_degree <- abundance[row.names(degree)]

umbral_max_d <- max(abundance_degree)
umbral_min_d <- min(abundance_degree)

umbral_d <- which( umbral_min_d < abundance & abundance < umbral_max_d)
cat(paste0(as.character(length(umbral_d)/length(abundance) ) , ","  ) )




abundance_closeness <- abundance[row.names(closeness)]

umbral_max_c <- max(abundance_closeness)
umbral_min_c <- min(abundance_closeness)

umbral_c <- which( umbral_min_c < abundance & abundance < umbral_max_c)
cat(paste0(as.character(length(umbral_c)/length(abundance) ) , ","  ) )



abundance_betweenness <- abundance[row.names(betweenness)]

umbral_max_b <- max(abundance_betweenness)
umbral_min_b <- min(abundance_betweenness)

umbral_b <- which( umbral_min_b < abundance & abundance < umbral_max_b)
cat(paste0(as.character(length(umbral_b)/length(abundance) ) , ","  ) )





composition_change <- c()

for (i in 1:(dim(community)[2] - 1)){
  for (j in (i+1):dim(community)[2]){
    if (i != j){
      composition_change <- c(composition_change , beta_diversity_community[i ,j ]  )
      
    }
  }
}

product_of_abundance <- function(v){
  prod <- c()
  for( i in 1:(length(v) - 1)){
    for(j in (i+1):(length(v))){
      prod <- c(prod , v[i]*v[j])
    }
  }
  return(prod)
}


random_abundance_k <- list()
random_abundance_d <- list()
random_abundance_c <- list()
random_abundance_b <- list()
#random_product_abundance <- list()
#random_cv <- list()

for ( i in 1:10000){
  #random_i <- sample(row.names(community) , size = dim(key_otus)[1] )
  random_k_i <- sample(row.names(community[umbral_k , ]) , size = dim(key_otus)[1] )
  random_k_i <- community[random_k_i , ]
  random_abundance_k[[i]] <- colSums(random_k_i)
#  random_product_abundance[[i]] <- product_of_abundance(colSums(random_i))
 # random_cv[[i]] <- cv_data(random_i)
}



for ( i in 1:10000){
  #random_i <- sample(row.names(community) , size = dim(key_otus)[1] )
  random_d_i <- sample(row.names(community[umbral_d , ]) , size = dim(degree)[1] )
  random_d_i <- community[random_d_i , ]
  random_abundance_d[[i]] <- colSums(random_d_i)
#  random_product_abundance[[i]] <- product_of_abundance(colSums(random_i))
 # random_cv[[i]] <- cv_data(random_i)
}


for ( i in 1:10000){
  #random_i <- sample(row.names(community) , size = dim(key_otus)[1] )
  random_c_i <- sample(row.names(community[umbral_c , ]) , size = dim(closeness)[1] )
  random_c_i <- community[random_c_i , ]
  random_abundance_c[[i]] <- colSums(random_c_i)
#  random_product_abundance[[i]] <- product_of_abundance(colSums(random_i))
 # random_cv[[i]] <- cv_data(random_i)
}


for ( i in 1:10000){
  #random_i <- sample(row.names(community) , size = dim(key_otus)[1] )
  random_b_i <- sample(row.names(community[umbral_b , ]) , size = dim(betweenness)[1] )
  random_b_i <- community[random_b_i , ]
  random_abundance_b[[i]] <- colSums(random_b_i)
#  random_product_abundance[[i]] <- product_of_abundance(colSums(random_i))
 # random_cv[[i]] <- cv_data(random_i)
}



alpha_correlations_k <- c()
beta_correlations_k <- c()

alpha_correlations_d <- c()
beta_correlations_d <- c()

alpha_correlations_c <- c()
beta_correlations_c <- c()

alpha_correlations_b <- c()
beta_correlations_b <- c()



for (i in 1:10000){
  if (cor.test(random_abundance_k[[i]] , diversity_community , method = "spearman" , alternative = "greater")$p.value < 0.05){
    if(cor.test(random_abundance_k[[i]] , convergence , method = "spearman" , alternative = "less")$p.value < 0.05){
      alpha_correlations_k <- c(alpha_correlations_k ,  (cor.test(random_abundance_k[[i]] , diversity_community , method = "spearman" , alternative = "greater")$estimate))
      beta_correlations_k <- c(beta_correlations_k ,  (cor.test(random_abundance_k[[i]] , convergence , method = "spearman" , alternative = "less")$estimate))
    }
  }
}


for (i in 1:10000){
  if (cor.test(random_abundance_d[[i]] , diversity_community , method = "spearman" , alternative = "greater")$p.value < 0.05){
    if(cor.test(random_abundance_d[[i]] , convergence , method = "spearman" , alternative = "less")$p.value < 0.05){
      alpha_correlations_d <- c(alpha_correlations_d ,  (cor.test(random_abundance_d[[i]] , diversity_community , method = "spearman" , alternative = "greater")$estimate))
      beta_correlations_d <- c(beta_correlations_d ,  (cor.test(random_abundance_d[[i]] , convergence , method = "spearman" , alternative = "less")$estimate))
    }
  }
}


for (i in 1:10000){
  if (cor.test(random_abundance_c[[i]] , diversity_community , method = "spearman" , alternative = "greater")$p.value < 0.05){
    if(cor.test(random_abundance_c[[i]] , convergence , method = "spearman" , alternative = "less")$p.value < 0.05){
      alpha_correlations_c <- c(alpha_correlations_c ,  (cor.test(random_abundance_c[[i]] , diversity_community , method = "spearman" , alternative = "greater")$estimate))
      beta_correlations_c <- c(beta_correlations_c ,  (cor.test(random_abundance_c[[i]] , convergence , method = "spearman" , alternative = "less")$estimate))
    }
  }
}



for (i in 1:10000){
  if (cor.test(random_abundance_b[[i]] , diversity_community , method = "spearman" , alternative = "greater")$p.value < 0.05){
    if(cor.test(random_abundance_b[[i]] , convergence , method = "spearman" , alternative = "less")$p.value < 0.05){
      alpha_correlations_b <- c(alpha_correlations_b ,  (cor.test(random_abundance_b[[i]] , diversity_community , method = "spearman" , alternative = "greater")$estimate))
      beta_correlations_b <- c(beta_correlations_b ,  (cor.test(random_abundance_b[[i]] , convergence , method = "spearman" , alternative = "less")$estimate))
    }
  }
}



percentil_alpha_k <- quantile(alpha_correlations_k , probs = seq(0, 1, 0.01))[96]
percentil_beta_k <- quantile(beta_correlations_k , probs = seq(0,1,0.01))[5]


percentil_alpha_d <- quantile(alpha_correlations_d , probs = seq(0, 1, 0.01))[96]
percentil_beta_d <- quantile(beta_correlations_d , probs = seq(0,1,0.01))[5]


percentil_alpha_c <- quantile(alpha_correlations_c , probs = seq(0, 1, 0.01))[96]
percentil_beta_c <- quantile(beta_correlations_c , probs = seq(0,1,0.01))[5]


percentil_alpha_b <- quantile(alpha_correlations_b , probs = seq(0, 1, 0.01))[96]
percentil_beta_b <- quantile(beta_correlations_b , probs = seq(0,1,0.01))[5]



cat(paste0(as.character(length(alpha_correlations_k)/10000) , "," , as.character(percentil_alpha_k) , "," , as.character(percentil_beta_k) , "," ))

cat(paste0(as.character(length(alpha_correlations_d)/10000) , "," , as.character(percentil_alpha_d) , "," , as.character(percentil_beta_d) , "," ))


cat(paste0(as.character(length(alpha_correlations_c)/10000) , "," , as.character(percentil_alpha_c) , "," , as.character(percentil_beta_c) , "," ))


cat(paste0(as.character(length(alpha_correlations_b)/10000) , "," , as.character(percentil_alpha_b) , "," , as.character(percentil_beta_b) , "\n"  ))


