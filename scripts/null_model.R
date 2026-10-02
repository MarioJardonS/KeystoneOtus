#!/usr/bin/env Rscript

library(statip)
library(vegan)
library(ggplot2)

args = commandArgs(trailingOnly=TRUE)

cat(paste0(args[1]) , "," )

#Datos de comunidad y normalizacion
community <- paste0( "./ecologia/KeystoneOtus/data/tables/" , args[1] , "_" , args[2] , ".tsv" )
community <- read.table(community , header = 1, row.names = 1 , sep = "\t")
#community <- paste0( "./ecologia/KeystoneOtus/data/tables/" , args[1] , "_" , args[2] , ".csv" )
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


key_otus <- paste0( "./ecologia/" , args[1] , "_" , args[2] ,"_keystone_otus.csv" )
key_otus <- read.csv(key_otus , row.names = 1)
key_otus <- community[ row.names(key_otus) ,   ]
  
diversity_community <- c()

for (i in 1:dim(community)[2]){
  diversity_i <- diversity(as.vector(community[ , i]) )
  diversity_i <- diversity_i/log(dim(community)[1])
  diversity_community <- c(diversity_community , diversity_i)
}


beta_diversity_community <- vegdist(as.matrix(t(community)) , method = "bray")
beta_diversity_community <- as.matrix(beta_diversity_community)
#head(beta_diversity_community)




abundance <- c()

for (i in 1:dim(community)[1]){
  abundance_i <- sum(community[ i , ])
  abundance <- c(abundance , abundance_i)
}

names(abundance) <- row.names(community)

abundance_key_otus <- abundance[row.names(key_otus)]

umbral_max <- max(abundance_key_otus)
umbral_min <- min(abundance_key_otus)

umbral <- which( umbral_min < abundance & abundance < umbral_max)
cat(paste0(as.character(length(umbral)/length(abundance) ) , ","  ) )


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


cv_X <- function(X){
  cv_s <- c()
  for(i in X){
    cv_i <- cv(as.numeric(community[i , ]))
    cv_s <- c(cv_s , cv_i)
  
}
  return(cv_s)
}



cv_data <- function(tabla){
  cv_vector <- c()
  for ( i in 1:(dim(tabla)[1])){
    cv_i <- cv(as.numeric(tabla[ i , ]))
    cv_vector <- c(cv_vector , cv_i)
    
  }
  return(cv_vector)
}


random_abundance <- list()
random_product_abundance <- list()
random_cv <- list()

for ( i in 1:10000){
  #random_i <- sample(row.names(community) , size = dim(key_otus)[1] )
  random_i <- sample(row.names(community[umbral , ]) , size = dim(key_otus)[1] )
  random_i <- community[random_i , ]
  random_abundance[[i]] <- colSums(random_i)
  random_product_abundance[[i]] <- product_of_abundance(colSums(random_i))
  random_cv[[i]] <- cv_data(random_i)
}


random_cv <- data.frame(matrix(unlist(random_cv), ncol=length(random_cv), byrow=TRUE))
write.csv(random_cv , paste0(args[1] , "_" , args[2] , "nulo_cv.csv" ))



alpha_correlations <- c()
#alpha_p_values <- c()
alpha_correlations_dif <- c()
beta_correlations <- c()
beta_correlations_dif <- c()
#beta_p_values <- c()
#cvs_alpha <- c()
#cvs_beta <- c()


for (i in 1:10000){
  if (cor.test(random_abundance[[i]] , diversity_community , method = "spearman" , alternative = "greater")$p.value < 0.05){
    if(cor.test(random_product_abundance[[i]] , composition_change , method = "spearman" , alternative = "less")$p.value < 0.05){
      cv_alpha <- cv(random_abundance[[i]]) 
      #cvs_alpha <- c(cvs_alpha , cv_alpha)
      alpha_correlations_dif <- c(alpha_correlations_dif ,  (cor.test(random_abundance[[i]] , diversity_community , method = "spearman" , alternative = "greater")$estimate)/cv_alpha)
      alpha_correlations <- c(alpha_correlations ,  (cor.test(random_abundance[[i]] , diversity_community , method = "spearman" , alternative = "greater")$estimate))
 # alpha_p_values <- c(alpha_p_values , cor.test(random_abundance[[i]] , diversity_community , method = "spearman" , alternative = "greater")$p.value)
      cv_beta <- cv(random_product_abundance[[i]]) 
      #cvs_beta <- c(cvs_beta , cv_beta)
      beta_correlations_dif <- c(beta_correlations_dif ,  (cor.test(random_product_abundance[[i]] , composition_change , method = "spearman" , alternative = "less")$estimate)/cv_beta)
      beta_correlations <- c(beta_correlations ,  (cor.test(random_product_abundance[[i]] , composition_change , method = "spearman" , alternative = "less")$estimate))
  #beta_p_values <- c(beta_p_values , cor.test(random_product_abundance[[i]] , composition_change , method = "spearman" , alternative = "less")$p.value)
 # cvs <- c(cvs ,mean(random_cv[[i]]))
    }
  }
}

#cat(length(alpha_correlations))
#cat(summary(alpha_correlations))
#cat(summary(alpha_p_values))
#cat(summary(beta_correlations))
#cat(summary(beta_p_values))
#cat(summary(cvs))



percentil_alpha <- quantile(alpha_correlations , probs = seq(0, 1, 0.01))[96]
percentil_beta <- quantile(beta_correlations , probs = seq(0,1,0.01))[5]

percentil_alpha_dif <- quantile(alpha_correlations_dif , probs = seq(0, 1, 0.01))[96]
percentil_beta_dif <- quantile(beta_correlations_dif , probs = seq(0,1,0.01))[5]

cat(paste0(as.character(length(alpha_correlations)/10000) , "," , as.character(percentil_alpha) , "," , as.character(percentil_beta) , "," , as.character(percentil_alpha_dif) , "," , as.character(percentil_beta_dif) , "\n"))



