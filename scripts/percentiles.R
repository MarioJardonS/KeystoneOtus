#!/usr/bin/env Rscript
library(statip)
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


alpha_diversity_community <- c()

for (i in 1:dim(community)[2]){
  diversity_i <- diversity(as.vector(community[ , i]) )
  diversity_i <- diversity_i/log(dim(community)[1])
  alpha_diversity_community <- c(alpha_diversity_community , diversity_i)
}

beta_diversity_community <- vegdist(as.matrix(t(community)) , method = "bray")
beta_diversity_community <- as.matrix(beta_diversity_community)



#centralidad

c_measures <- paste0("../results/otus_by_centrality/" , args[1] , "_" , args[2] , "_centrality_measures.csv")
c_measures<- read.csv(c_measures , header = 1, row.names = 1)

#funciones para aplicar a cada percentil

cv_mean <- function(df){
  cv_key <- c()
  for(i in 1:dim(df)[1]){
    cv_i <- cv(as.numeric(df[ i , ]))
    cv_key <- c(cv_key , cv_i)
    #print(c(key_otu_i , cv_i , cv_all_mean - cv_i ))
    return(mean(cv_key))
  }
  
}

cor_alpha <- function(df){
  abundance_df <- colSums(df)
  res <- cor.test(abundance_df , alpha_diversity_community , alternative = "greater" , method = "spearman")
  return(c(res$estimate , res$p.value))
}

cor_beta <- function(df){
  abundance_df <- colSums(df)
  composition_change <- c()
  product_of_abundance <- c()
  
  for (i in 1:(length(abundance_df) - 1)){
    for (j in (i+1):length(abundance_df)){
      if (i != j){
        composition_change <- c(composition_change , beta_diversity_community[i ,j ]  )
        product_of_abundance <- c(product_of_abundance , abundance_df[i]*abundance_df[j] ) 
      }
    }
  }
  res <- cor.test(product_of_abundance , composition_change , alternative = "less" , method = "spearman")
  return(c(res$estimate , res$p.value))
}


#evaluacion de medidas segun uso de percentiles


percentile_correlations <- list()

for (i in 1:100){
  hdeg <- which(c_measures$degrees >= quantile(c_measures$degrees , probs = seq(0, 1, 0.01))[101 - i])
  hclose <- which(c_measures$closeness >= quantile(c_measures$closeness , probs = seq(0, 1, 0.01))[101 - i])
  lbetween <- which(c_measures$betweenness <= quantile(c_measures$betweenness , probs = seq(0, 1, 0.01))[i + 1])
  
  X <- intersect(hdeg,hclose)
  X <- intersect(X , lbetween)
  X <- row.names(c_measures)[X]
  if (length(X) > 0 ){
    data_X <- community[ X ,  ]
    if (cor_alpha(data_X)[2] < 0.05 && cor_beta(data_X)[2] < 0.05){
      vec <- c(cor_alpha(data_X)[1] , cor_beta(data_X)[1] , i) # cv_mean(data_X) , i)
      percentile_correlations[[length(percentile_correlations) + 1]] <- vec
    }
    
  }
  
}




percentile_degree <- list()
percentile_closeness <- list()
percentile_betweennness <- list()

for (i in 5:100){
  hdeg <- which(c_measures$degrees >= quantile(c_measures$degrees , probs = seq(0, 1, 0.01))[101 - i])
  hclose <- which(c_measures$closeness >= quantile(c_measures$closeness , probs = seq(0, 1, 0.01))[101 - i])
  hbetween <- which(c_measures$betweenness >= quantile(c_measures$betweenness , probs = seq(0, 1, 0.01))[101 - i])
  
  hdeg <- row.names(c_measures)[hdeg]
  hclose <- row.names(c_measures)[hclose]
  hbetween <- row.names(c_measures)[hbetween]
  
  data_deg <- community[ hdeg , ]
  data_close <- community[ hclose , ]
  data_between <- community[ hbetween , ]
  
  if (cor_alpha(data_deg)[2] < 0.05 && cor_beta(data_deg)[2] < 0.05){
    vec <- c(cor_alpha(data_deg)[1] , cor_beta(data_deg)[1] , i) # cv_mean(data_deg) , i)
    percentile_degree[[length(percentile_degree) + 1]] <- vec
  }
  
  
  if (cor_alpha(data_close)[2] < 0.05 && cor_beta(data_close)[2] < 0.05){
    vec <- c(cor_alpha(data_close)[1] , cor_beta(data_close)[1] , i) #cv_mean(data_close) , i)
    percentile_closeness[[length(percentile_closeness) + 1]] <- vec
  }
  
  
  if (cor_alpha(data_between)[2] < 0.05 && cor_beta(data_between)[2] < 0.05){
    vec <- c(cor_alpha(data_between)[1] , cor_beta(data_between)[1] , i) # cv_mean(data_between) , i)
    percentile_betweennness[[length(percentile_betweennness) + 1]] <- vec
  }
}


if (length(percentile_correlations) > 0 ){
  percentile_correlations <- data.frame(matrix(unlist(percentile_correlations), nrow=length(percentile_correlations), byrow=TRUE))
  
  plot <- ggplot(percentile_correlations, aes(X3)) + 
    geom_line(aes(y = X1, colour = "X1")) + 
    geom_line(aes(y = X2, colour = "X2")) # + 
    #geom_line(aes(y = X3 , colour = "X3"))
  
  ggsave(paste0("../results/figures/" , args[1] , "_" , args[2] , "_percentiles.png") , plot = last_plot() , device = "png")
}



if (length(percentile_degree) > 0 ){
  percentile_degree <- data.frame(matrix(unlist(percentile_degree), nrow=length(percentile_degree), byrow=TRUE))
  plot_deg <- ggplot(percentile_degree , aes(X3))  + 
    geom_line(aes(y = X1, colour = "X1")) + 
    geom_line(aes(y = X2, colour = "X2")) # + 
    #geom_line(aes(y = X3 , colour = "X3"))
  
  
  ggsave(paste0("../results/figures/" , args[1] , "_" , args[2] , "_degree_percentiles.png") , plot = last_plot() , device = "png")
  
}

if (length(percentile_closeness) > 0 ){
  percentile_closeness <- data.frame(matrix(unlist(percentile_closeness), nrow=length(percentile_closeness), byrow=TRUE))
  plot_deg <- ggplot(percentile_closeness , aes(X3))  + 
    geom_line(aes(y = X1, colour = "X1")) + 
    geom_line(aes(y = X2, colour = "X2")) #+ 
    #geom_line(aes(y = X3 , colour = "X3"))
  
  
  ggsave(paste0("../results/figures/" , args[1] , "_" , args[2] , "_closeness_percentiles.png") , plot = last_plot() , device = "png")
  
}


if (length(percentile_betweennness) > 0 ){
  percentile_betweennness <- data.frame(matrix(unlist(percentile_betweennness), nrow=length(percentile_betweennness), byrow=TRUE))
  head(percentile_betweennness)
  plot_deg <- ggplot(percentile_betweennness , aes(X3))  + 
    geom_line(aes(y = X1, colour = "X1")) + 
    geom_line(aes(y = X2, colour = "X2")) #+ 
    #geom_line(aes(y = X3 , colour = "X3"))
  
  
  ggsave(paste0("../results/figures/" , args[1] , "_" , args[2] , "_betweenness_percentiles.png") , plot = last_plot() , device = "png")
  
}

#print(args[1])
#print(percentile_correlations)




