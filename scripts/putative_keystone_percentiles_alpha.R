#!/usr/bin/env Rscript
## -----------------------------------------------------------------------------------------------------------------------
library(statip)
library(vegan)
library(ggplot2)


args = commandArgs(trailingOnly=TRUE)



#Datos de comunidad y normalizacion
community <- paste0( "./KeystoneOtus/data/tables/" , args[1] , "_" , args[2] , ".tsv" )
community <- read.table(community , header = 1, row.names = 1 , sep = "\t")
#community <- paste0( "./KeystoneOtus/data/tables/" , args[1] , "_" , args[2] , ".csv" )
#community <- read.csv(community , header = 1, row.names = 1 )

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

composition_change <- c()
 
for (i in 1:(length(alpha_diversity_community) - 1)){
  for (j in (i+1):length(alpha_diversity_community)){
    if (i != j){
      composition_change<- c(composition_change , beta_diversity_community[i ,j ]  )
        
    }
  }
}
  




#medidas de centralidad

c_measures <- paste0("./KeystoneOtus/results/otus_by_centrality/" , args[1] , "_" , args[2] , "_centrality_measures.csv")
c_measures <- read.csv(c_measures , header = 1, row.names = 1)



#selección de core de OTUS
core <- c()
for (i in 1:dim(community)[1]) {
  
  v_i <- as.vector(community[i,1:(dim(community)[2])])
  
  
  if (length(v_i [ v_i > 0 ]) == dim(community)[2] ) {
    core <- c(core, i)
}
}

community_core <- community[core , ]


#calculo de coeficiente de variacion para todos los otus, para obtener su promedio
cv_all <- c()

for (x in 1:dim(community)[1]) {
  cv_x <- cv(as.numeric(community[x , ]) )
  
  
  if(is.na(cv_x) == FALSE){
   cv_all <- c(cv_all , cv_x)
  }
}


cv_all_mean <- mean(cv_all)

#lo anterior, pero con los core
cv_core <- c()
for (x in 1:dim(community_core)[1]) {
  
  cv_x <- cv(as.numeric(community_core[x , ]) )
  
  cv_core <- c(cv_core , cv_x)
}  


cv_core_mean <- mean(cv_core)





#funciones para buscar el percentil adecuado


cor_alpha <- function(df){
  abundance_df <- colSums(df)
  cv_alpha <- cv(abundance_df)
  res <- cor.test(abundance_df , alpha_diversity_community , alternative = "greater" , method = "spearman")
  return(c(res$estimate , res$p.value , cv_alpha))
}



product_of_abundance <- function(df){
  abundance_df <- colSums(df)
  p_ab <- c()
  for (i in 1:(length(abundance_df) - 1)){
    for (j in (i+1):length(abundance_df)){
      if (i != j){
        p_ab <- c(p_ab , abundance_df[i]*abundance_df[j]  )
        
      }
    }
  }
  return(p_ab)
}



cor_beta <- function(df){
  
#  change <- composition_change(df)
  product <- product_of_abundance(df)
  cv_beta <- cv(product)
  #dif_b <- max(product) - min(product)
  res <- cor.test(product , composition_change , alternative = "less" , method = "spearman")
  return(c(res$estimate , res$p.value , cv_beta))
}

cv_X <- function(X){
  cv_s <- c()
  for(i in X){
    cv_i <- cv(as.numeric(community[i , ]))
    cv_s <- c(cv_s , cv_i)
  
}
  return(cv_s)
}

percentil_k <- function(n){
  hdeg <- which(c_measures$degrees >= quantile(c_measures$degrees , probs = seq(0, 1, 0.001))[1001 - n])
  hclose <- which(c_measures$closeness >= quantile(c_measures$closeness , probs = seq(0, 1, 0.001))[1001 - n])
  lbetween <- which(c_measures$betweenness <= quantile(c_measures$betweenness , probs = seq(0, 1, 0.001))[n + 1])
  
  X <- intersect(hdeg,hclose)
  X <- intersect(X , lbetween)
  X <- row.names(c_measures)[X]
  return(X)
}

percentil_d <- function(n){
  X <- which(c_measures$degrees >= quantile(c_measures$degrees , probs = seq(0, 1, 0.001))[1001 - n])
  X <- row.names(c_measures)[X]
  return(X)
}


percentil_c <- function(n){
  X <- which(c_measures$closeness >= quantile(c_measures$closeness , probs = seq(0, 1, 0.001))[1001 - n])
  X <- row.names(c_measures)[X]
  return(X)
}


percentil_b <- function(n){
  X <- which(c_measures$betweenness >= quantile(c_measures$betweenness , probs = seq(0, 1, 0.001))[1001 - n])
  X <- row.names(c_measures)[X]
  return(X)
}
#proceso para buscar alto y bajo en las medidas de centralidad 

for ( i in 1:999){
  K <- percentil_k(i)
 #print(i)
  if (length(K) > 5 ){
    data_K <- community[ K , ]
    if (is.numeric(cor_alpha(data_K))){
      if (  cor_alpha(data_K)[2] < 0.05){
      #alpha <- cor_alpha(data_K)[1]  
      break
    }
}
}
}

alpha <- cor_alpha(data_K)[1]  

for ( j in (i+1):999){
  K <- percentil_k(j)
 # print(j)
  data_K <- community[K , ]
  if(is.numeric(cor_alpha(data_K))){
    if (cor_alpha(data_K)[1] > alpha && cor_alpha(data_K)[2] < 0.05 ){
    alpha <- cor_alpha(data_K)[1]
  }else{
    k <- j - 1
    K <- percentil_k(k)
    data_K <- community[K , ]
    break
  }
  
  
}
  
  
}

keystone <- c_measures[ K , ]

abundance_key_otus <- colSums(data_K)


df_K <- data.frame(
  x_var = abundance_key_otus,
  y_var = alpha_diversity_community
)


ggplot(data = df_K, aes(x = x_var, y = y_var)) +
  geom_point(size = 5)

ggsave(paste0("./correlation_alpha_diversity_" , args[1] , "_" , args[2] , "_keystone.png") , plot = last_plot() , width = 7 , height = 7 , units = "in" ,  device = "png")


df_K1 <- data.frame(
  x_var = product_of_abundance(data_K),
  y_var = composition_change
)


ggplot(data = df_K1, aes(x = x_var, y = y_var)) +
  geom_point(size = 5)

ggsave(paste0("./correlation_beta_diversity_" , args[1] , "_" , args[2] , "_keystone.png") , plot = last_plot() , width = 7 , height = 7 , units = "in" , device = "png")

cv_K <- cv_X(K)

plot_data <- data.frame( otu = row.names(data_K) , coef_v = cv_K , Media = cv_all_mean , Media_Core = cv_core_mean)

plot <- ggplot() + 
  geom_point(data = plot_data , aes(x = otu , y = coef_v , group = 1 , size = 5)) +
  geom_line(data = plot_data , aes(x = otu , y = Media , group = 1 , color = 'blue')) +
  geom_line(data = plot_data , aes(x = otu , y = Media_Core , group = 1 , color = 'red')) +
                       theme(axis.text.x = element_text(angle = 45, hjust = 1) )


ggsave(paste0("./coefficient_of_variation_" , args[1] , "_" , args[2] , "_keystone_otus" , ".png") , plot = plot , width = 7 , height = 7 , units = "in" ,  device = "png")


test_cv <- c()
t_K <- t.test(cv_K , alternative = "less" , mu = cv_all_mean )
#print(t_K)
if ( t_K$p.value < 0.05){
  test_cv <- c(test_cv , TRUE)
}else{
  test_cv <- c(test_cv , FALSE)
}


t_K <- t.test(cv_K , alternative = "less" , mu = cv_core_mean )
#print(t_K)
if ( t_K$p.value < 0.05 ){
  test_cv <- c(test_cv , TRUE)
}else{
  test_cv <- c(test_cv , FALSE)
}


write.csv(keystone , paste0(args[1] , "_" , args[2] , "_keystone_otus.csv" ) , row.names = TRUE)

cat(paste0(args[1] , "," , as.character(k) , ","  , as.character(test_cv[1]) , "," , as.character(test_cv[2]) , ","  ))
cat(paste0( as.character(cor_alpha(data_K)[1]) ,"," , as.character(cor_alpha(data_K)[1]/cor_alpha(data_K)[3]) ,"," , as.character(cor_alpha(data_K)[2]) ,"," ) )
cat(paste0(as.character(cor_beta(data_K)[1]) , "," ,  as.character(cor_beta(data_K)[1]/cor_beta(data_K)[3]) , "," , as.character(cor_beta(data_K)[2]) , ","))
cat(paste0(as.character(dim(keystone)[1]) , "," , as.character(min(keystone$degrees)) , "," , as.character(min(keystone$closeness)) ,"," , as.character(max(keystone$betweenness)) , ","))


for ( i in 1:999){
  D <- percentil_d(i)
  #print(i)
  if (length(D) > 5 ){
    data_D <- community[ D , ]
    if (is.numeric(cor_alpha(data_D))){
      if (  cor_alpha(data_D)[2] < 0.05){
#      alpha <- cor_alpha(data_D)[1]  
      break
    }
   
   
}
        
}
}

alpha <- cor_alpha(data_D)[1]

for ( j in (i+1):999){
  D <- percentil_d(j)
  #print(j)
  data_D <- community[D , ]
  if(is.numeric(cor_alpha(data_D))){
    if (cor_alpha(data_D)[1] > alpha && cor_alpha(data_D)[2] < 0.05 ){
    alpha <- cor_alpha(data_D)[1]
  }else{
    d <- j - 1
    D <- percentil_d(d)
    data_D <- community[D , ]
    break
  }
  
  
}
  
  
}


high_degree <- c_measures [ D , ]


abundance_high_degree <- colSums(data_D)


df_D <- data.frame(
  x_var = abundance_high_degree,
  y_var = alpha_diversity_community
)


ggplot(data = df_D, aes(x = x_var, y = y_var)) +
  geom_point(size = 5)

ggsave(paste0("./correlation_diversity_" , args[1] , "_" , args[2] , "_high_degree.png") , plot = last_plot() , width = 7 , height = 7 , units = "in" , device = "png")



df_D1 <- data.frame(
  x_var = product_of_abundance(data_D),
  y_var = composition_change
)


ggplot(data = df_D1, aes(x = x_var, y = y_var)) +
  geom_point(size = 5)

ggsave(paste0("./correlation_beta_diversity_" , args[1] , "_" , args[2] , "_high_degree.png") , plot = last_plot() , width = 7 , height = 7 , units = "in" ,  device = "png")


cv_D <- cv_X(D)

plot_data <- data.frame( otu = row.names(data_D) , coef_v = cv_D , Media = cv_all_mean , Media_Core = cv_core_mean)

plot <- ggplot() + 
  geom_point(data = plot_data , aes(x = otu , y = coef_v , group = 1 , size = 5)) +
  geom_line(data = plot_data , aes(x = otu , y = Media , group = 1 , color = 'blue')) +
  geom_line(data = plot_data , aes(x = otu , y = Media_Core , group = 1 , color = 'red')) +
                       theme(axis.text.x = element_text(angle = 45, hjust = 1) )


ggsave(paste0("./coefficient_of_variation_" , args[1] , "_" , args[2] , "_high_degree" , ".png") , plot = plot , width = 7 , height = 7 , units = "in" , device = "png")



test_cv <- c()
t_D <- t.test(cv_D , alternative = "less" , mu = cv_all_mean )
#print(t_K)
if ( t_D$p.value < 0.05){
  test_cv <- c(test_cv , TRUE)
}else{
  test_cv <- c(test_cv , FALSE)
}


t_D <- t.test(cv_D , alternative = "less" , mu = cv_core_mean )

if ( t_D$p.value < 0.05 ){
  test_cv <- c(test_cv , TRUE)
}else{
  test_cv <- c(test_cv , FALSE)
}




write.csv(high_degree , paste0(args[1] , "_" , args[2] , "_high_degree_otus.csv" ) , row.names = TRUE)


cat(paste0(as.character(d) , ","  , as.character(test_cv[1]) , "," , as.character(test_cv[2]) , ","   ))
cat(paste0( as.character(cor_alpha(data_D)[1]) ,"," , as.character(cor_alpha(data_D)[1]/cor_alpha(data_D)[3]) ,"," , as.character(cor_alpha(data_D)[2]) ,"," ) )
cat(paste0( as.character(cor_beta(data_D)[1]) ,"," , as.character(cor_beta(data_D)[1]/cor_beta(data_D)[3]) ,"," , as.character(cor_beta(data_D)[2]) ,"," ) )

cat(paste0(as.character(dim(high_degree)[1]) , "," , as.character(min(high_degree$degrees))  , ","))


for ( i in 1:999){
  C <- percentil_c(i)
  if (length(C) > 5 ){
    data_C <- community[ C , ]
    if (is.numeric(cor_alpha(data_C))){
      if (  cor_alpha(data_C)[2] < 0.05){
#      alpha <- cor_alpha(data_C)[1]  
      break
    }
   
   
}
        
}
}

alpha <- cor_alpha(data_C)[1]  

for ( j in (i+1):999){
  C <- percentil_c(j)
  data_C <- community[C , ]
  if(is.numeric(cor_alpha(data_C))){
    if (cor_alpha(data_C)[1] > alpha && cor_alpha(data_C)[2] < 0.05 ){
    alpha <- cor_alpha(data_C)[1]
  }else{
    c <- j - 1
    C <- percentil_c(c)
    data_C <- community[C , ]
    break
  }
  
  
}
  
  
}

high_closeness <- c_measures [ C , ]


abundance_high_closeness <- colSums(data_C)


df_C <- data.frame(
  x_var = abundance_high_closeness,
  y_var = alpha_diversity_community
)


ggplot(data = df_C, aes(x = x_var, y = y_var)) +
  geom_point(size = 5)

ggsave(paste0("./correlation_diversity_" , args[1] , "_" , args[2] , "_high_closeness.png") , plot = last_plot() , width = 7 , height = 7 , units = "in" , device = "png")



df_C1 <- data.frame(
  x_var = product_of_abundance(data_C),
  y_var = composition_change
)


ggplot(data = df_C1, aes(x = x_var, y = y_var)) +
  geom_point(size = 5)

ggsave(paste0("./correlation_beta_diversity_" , args[1] , "_" , args[2] , "_high_closeness.png") , plot = last_plot() , width = 7 , height = 7 , units = "in" ,  device = "png")



cv_C <- cv_X(C)

plot_data <- data.frame( otu = row.names(data_C) , coef_v = cv_C , Media = cv_all_mean , Media_Core = cv_core_mean)

plot <- ggplot() + 
  geom_point(data = plot_data , aes(x = otu , y = coef_v , group = 1 , size = 5)) +
  geom_line(data = plot_data , aes(x = otu , y = Media , group = 1 , color = 'blue')) +
  geom_line(data = plot_data , aes(x = otu , y = Media_Core , group = 1 , color = 'red')) +
                       theme(axis.text.x = element_text(angle = 45, hjust = 1) )


ggsave(paste0("./coefficient_of_variation_" , args[1] , "_" , args[2] , "_high_closeness" , ".png") , plot = plot , width = 7 , height = 7 , units = "in" ,  device = "png")



test_cv <- c()
t_C <- t.test(cv_C , alternative = "less" , mu = cv_all_mean )
#print(t_K)
if ( t_C$p.value < 0.05){
  test_cv <- c(test_cv , TRUE)
}else{
  test_cv <- c(test_cv , FALSE)
}


t_C <- t.test(cv_C , alternative = "less" , mu = cv_core_mean )

if ( t_C$p.value < 0.05 ){
  test_cv <- c(test_cv , TRUE)
}else{
  test_cv <- c(test_cv , FALSE)
}


write.csv(high_closeness , paste0(args[1] , "_" , args[2] , "_high_closeness_otus.csv" ) , row.names = TRUE)



cat(paste0(as.character(c) , "," , as.character(test_cv[1]) , "," , as.character(test_cv[2]) , "," ))
cat(paste0( as.character(cor_alpha(data_C)[1]) ,"," , as.character(cor_alpha(data_C)[1]/cor_alpha(data_C)[3]) ,"," , as.character(cor_alpha(data_C)[2]) ,"," ) )
cat(paste0( as.character(cor_beta(data_C)[1]) ,"," , as.character(cor_beta(data_C)[1]/cor_beta(data_C)[3]) ,"," , as.character(cor_beta(data_C)[2]) ,"," ) )
cat(paste0(as.character(dim(high_closeness)[1]) , "," , as.character(min(high_closeness$closeness))  , ","))

for ( i in 1:999){
  B <- percentil_b(i)
  if (length(B) > 5 ){
    data_B <- community[ B , ]
    if (is.numeric(cor_alpha(data_B))){
      if (  cor_alpha(data_B)[2] < 0.05){
#      alpha <- cor_alpha(data_B)[1]  
      break
    }
   
   
}
        
}
}


alpha <- cor_alpha(data_B)[1]  


for ( j in (i+1):999){
  B <- percentil_b(j)
  data_B <- community[B , ]
  if(is.numeric(cor_alpha(data_B))){
    if (cor_alpha(data_B)[1] > alpha && cor_alpha(data_B)[2] < 0.05 ){
    alpha <- cor_alpha(data_B)[1]
  }else{
    b <- j - 1
    B <- percentil_b(b)
    data_B <- community[B , ]
    break
  }
  
  
}
  
  
}

high_betweenness <- c_measures [ B , ]


abundance_high_betweenness <- colSums(data_B)


df_B <- data.frame(
  x_var = abundance_high_betweenness,
  y_var = alpha_diversity_community
)


ggplot(data = df_B, aes(x = x_var, y = y_var)) +
  geom_point(size = 5)

ggsave(paste0("./correlation_diversity_" , args[1] , "_" , args[2] , "_high_betweenness.png") , plot = last_plot() , width = 7 , height = 7 , units = "in" ,  device = "png")



df_B1 <- data.frame(
  x_var = product_of_abundance(data_B),
  y_var = composition_change
)


ggplot(data = df_B1, aes(x = x_var, y = y_var)) +
  geom_point(size = 5)

ggsave(paste0("./correlation_beta_diversity_" , args[1] , "_" , args[2] , "_high_betweenness.png") , plot = last_plot() , width = 7 , height = 7 , units = "in" , device = "png")



cv_B <- cv_X(B)

plot_data <- data.frame( otu = row.names(data_B) , coef_v = cv_B , Media = cv_all_mean , Media_Core = cv_core_mean)

plot <- ggplot() + 
  geom_point(data = plot_data , aes(x = otu , y = coef_v , group = 1 , size = 5)) +
  geom_line(data = plot_data , aes(x = otu , y = Media , group = 1 , color = 'blue')) +
  geom_line(data = plot_data , aes(x = otu , y = Media_Core , group = 1 , color = 'red')) +
                       theme(axis.text.x = element_text(angle = 45, hjust = 1) )



ggsave(paste0("./coefficient_of_variation_" , args[1] , "_" , args[2] , "_high_betweenness" , ".png") , plot = plot , width = 7 , height = 7 , units = "in" ,  device = "png")



test_cv <- c()
t_B <- t.test(cv_B , alternative = "less" , mu = cv_all_mean )
#print(t_K)
if ( t_B$p.value < 0.05){
  test_cv <- c(test_cv , TRUE)
}else{
  test_cv <- c(test_cv , FALSE)
}


t_B <- t.test(cv_B , alternative = "less" , mu = cv_core_mean )

if ( t_B$p.value < 0.05 ){
  test_cv <- c(test_cv , TRUE)
}else{
  test_cv <- c(test_cv , FALSE)
}



write.csv(high_betweenness , paste0(args[1] , "_" , args[2] , "_high_betweenness_otus.csv" ) , row.names = TRUE)


cat(paste0(as.character(b) , "," , as.character(test_cv[1]) , "," , as.character(test_cv[2]) , "," ))
cat(paste0( as.character(cor_alpha(data_B)[1]) ,"," , as.character(cor_alpha(data_B)[1]/cor_alpha(data_B)[3]) ,"," , as.character(cor_alpha(data_B)[2]) ,"," ) )
cat(paste0( as.character(cor_beta(data_B)[1]) ,"," , as.character(cor_beta(data_B)[1]/cor_beta(data_B)[3]) ,"," , as.character(cor_beta(data_B)[2]) ,"," ) )

cat(paste0(as.character(dim(high_betweenness)[1]) , "," , as.character(min(high_betweenness$betweenness))  , "\n"))

write.csv(cv_K , paste0(args[1] , "_" , args[2] , "_cv_keystone.csv") , row.names = FALSE)
write.csv(cv_D , paste0(args[1] , "_" , args[2] , "_cv_degree.csv") , row.names = FALSE)
write.csv(cv_C , paste0(args[1] , "_" , args[2] , "_cv_closeness.csv") , row.names = FALSE)
write.csv(cv_B , paste0(args[1] , "_" , args[2] , "_cv_betweenness.csv") , row.names = FALSE)

