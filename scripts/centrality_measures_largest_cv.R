#!/usr/bin/env Rscript

args = commandArgs(trailingOnly=TRUE)
##tabla de OTUs general, tabla de OTUs particulares (interesan los otus clave)

library(statip)
library(phyloseq)
library(ggplot2)
library(dplyr)


##carga de tablas
key_otus <- paste0( "../results/central_otus/" , args[1] )
data <- paste0( "../data/tables/" , args[2] )
measures <- paste0("../results/otus_by_centrality/" , args[3])


key_otus <- read.csv(key_otus , row.names = 1 ) #se asume que es una tabla de salida del script ./first_analysis.R

if (substr( data ,  length(data) - 3 , length(data)  ) == ".csv"){
  data <- read.csv( data , row.names = 1 , header = TRUE )
} else {
  data <- read.table( data , row.names = 1 , header = TRUE , sep = "" )
}
#print(head(data))
#normalización de las distribuciones
for(i in 1:dim(data)[2]){
  data[ , i] <- data[ , i]/sum(data[ , i ])
  #print(data[ , i])
}
#print(head(data))

measures <- read.csv(measures , row.names = 1)

#calculo de coeficiente de variacion para todos los otus, para obtener su promedio
cv_all <- c()

for (x in 1:dim(data)[1]) {
  
  #print(is.numeric(as.numeric(data[x,])))
  cv_x <- cv(as.numeric(data[x , ]) )
  #rint(data[x , ])
  cv_all <- c(cv_all , cv_x)
  #if(is.na(cv_x) == FALSE){
  # print(cv_x)
  #}
}

data$cv <- cv_all

min_cv <- row.names(data[ sort(data$cv ) ][ 1:50 ,  ])

min_cv <- measures[ min_cv , c("degrees" , "closeness" , "betweenness") ]

key_otus <- key_otus[ , c("degrees" , "closeness" , "betweenness")]

#print(min_cv)
#print(key_otus[ , c("degrees" , "closeness" , "betweenness")])

comparison <- bind_rows( list( "Keystone OTUs" = key_otus , "Least CV OTUs" =  min_cv  ) , .id = "Type")
#print(comparison)

degree <- ggplot(comparison , mapping = aes(x = .data[["Type"]], y = .data[["degrees"]])) + 
  geom_boxplot() +
  labs(y = "Degree", x = "Type")

ggsave(paste0(args[2] , "_degree_key_min_cv.png") , plot = degree , device = png)


closeness <- ggplot(comparison , mapping = aes(x = .data[["Type"]], y = .data[["closeness"]])) + 
  geom_boxplot() +
  labs(y = "Closeness", x = "Type")

ggsave(paste0(args[2] , "_closeness_key_min_cv.png") , plot = closeness , device = png)



betweenness <- ggplot(comparison , mapping = aes(x = .data[["Type"]], y = .data[["betweenness"]])) + 
  geom_boxplot() +
  labs(y = "Betweenness", x = "Type")

ggsave(paste0(args[2] , "_betweenness_key_min_cv.png") , plot = betweenness , device = png)
