#!/usr/bin/env Rscript

args = commandArgs(trailingOnly=TRUE)
##tabla de OTUs general, tabla de OTUs particulares (interesan los otus clave)

library(statip)
library(phyloseq)
library(ggplot2)

##carga de tablas
key_otus <- paste0( "../results/central_otus/" , args[1] )
data <- paste0( "../data/tables/" , args[2] )



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

#selección de core de OTUS
core <- c()
for (i in 1:dim(data)[1]) {
  
  v_i <- as.vector(data[i,1:(dim(data)[2])])
  
  
  if (length(v_i [ v_i > 0 ]) == dim(data)[2] ) {
    core <- c(core, i)
}
}

data_core  <- data[core , ]


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


cv_all_mean <- mean(cv_all)

#lo anterior, pero con los core
cv_core <- c()
for (x in 1:dim(data_core)[1]) {
  
  #print(is.numeric(as.numeric(data[x,])))
  cv_x <- cv(as.numeric(data_core[x , ]) )
  #rint(data[x , ])
  cv_core <- c(cv_core , cv_x)
  #if(is.na(cv_x) == FALSE){
  # print(cv_x)
  #}
}


cv_core_mean <- mean(cv_core)


#comparación de cada uno de los coeficientes de variación de los otus clave y lel coeficiente promed
cv_key <- c()

for(i in 1:dim(key_otus)[1]){
  key_otu_i <- row.names(key_otus)[i]
  cv_i <- cv(as.numeric(data[ key_otu_i , ]))
  cv_key <- c(cv_key , cv_i)
  #print(c(key_otu_i , cv_i , cv_all_mean - cv_i ))
}

#dataframe para plot
plot_data <- data.frame( otu = row.names(key_otus) , coef_v = cv_key , Media = cv_all_mean , Media_Core = cv_core_mean)

plot <- ggplot() + 
  geom_line(data = plot_data , aes(x = otu , y = coef_v , group = 1)) +
  geom_line(data = plot_data , aes(x = otu , y = Media , group = 1 , color = 'blue')) +
  geom_line(data = plot_data , aes(x = otu , y = Media_Core , group = 1 , color = 'red')) +
                       theme(axis.text.x = element_text(angle = 45, hjust = 1) )


ggsave(paste0("coeficiente_varicion_" , args[1] , ".png") , plot = plot , device = "png")
