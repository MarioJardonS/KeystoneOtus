#!/usr/bin/env Rscript
## -----------------------------------------------------------------------------------------------------------------------


args = commandArgs(trailingOnly=TRUE)


metadatos <- args[1]

etiqueta <- args[2]

clases <- args[3]

nivel <- args[4]

metadatos <- read.csv(metadatos)

archivos <- metadatos[ , "ID"][which(metadatos[ , clases] == etiqueta)]



columnas <- list()
list_nombres <- list()

for (i in 1:length(archivos)){
  
  archivo_i <- read.table(paste0("corrected" , archivos[i] , "." , nivel , ".bracken.tsv") )
  
  columnas[[i]] <- archivo_i[2:dim(archivo_i)[1] , 6]
  list_nombres[[i]] <- archivo_i[2:dim(archivo_i)[1] , 1]
  names(columnas[[i]]) <- archivo_i[2:dim(archivo_i)[1] , 1]
}

nombres <- c()
for (i in 1:length(list_nombres)){
  nombres <- union(nombres , list_nombres[[i]])
}

tabla <- data.frame(nombres)

for ( j in 1:length(archivos) ){
  columna_j <- c()
  for (i in 1:length(nombres)){
    if (is.element(nombres[i], list_nombres[[j]])){
      columna_j <- c(columna_j , columnas[[j]][ nombres[i]] )
    }else{ 
      columna_j <- c(columna_j , 0)
    }
  }
  tabla <- cbind(tabla , columna_j)
}


row.names(tabla) <- tabla[ , 1]

tabla <- tabla[ , 2:dim(tabla)[2]]


colnames(tabla) <- archivos
write.csv(tabla , file = paste0("bracken_table_" , etiqueta , "_" , nivel , ".csv"))