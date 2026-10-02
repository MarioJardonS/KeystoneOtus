#!/usr/bin/env Rscript

#args = commandArgs(trailingOnly=TRUE)

plantas <- c("Arachis_hypogaea" , "Capsicum_annuum" , "Glycine_max" , "Lactuca_sativa_L" , "Malus_domestica" , "Oryza_sativa", "Panax_ginseng" ,"Panax_notoginseng" , "Persea_americana_Mill" , "Sorghum_bicolor" , "Triticum_aestivum" , "Zea_mays_L._hybrid")


rizosferas <- list()

for (i  in 1:12){
  data_planta <- paste0( "../data/tables/" , plantas[i] , "_Rhizosphere.csv"  )
  data_planta <- read.csv(data_planta , header = TRUE , row.names = 1)
  rizosferas[[i]] <- data_planta
}

zeros <- list()

for ( i in 1:12){
  rizosfera_i <- rizosferas[[i]]
  #filtro_i <- c()
  proporcion_i <- c()
  for (j in 1:dim(rizosfera_i)[1] ){
    v_j <- rizosfera_i[j , ]
    if (length( v_j[v_j > 1 ]) > 1 ){
      #filtro_i <- c(filtro_i , j)
      proporcion_i <- c(proporcion_i , length( v_j[v_j > 1 ])/length(v_j) )
    }
  }
  zeros[[i]] <- proporcion_i
}

for ( i in 1:12){
  print(plantas[i])
  print(sum(zeros[[i]]))
  print(length(which(zeros[[i]] < 0.05 ) )  )
  print(dim(rizosferas[[i]]))
}

plantas <- c("chile" , "maiz" , "tomate")
rizosferas <- list()

for (i in 1:3){
  data_planta <- paste0("../results/otus_by_centrality/table.from_" , plantas[i] , "_bydegree.csv")
  data_planta <- read.csv(data_planta , header = TRUE , row.names = 1)
  rizosferas[[i]] <- data_planta
}  

for ( i in 1:3){
  rizosfera_i <- rizosferas[[i]]
  #filtro_i <- c()
  proporcion_i <- c()
  for (j in 1:dim(rizosfera_i)[1] ){
    v_j <- rizosfera_i[j , ]
    if (length( v_j[v_j > 1 ]) > 1 ){
      #filtro_i <- c(filtro_i , j)
      proporcion_i <- c(proporcion_i , length( v_j[v_j > 1 ])/length(v_j) )
    }
  }
  zeros[[i]] <- proporcion_i
}

for ( i in 1:3){
  print(plantas[i])
  print(length(which(zeros[[i]] < 0.05 ) )  )
  print(length(zeros[[i]]))
}

