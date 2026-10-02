#!/usr/bin/env Rscript

args = commandArgs(trailingOnly=TRUE)


taxonomia <- read.csv("../../taxonomy_def_biom.csv" , row.names = "OTU")

taxonomia <- taxonomia[ , c("Genus" , "Species") ]

key_otus <- paste0("../results/central_otus/" , args[1] , "_" , args[2] , "_" , args[3] , "_otus.csv" )
key_otus <- read.csv(key_otus , row.names = 1)

key_otus <- row.names(key_otus)

key_otus <- taxonomia[ key_otus , ]

#write.csv(key_otus , file = paste0(args[3] , "_high_" , args[2]  , "_taxonomy.csv"))
write.csv(key_otus , file = paste0("../results/central_otus/" , args[1] , "_" , args[2] , "_" , args[3] , "_otus_taxonomy.csv" ))


