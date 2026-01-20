#!/usr/bin/env Rscript
## -----------------------------------------------------------------------------------------------------------------------


args = commandArgs(trailingOnly=TRUE)

taxonomy <- read.table("taxonomy.txt" , header = TRUE )
data <- read.csv(args[1] , header = TRUE , row.names = 1)

data <- data[ , c("Genus" , "Species")  ]

target <- rbind(data , taxonomy)
write.table(target , "taxonomy.txt")
