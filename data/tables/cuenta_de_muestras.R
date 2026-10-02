#!/usr/bin/env Rscript

args = commandArgs(trailingOnly=TRUE)

community <- read.table(args[1] , header = TRUE )


present <- c()

for(i in 1:dim(community)[1]){
  if(sum(community[i ,]) > 0){
    present <- c(present , i)
}
}

print(length(present))
