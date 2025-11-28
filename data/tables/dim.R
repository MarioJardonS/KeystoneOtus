#!/usr/bin/env Rscript

args = commandArgs(trailingOnly=TRUE)

data <- read.csv(args[1]) 

cat(dim(data))
