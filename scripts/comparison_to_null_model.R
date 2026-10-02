#!/usr/bin/env Rscript

library(statip)
library(vegan)
library(ggplot2)

args = commandArgs(trailingOnly=TRUE)

modelo_nulo <- read.csv("./nulo.csv" , header = FALSE)

reporte <- read.csv("./ecologia/reporte.csv" , header = FALSE)
reporte <- reporte[ 2:16 , ]

reporte_alpha <- reporte[ , c("V5" , "V7" , "V18" , "V20" ,  "V29" , "V31" , "V40" , "V42")]
reporte_beta <- reporte[ , c("V8" , "V10" , "V21" , "V23", "V32" , "V34" ,  "V43" , "V45")]

#reporte_alpha

comparacion_alpha_K <- c()
comparacion_alpha_D <- c()
comparacion_alpha_C <- c()
comparacion_alpha_B <- c()


for (i in 1:(dim(reporte)[1])){
  valor_critico <- modelo_nulo[i , 4]
#  print(valor_critico)
  if (!is.na(valor_critico) ){
    if (valor_critico < reporte_alpha[i , "V5"] && reporte_alpha[i , "V7"] < 0.05 ){
      comparacion_alpha_K <- c(comparacion_alpha_K , TRUE)
    }else{
      comparacion_alpha_K <- c(comparacion_alpha_K , FALSE)
    }
    if (valor_critico < reporte_alpha[i , "V18"] && reporte_alpha[i , "V20"] < 0.05 ){
      comparacion_alpha_D <- c(comparacion_alpha_D , TRUE)
    }else{
      comparacion_alpha_D <- c(comparacion_alpha_D , FALSE)
    }
    if (valor_critico < reporte_alpha[i , "V29"] && reporte_alpha[i , "V31"] < 0.05 ){
      comparacion_alpha_C <- c(comparacion_alpha_C , TRUE)
    }else{
      comparacion_alpha_C <- c(comparacion_alpha_C , FALSE)
    }
    if (valor_critico < reporte_alpha[i , "V40"] && reporte_alpha[i , "V42"] < 0.05 ){
      comparacion_alpha_B <- c(comparacion_alpha_B , TRUE)
    }else{
      comparacion_alpha_B <- c(comparacion_alpha_B , FALSE)
    }
  }else{
    if (reporte_alpha[i , "V7"] < 0.05 ){
      comparacion_alpha_K <- c(comparacion_alpha_K , TRUE)
    }else{
      comparacion_alpha_K <- c(comparacion_alpha_K , FALSE)
    }
    if (reporte_alpha[i , "V20"] < 0.05 ){
      comparacion_alpha_D <- c(comparacion_alpha_D , TRUE)
    }else{
      comparacion_alpha_D <- c(comparacion_alpha_D , FALSE)
    }
    if (reporte_alpha[i , "V31"] < 0.05 ){
      comparacion_alpha_C <- c(comparacion_alpha_C , TRUE)
    }else{
      comparacion_alpha_C <- c(comparacion_alpha_C , FALSE)
    }
    if (reporte_alpha[i , "V42"] < 0.05 ){
      comparacion_alpha_B <- c(comparacion_alpha_B , TRUE)
    }else{
      comparacion_alpha_B <- c(comparacion_alpha_B , FALSE)
    }
  }
}


comparacion_alpha <- data.frame( comparacion_alpha_K , comparacion_alpha_D , comparacion_alpha_C , comparacion_alpha_B )


comparacion_beta_K <- c()
comparacion_beta_D <- c()
comparacion_beta_C <- c()
comparacion_beta_B <- c()


for (i in 1:(dim(reporte)[1])){
  valor_critico <- modelo_nulo[i , 5]
  if (!is.na(valor_critico)){
    if (valor_critico > reporte_beta[i , "V8"] && reporte_beta[i , "V10"] < 0.05 ){
      comparacion_beta_K <- c(comparacion_beta_K , TRUE)
    }else{
      comparacion_beta_K <- c(comparacion_beta_K , FALSE)
    }
    if (valor_critico > reporte_beta[i , "V21"] && reporte_beta[i , "V23"] < 0.05 ){
      comparacion_beta_D <- c(comparacion_beta_D , TRUE)
    }else{
      comparacion_beta_D <- c(comparacion_beta_D , FALSE)
    }
    if (valor_critico > reporte_beta[i , "V32"] && reporte_beta[i , "V34"] < 0.05 ){
      comparacion_beta_C <- c(comparacion_beta_C , TRUE)
    }else{
      comparacion_beta_C <- c(comparacion_beta_C , FALSE)
    }
    if (valor_critico > reporte_beta[i , "V43"] && reporte_beta[i , "V45"] < 0.05 ){
      comparacion_beta_B <- c(comparacion_beta_B , TRUE)
    }else{
      comparacion_beta_B <- c(comparacion_beta_B , FALSE)
    }
  }
  else{
    if (reporte_beta[i , "V10"] < 0.05 ){
      comparacion_beta_K <- c(comparacion_beta_K , TRUE)
    }else{
      comparacion_beta_K <- c(comparacion_beta_K , FALSE)
    }
    if (reporte_beta[i , "V23"] < 0.05 ){
      comparacion_beta_D <- c(comparacion_beta_D , TRUE)
    }else{
      comparacion_beta_D <- c(comparacion_beta_D , FALSE)
    }
    if (reporte_beta[i , "V34"] < 0.05 ){
      comparacion_beta_C <- c(comparacion_beta_C , TRUE)
    }else{
      comparacion_beta_C <- c(comparacion_beta_C , FALSE)
    }
    if (reporte_beta[i , "V45"] < 0.05 ){
      comparacion_beta_B <- c(comparacion_beta_B , TRUE)
    }else{
      comparacion_beta_B <- c(comparacion_beta_B , FALSE)
    }
  }
}


comparacion_beta <- data.frame( comparacion_beta_K , comparacion_beta_D , comparacion_beta_C , comparacion_beta_B )

write.csv(comparacion_alpha , "relevancias_alpha.csv" )
write.csv(comparacion_beta , "relevancias_beta.csv")


#comparacion_alpha_K <- c()
#comparacion_alpha_D <- c()
#comparacion_alpha_C <- c()
#comparacion_alpha_B <- c()


#for (i in 1:(dim(reporte)[1])){
 # valor_critico <- modelo_nulo[i , 6]
#  print(valor_critico)
  #if (!is.na(valor_critico) ){
   # if (valor_critico < reporte_alpha[i , "V4"] && reporte_alpha[i , "V5"] < 0.05 ){
    #  comparacion_alpha_K <- c(comparacion_alpha_K , TRUE)
    #}else{
     # comparacion_alpha_K <- c(comparacion_alpha_K , FALSE)
    #}
    #if (valor_critico < reporte_alpha[i , "V15"] && reporte_alpha[i , "V16"] < 0.05 ){
    #  comparacion_alpha_D <- c(comparacion_alpha_D , TRUE)
    #}else{
    #  comparacion_alpha_D <- c(comparacion_alpha_D , FALSE)
    #}
    #if (valor_critico < reporte_alpha[i , "V24"] && reporte_alpha[i , "V25"] < 0.05 ){
     # comparacion_alpha_C <- c(comparacion_alpha_C , TRUE)
    #}else{
    #  comparacion_alpha_C <- c(comparacion_alpha_C , FALSE)
    #}
    #if (valor_critico < reporte_alpha[i , "V33"] && reporte_alpha[i , "V34"] < 0.05 ){
    #  comparacion_alpha_B <- c(comparacion_alpha_B , TRUE)
    #}else{
    #  comparacion_alpha_B <- c(comparacion_alpha_B , FALSE)
    #}
  #}else{
  #  if (reporte_alpha[i , "V5"] < 0.05 ){
   #   comparacion_alpha_K <- c(comparacion_alpha_K , TRUE)
   # }else{
    #  comparacion_alpha_K <- c(comparacion_alpha_K , FALSE)
    #}
    #if (reporte_alpha[i , "V16"] < 0.05 ){
    #  comparacion_alpha_D <- c(comparacion_alpha_D , TRUE)
    #}else{
    #  comparacion_alpha_D <- c(comparacion_alpha_D , FALSE)
    #}
    #if (reporte_alpha[i , "V25"] < 0.05 ){
    #  comparacion_alpha_C <- c(comparacion_alpha_C , TRUE)
    #}else{
    #  comparacion_alpha_C <- c(comparacion_alpha_C , FALSE)
    #}
    #if (reporte_alpha[i , "V34"] < 0.05 ){
    #  comparacion_alpha_B <- c(comparacion_alpha_B , TRUE)
    #}else{
    #  comparacion_alpha_B <- c(comparacion_alpha_B , FALSE)
    #}
  #}
#}


#comparacion_alpha <- data.frame( comparacion_alpha_K , comparacion_alpha_D , comparacion_alpha_C , comparacion_alpha_B )


#comparacion_beta_K <- c()
#comparacion_beta_D <- c()
#comparacion_beta_C <- c()
#comparacion_beta_B <- c()


#for (i in 1:(dim(reporte)[1])){
 # valor_critico <- modelo_nulo[i , 7]
  #if (!is.na(valor_critico)){
   # if (valor_critico > reporte_beta[i , "V7"] && reporte_beta[i , "V8"] < 0.05 ){
    #  comparacion_beta_K <- c(comparacion_beta_K , TRUE)
    #}else{
     # comparacion_beta_K <- c(comparacion_beta_K , FALSE)
    #}
    #if (valor_critico > reporte_beta[i , "V18"] && reporte_beta[i , "V19"] < 0.05 ){
    #  comparacion_beta_D <- c(comparacion_beta_D , TRUE)
    #}else{
    #  comparacion_beta_D <- c(comparacion_beta_D , FALSE)
    #}
    #if (valor_critico > reporte_beta[i , "V27"] && reporte_beta[i , "V28"] < 0.05 ){
    #  comparacion_beta_C <- c(comparacion_beta_C , TRUE)
    #}else{
    #  comparacion_beta_C <- c(comparacion_beta_C , FALSE)
    #}
    #if (valor_critico > reporte_beta[i , "V36"] && reporte_beta[i , "V37"] < 0.05 ){
     # comparacion_beta_B <- c(comparacion_beta_B , TRUE)
    #}else{
     # comparacion_beta_B <- c(comparacion_beta_B , FALSE)
    #}
  #}
  #else{
  #  if (reporte_beta[i , "V8"] < 0.05 ){
   #   comparacion_beta_K <- c(comparacion_beta_K , TRUE)
    #}else{
     # comparacion_beta_K <- c(comparacion_beta_K , FALSE)
    #}
    #if (reporte_beta[i , "V19"] < 0.05 ){
     # comparacion_beta_D <- c(comparacion_beta_D , TRUE)
    #}else{
     # comparacion_beta_D <- c(comparacion_beta_D , FALSE)
    #}
    #if (reporte_beta[i , "V28"] < 0.05 ){
    #  comparacion_beta_C <- c(comparacion_beta_C , TRUE)
    #}else{
    #  comparacion_beta_C <- c(comparacion_beta_C , FALSE)
    #}
    #if (reporte_beta[i , "V37"] < 0.05 ){
     # comparacion_beta_B <- c(comparacion_beta_B , TRUE)
    #}else{
     # comparacion_beta_B <- c(comparacion_beta_B , FALSE)
    #}
  #}
#}


#comparacion_beta <- data.frame( comparacion_beta_K , comparacion_beta_D , comparacion_beta_C , comparacion_beta_B )

#write.csv(comparacion_alpha , "relevancias_alpha_cv.csv" )
#write.csv(comparacion_beta , "relevancias_beta_cv.csv")



