#!/usr/bin/env Rscript
library(ggplot2)

#args = commandArgs(trailingOnly=TRUE)

plantas <- c("Arachis_hypogaea" , "Capsicum_annuum" , "Glycine_max" , "Lactuca_sativa_L" , "Malus_domestica" , "Oryza_sativa", "Panax_ginseng" ,"Panax_notoginseng" , "Persea_americana_Mill" , "Sorghum_bicolor" , "Triticum_aestivum" , "Zea_mays_L._hybrid")
rizosferas <- c("chile" , "maiz" , "tomate")
plantas <- c(plantas , c("Capsicum" , "Zea_mays" , "Solanum_lycopersicum"))


centralities <- list()

for (i  in 1:12){
  data_planta <- paste0( "../results/otus_by_centrality/" , plantas[i] , "_Rhizosphere_centrality_measures.csv"  )
  data_planta <- read.csv(data_planta , header = TRUE , row.names = 1)
  centralities[[i]] <- data_planta
  
}

for (i in 1:3){
  data_planta <- paste0("../results/otus_by_centrality/table.from_" , rizosferas[i] , "_bydegree.csv")
  data_planta <- read.csv(data_planta , header = TRUE , row.names = 1)
  centralities[[12+i]] <- data_planta
}

for (i in 1:15){
  x_i = c()
  for (j in 1:dim(centralities[[i]])[1]){
    x_i <- c(x_i , plantas[i])
  }
  centralities[[i]]$clase <- x_i
}






primer_decil_betweenness <- c()
primer_tercil_betweenness <- c()
primer_cuartil_betweenness <- c()
media_betweenness <- c()

for (i in 1:15){
  primer_decil_betweenness <- c(primer_decil_betweenness, quantile(centralities[[i]]$betweenness , probs = seq(0 , 1 , 0.1))[2] )
  primer_tercil_betweenness <- c(primer_tercil_betweenness , quantile(centralities[[i]]$betweenness , probs = seq(0 , 1 , 0.33))[2])
  primer_cuartil_betweenness <- c(primer_cuartil_betweenness , quantile(centralities[[i]]$betweenness , probs = seq(0 , 1 , 0.25)[2]))
  media_betweenness <- c(media_betweenness , log(mean(centralities[[i]]$betweenness) , 10) )
}

data_betweenness <- data.frame(Plant = plantas , Decile = primer_decil_betweenness , Tertile = primer_tercil_betweenness , Quartile = primer_cuartil_betweenness , Mean = media_betweenness)

plot_betweenness <- ggplot() +
  geom_boxplot( data = centralities[[1]], aes( x=centralities[[1]]$clase , y=log(centralities[[1]]$betweenness ,10) ) ) +
  geom_boxplot( data = centralities[[2]], aes(x=centralities[[2]]$clase , y=log(centralities[[2]]$betweenness ,10) ) ) +
  geom_boxplot( data = centralities[[3]], aes( x=centralities[[3]]$clase , y=log(centralities[[3]]$betweenness ,10) ) ) +
  geom_boxplot( data = centralities[[4]], aes(x=centralities[[4]]$clase , y=log(centralities[[4]]$betweenness ,10)) ) +
  geom_boxplot( data = centralities[[5]], aes( x=centralities[[5]]$clase , y=log(centralities[[5]]$betweenness ,10) ) ) +
  geom_boxplot( data = centralities[[6]], aes( x=centralities[[6]]$clase , y=log(centralities[[6]]$betweenness ,10) ) ) +
  geom_boxplot( data = centralities[[7]], aes( x=centralities[[7]]$clase , y=log(centralities[[7]]$betweenness ,10) ) ) +
  geom_boxplot( data = centralities[[8]], aes( x=centralities[[8]]$clase , y=log(centralities[[8]]$betweenness ,10) ) ) +
  geom_boxplot( data = centralities[[9]], aes( x=centralities[[9]]$clase , y=log(centralities[[9]]$betweenness ,10) ) ) +
  geom_boxplot( data = centralities[[10]], aes( x=centralities[[10]]$clase , y=log(centralities[[10]]$betweenness ,10) ) ) +
  geom_boxplot( data = centralities[[11]], aes( x=centralities[[11]]$clase , y=log(centralities[[11]]$betweenness ,10) ) ) +
  geom_boxplot( data = centralities[[12]], aes( x=centralities[[12]]$clase , y=log(centralities[[12]]$betweenness ,10) ) ) +
  geom_boxplot( data = centralities[[13]], aes( x=centralities[[13]]$clase , y=log(centralities[[13]]$betweenness ,10) )) +
  geom_boxplot( data = centralities[[14]], aes( x=centralities[[14]]$clase , y=log(centralities[[14]]$betweenness ,10) ) ) +
  geom_boxplot( data = centralities[[15]], aes( x=centralities[[15]]$clase , y=log(centralities[[15]]$betweenness ,10) ) ) +
  geom_line(data = data_betweenness , aes(x = Plant , y = log(Decile , 10) , group = 1 , color = 'blue')) +
  geom_line(data = data_betweenness , aes(x = Plant , y = log(Tertile ,10) , group = 1 , color = 'red')) +
  geom_line(data = data_betweenness , aes(x = Plant , y = Mean , group = 1 , color = 'green')) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1) )

plot_betweenness

ggsave("distribution_betweenness_boxplot.png" , plot_betweenness , device = "png")

primer_decil_closeness <- c()
primer_tercil_closeness <- c()
ultimo_cuartil_closeness <- c()
media_closeness <- c()

for (i in 1:15){
  primer_decil_closeness <- c(primer_decil_closeness, quantile(centralities[[i]]$closeness , probs = seq(0 , 1 , 0.1))[10] )
  primer_tercil_closeness <- c(primer_tercil_closeness , quantile(centralities[[i]]$closeness , probs = seq(0 , 1 , 0.33))[3])
  ultimo_cuartil_closeness <- c(ultimo_cuartil_closeness , quantile(centralities[[i]]$closeness , probs = seq(0 , 1 , 0.25))[4])
  media_closeness <- c(media_closeness , mean(centralities[[i]]$closeness))
}

data_closeness <- data.frame(Plant = plantas , Decile = primer_decil_closeness , Tertile = primer_tercil_closeness , Quartile = ultimo_cuartil_closeness ,  Mean = media_closeness)

plot_closeness <- ggplot() +
  geom_boxplot( data = centralities[[1]], aes( x=centralities[[1]]$clase , y=centralities[[1]]$closeness ) ) +
  geom_boxplot( data = centralities[[2]], aes(x=centralities[[2]]$clase , y=centralities[[2]]$closeness ) ) +
  geom_boxplot( data = centralities[[3]], aes( x=centralities[[3]]$clase , y=centralities[[3]]$closeness ) ) +
  geom_boxplot( data = centralities[[4]], aes(x=centralities[[4]]$clase , y=centralities[[4]]$closeness) ) +
  geom_boxplot( data = centralities[[5]], aes( x=centralities[[5]]$clase , y=centralities[[5]]$closeness ) ) +
  geom_boxplot( data = centralities[[6]], aes( x=centralities[[6]]$clase , y=centralities[[6]]$closeness ) ) +
  geom_boxplot( data = centralities[[7]], aes( x=centralities[[7]]$clase , y=centralities[[7]]$closeness ) ) +
  geom_boxplot( data = centralities[[8]], aes( x=centralities[[8]]$clase , y=centralities[[8]]$closeness ) ) +
  geom_boxplot( data = centralities[[9]], aes( x=centralities[[9]]$clase , y=centralities[[9]]$closeness ) ) +
  geom_boxplot( data = centralities[[10]], aes( x=centralities[[10]]$clase , y=centralities[[10]]$closeness ) ) +
  geom_boxplot( data = centralities[[11]], aes( x=centralities[[11]]$clase , y=centralities[[11]]$closeness ) ) +
  geom_boxplot( data = centralities[[12]], aes( x=centralities[[12]]$clase , y=centralities[[12]]$closeness ) ) +
  geom_boxplot( data = centralities[[13]], aes( x=centralities[[13]]$clase , y=centralities[[13]]$closeness )) +
  geom_boxplot( data = centralities[[14]], aes( x=centralities[[14]]$clase , y=centralities[[14]]$closeness ) ) +
  geom_boxplot( data = centralities[[15]], aes( x=centralities[[15]]$clase , y=centralities[[15]]$closeness ) ) +
  geom_line(data = data_closeness , aes(x = Plant , y = Decile , group = 1 , color = 'blue')) +
  geom_line(data = data_closeness , aes(x = Plant , y = Tertile , group = 1 , color = 'red')) +
  geom_line(data = data_closeness , aes(x = Plant , y = Mean , group = 1 , color = 'green')) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1) )

plot_closeness

ggsave("distribution_closeness_boxplot.png" , plot_closeness , device = "png")

primer_decil_degrees <- c()
primer_tercil_degrees <- c()
ultimo_cuartil_degrees <- c()
media_degrees <- c()

for (i in 1:15){
  primer_decil_degrees <- c(primer_decil_degrees, quantile(centralities[[i]]$degrees , probs = seq(0 , 1 , 0.1))[10] )
  primer_tercil_degrees <- c(primer_tercil_degrees , quantile(centralities[[i]]$degrees , probs = seq(0 , 1 , 0.33))[3])
  ultimo_cuartil_degrees <- c(ultimo_cuartil_degrees , quantile(centralities[[i]]$degrees , probs = seq(0 , 1 , 0.25))[4])
  media_degrees <- c(media_degrees , mean(centralities[[i]]$degrees))
}

data_degrees <- data.frame(Plant = plantas , Decile = primer_decil_degrees , Tertile = primer_tercil_degrees , Quartile = ultimo_cuartil_degrees,  Mean = media_degrees)

plot_degrees <- ggplot() +
  geom_boxplot( data = centralities[[1]], aes( x=centralities[[1]]$clase , y=centralities[[1]]$degrees ) ) +
  geom_boxplot( data = centralities[[2]], aes(x=centralities[[2]]$clase , y=centralities[[2]]$degrees ) ) +
  geom_boxplot( data = centralities[[3]], aes( x=centralities[[3]]$clase , y=centralities[[3]]$degrees ) ) +
  geom_boxplot( data = centralities[[4]], aes(x=centralities[[4]]$clase , y=centralities[[4]]$degrees) ) +
  geom_boxplot( data = centralities[[5]], aes( x=centralities[[5]]$clase , y=centralities[[5]]$degrees ) ) +
  geom_boxplot( data = centralities[[6]], aes( x=centralities[[6]]$clase , y=centralities[[6]]$degrees ) ) +
  geom_boxplot( data = centralities[[7]], aes( x=centralities[[7]]$clase , y=centralities[[7]]$degrees ) ) +
  geom_boxplot( data = centralities[[8]], aes( x=centralities[[8]]$clase , y=centralities[[8]]$degrees ) ) +
  geom_boxplot( data = centralities[[9]], aes( x=centralities[[9]]$clase , y=centralities[[9]]$degrees ) ) +
  geom_boxplot( data = centralities[[10]], aes( x=centralities[[10]]$clase , y=centralities[[10]]$degrees ) ) +
  geom_boxplot( data = centralities[[11]], aes( x=centralities[[11]]$clase , y=centralities[[11]]$degrees ) ) +
  geom_boxplot( data = centralities[[12]], aes( x=centralities[[12]]$clase , y=centralities[[12]]$degrees ) ) +
  geom_boxplot( data = centralities[[13]], aes( x=centralities[[13]]$clase , y=centralities[[13]]$degrees )) +
  geom_boxplot( data = centralities[[14]], aes( x=centralities[[14]]$clase , y=centralities[[14]]$degrees ) ) +
  geom_boxplot( data = centralities[[15]], aes( x=centralities[[15]]$clase , y=centralities[[15]]$degrees ) ) +
  geom_line(data = data_degrees , aes(x = Plant , y = Decile , group = 1 , color = 'blue')) +
  geom_line(data = data_degrees , aes(x = Plant , y = Tertile , group = 1 , color = 'red')) +
  geom_line(data = data_degrees , aes(x = Plant , y = Mean , group = 1 , color = 'green')) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1) )

plot_degrees

ggsave("distribution_degree_boxplot.png" , plot_degrees , device = "png")






#for ( i in 1:15){
  #print(plantas[i])
  #print(mean(centralities[[i]]$betweenness/quantile(centralities[[i]]$betweenness , probs = seq(0, 1, 0.33))[2] ))
  #umbral <- quantile(centralities[[i]]$betweenness , probs = seq(0, 1, 0.33))[2]
  #umbral <- which(centralities[[i]]$betweenness < umbral)
  #planta_i <- centralities[[i]][ umbral, ]
  #print(quantile(centralities[[i]]$degrees , probs = seq(0, 1, 0.33))[3]/mean(planta_i$degrees))
  #print(quantile(centralities[[i]]$closeness , probs = seq(0, 1, 0.33))[3]/mean(planta_i$closeness))
  
#}



plot_degrees <- ggplot() + # fill=name allow to automatically dedicate a color for each group
  geom_violin( data = centralities[[1]], aes( x=centralities[[1]]$clase , y=centralities[[1]]$degrees )) +
  geom_violin( data = centralities[[2]], aes(x=centralities[[2]]$clase , y=centralities[[2]]$degrees )) +
  geom_violin( data = centralities[[3]], aes( x=centralities[[3]]$clase , y=centralities[[3]]$degrees )) +
  geom_violin( data = centralities[[4]], aes(x=centralities[[4]]$clase , y=centralities[[4]]$degrees)) +
  geom_violin( data = centralities[[5]], aes( x=centralities[[5]]$clase , y=centralities[[5]]$degrees )) +
  geom_violin( data = centralities[[6]], aes( x=centralities[[6]]$clase , y=centralities[[6]]$degrees )) +
  geom_violin( data = centralities[[7]], aes( x=centralities[[7]]$clase , y=centralities[[7]]$degrees )) +
  geom_violin( data = centralities[[8]], aes( x=centralities[[8]]$clase , y=centralities[[8]]$degrees )) +
  geom_violin( data = centralities[[9]], aes( x=centralities[[9]]$clase , y=centralities[[9]]$degrees )) +
  geom_violin( data = centralities[[10]], aes( x=centralities[[10]]$clase , y=centralities[[10]]$degrees )) +
  geom_violin( data = centralities[[11]], aes( x=centralities[[11]]$clase , y=centralities[[11]]$degrees )) +
  geom_violin( data = centralities[[12]], aes( x=centralities[[12]]$clase , y=centralities[[12]]$degrees )) +
  geom_violin( data = centralities[[13]], aes( x=centralities[[13]]$clase , y=centralities[[13]]$degrees )) +
  geom_violin( data = centralities[[14]], aes( x=centralities[[14]]$clase , y=centralities[[14]]$degrees )) +
  geom_violin( data = centralities[[15]], aes( x=centralities[[15]]$clase , y=centralities[[15]]$degrees )) +
  geom_line(data = data_degrees , aes(x = Plant , y = Decile , group = 1 , color = 'blue')) +
  geom_line(data = data_degrees , aes(x = Plant , y = Tertile , group = 1 , color = 'red')) +
  geom_line(data = data_degrees , aes(x = Plant , y = Quartile , group = 1 , color = 'purple')) +
  geom_line(data = data_degrees , aes(x = Plant , y = Mean , group = 1 , color = 'green')) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1) )

plot_degrees



plot_closeness <- ggplot() + # fill=name allow to automatically dedicate a color for each group
  geom_violin( data = centralities[[1]], aes( x=centralities[[1]]$clase , y=centralities[[1]]$closeness )) +
  geom_violin( data = centralities[[2]], aes(x=centralities[[2]]$clase , y=centralities[[2]]$closeness )) +
  geom_violin( data = centralities[[3]], aes( x=centralities[[3]]$clase , y=centralities[[3]]$closeness )) +
  geom_violin( data = centralities[[4]], aes(x=centralities[[4]]$clase , y=centralities[[4]]$closeness)) +
  geom_violin( data = centralities[[5]], aes( x=centralities[[5]]$clase , y=centralities[[5]]$closeness )) +
  geom_violin( data = centralities[[6]], aes( x=centralities[[6]]$clase , y=centralities[[6]]$closeness )) +
  geom_violin( data = centralities[[7]], aes( x=centralities[[7]]$clase , y=centralities[[7]]$closeness )) +
  geom_violin( data = centralities[[8]], aes( x=centralities[[8]]$clase , y=centralities[[8]]$closeness )) +
  geom_violin( data = centralities[[9]], aes( x=centralities[[9]]$clase , y=centralities[[9]]$closeness )) +
  geom_violin( data = centralities[[10]], aes( x=centralities[[10]]$clase , y=centralities[[10]]$closeness )) +
  geom_violin( data = centralities[[11]], aes( x=centralities[[11]]$clase , y=centralities[[11]]$closeness )) +
  geom_violin( data = centralities[[12]], aes( x=centralities[[12]]$clase , y=centralities[[12]]$closeness )) +
  geom_violin( data = centralities[[13]], aes( x=centralities[[13]]$clase , y=centralities[[13]]$closeness )) +
  geom_violin( data = centralities[[14]], aes( x=centralities[[14]]$clase , y=centralities[[14]]$closeness )) +
  geom_violin( data = centralities[[15]], aes( x=centralities[[15]]$clase , y=centralities[[15]]$closeness )) +
  geom_line(data = data_closeness , aes(x = Plant , y = Decile , group = 1 , color = 'blue')) +
  geom_line(data = data_closeness , aes(x = Plant , y = Tertile , group = 1 , color = 'red')) +
  geom_line(data = data_closeness , aes(x = Plant , y = Quartile , group = 1 , color = 'purple')) +
  geom_line(data = data_closeness , aes(x = Plant , y = Mean , group = 1 , color = 'green')) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1) )

plot_closeness

plot_betweenness <- ggplot() + # fill=name allow to automatically dedicate a color for each group
  geom_violin( data = centralities[[1]], aes( x=centralities[[1]]$clase , y= log(centralities[[1]]$betweenness , 10 ) )) +
  geom_violin( data = centralities[[2]], aes(x=centralities[[2]]$clase , y=log(centralities[[2]]$betweenness , 10 ) )) +
  geom_violin( data = centralities[[3]], aes( x=centralities[[3]]$clase , y=log(centralities[[3]]$betweenness , 10 ))) +
  geom_violin( data = centralities[[4]], aes(x=centralities[[4]]$clase , y=log(centralities[[4]]$betweenness , 10 ))) +
  geom_violin( data = centralities[[5]], aes( x=centralities[[5]]$clase , y=log(centralities[[5]]$betweenness , 10 )) ) +
  geom_violin( data = centralities[[6]], aes( x=centralities[[6]]$clase , y=log(centralities[[6]]$betweenness , 10 ) )) +
  geom_violin( data = centralities[[7]], aes( x=centralities[[7]]$clase , y=log(centralities[[7]]$betweenness , 10 ))) +
  geom_violin( data = centralities[[8]], aes( x=centralities[[8]]$clase , y=log(centralities[[8]]$betweenness , 10 ) )) +
  geom_violin( data = centralities[[9]], aes( x=centralities[[9]]$clase , y=log(centralities[[9]]$betweenness , 10 ) )) +
  geom_violin( data = centralities[[10]], aes( x=centralities[[10]]$clase , y=log(centralities[[10]]$betweenness , 10 ))) +
  geom_violin( data = centralities[[11]], aes( x=centralities[[11]]$clase , y=log(centralities[[11]]$betweenness , 10 ) )) +
  geom_violin( data = centralities[[12]], aes( x=centralities[[12]]$clase , y=log(centralities[[12]]$betweenness , 10 ))) +
  geom_violin( data = centralities[[13]], aes( x=centralities[[13]]$clase , y=log(centralities[[13]]$betweenness , 10 ) )) +
  geom_violin( data = centralities[[14]], aes( x=centralities[[14]]$clase , y=log(centralities[[14]]$betweenness , 10 ) )) +
  geom_violin( data = centralities[[15]], aes( x=centralities[[15]]$clase , y=log(centralities[[15]]$betweenness , 10 ) )) +
  geom_line(data = data_betweenness , aes(x = Plant , y = log(Decile , 10) , group = 1 , color = 'blue')) +
  geom_line(data = data_betweenness , aes(x = Plant , y = log(Tertile , 10) , group = 1 , color = 'red')) +
  geom_line(data = data_betweenness , aes(x = Plant , y = log(Quartile , 10 ) , group = 1 , color = 'purple')) +
  geom_line(data = data_betweenness , aes(x = Plant , y = Mean , group = 1 , color = 'green')) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1) )




plot_betweenness


ggsave("distribution_of_degrees.png" , plot_degrees , device = "png")
ggsave("distribution_of_closeness.png" , plot_closeness , device = "png")
ggsave("distribution_of_betweenness.png" , plot_betweenness , device = "png")
#degree <- list()

#for (i in 1:length(plantas)){
  #degree_i <- c()
  #for (otu in otus){
 #   if (otu in row.names(centralities[[i]])){
 #     degree_i <- c(degree_i , centralities[[i]])
 #   }
      
#},
#}



