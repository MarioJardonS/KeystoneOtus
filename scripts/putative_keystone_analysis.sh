#!/bin/bash
echo $1 

Rscript putative_keystone.R $1 $2 

for centrality in degrees closeness betweenness ; do Rscript cv_keyotus_vs_all_high_centrality.R $1 $2 $centrality ; done

Rscript cv_keyotus_vs_all.R $1 $2 

for centrality in  degrees closeness betweenness ; do Rscript correlation_of_diversity_centrality_measures.R $1 $2 $centrality >> ../results/report.txt ; done

Rscript correlation_of_diversity.R $1 $2 >> ../results/report.txt
