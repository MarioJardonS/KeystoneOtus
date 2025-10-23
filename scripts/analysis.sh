#!/bin/bash

python generateNetwork.py $1 $2

Rscript centrality_measures.R $1 $2 

for centrality in degrees closeness betweenness ; do Rscript cv_keyotus_vs_all_high_centrality.R $1 $2 $centrality ; done

Rscript cv_keyotus_vs_all.R $1 $2 

for centrality in  degrees closeness betweenness key ; do Rscript correlation_of_diversity.R $1 $2 $centrality >> ../results/report.txt ; done
