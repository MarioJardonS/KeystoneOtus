#!/bin/bash

python generateNetwork.py $1 $2

touch ../results/otus_by_centrality/${1}_${2}_centrality_measures.csv

echo degrees , closeness , betweenness >> ../results/otus_by_centrality/${1}_${2}_centrality_measures.csv

for i in {0..30}; do Rscript centrality_measures.R $1 $2 $i >> ../results/otus_by_centrality/${1}_${2}_centrality_measures.csv &  done

Rscript putative_keystone.R $1 $2 

for centrality in degrees closeness betweenness ; do Rscript cv_keyotus_vs_all_high_centrality.R $1 $2 $centrality ; done

Rscript cv_keyotus_vs_all.R $1 $2 

for centrality in  degrees closeness betweenness key ; do Rscript correlation_of_diversity.R $1 $2 $centrality >> ../results/report.txt ; done
