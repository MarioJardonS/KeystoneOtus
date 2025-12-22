#!/bin/bash

#python generateNetworkParallel_no_permutation.py $1 $2

touch ../results/otus_by_centrality/${1}_${2}_centrality_measures.csv

echo degrees , closeness , betweenness >> ../results/otus_by_centrality/${1}_${2}_centrality_measures.csv

for i in {0..40}; do
  Rscript centrality_measures.R $1 $2 $i 40  >> ../results/otus_by_centrality/${1}_${2}_centrality_measures.csv &
done
wait 


