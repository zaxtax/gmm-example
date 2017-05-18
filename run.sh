#!/bin/bash
dataSize=250

for sweeps in $(seq 1 5); do
    for i in $(seq 1 20); do
        Rscript gmm.R $dataSize $sweeps $i;
        ./GMMRunner $dataSize $sweeps $i;
    done
done
