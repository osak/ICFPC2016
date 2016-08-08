#!/usr/bin/env zsh

cd gideon

for t in `seq 1470445200 3600 1470603600`; do
        stack exec gideon mov 1/3,1/4 < "../slv/submission_$((t-3600)).slv" > "./tmp"
        ../../kawatea/shuffle < "./tmp" > "../slv/submission_${t}.slv"
        echo "created submission_${t}.slv"
done
