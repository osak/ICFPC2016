#!/bin/sh
for i in $*; do
  echo $i
  in="${i}.in"
  out="tenku/${i}.tenku.out"
  ./a.out < $in > $out
  ruby ../inserter/inserter.rb
  ruby ../inserter/reverseinserter.rb
done
