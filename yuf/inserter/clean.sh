#! /bin/sh
for i in $*; do
    echo "clean up out files for $i"
    rm /home/icfpc/shared/yuf/inserter/out/$1.tenku.out
    rm /home/icfpc/shared/yuf/inserter/tenku/$1.tenku.out
done
