#!/usr/bin/env zsh

for t in `seq 1470441600 3600 1470603600`; do
    curl --compressed -L -H Expect: -H 'X-API-Key: 58-9827ac551dda5c98cc7b3bd7e9de92ec' -F 'solution_spec=@../mkut/slv/32-F-mid3.slv' -F "publish_time=${t}" 'http://2016sv.icfpcontest.org/api/problem/submit'
    sleep 1
done
