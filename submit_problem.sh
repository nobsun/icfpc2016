#!/bin/sh

if [ $# -ge 1 ]; then
    curl --compressed -L -H Expect: -H 'X-API-Key: 49-99eab0ca16efde61012b3a535bab0edb' -F "solution_spec=@publish-problem/submissions/${1}.dat" -F "publish_time=$1" 'http://2016sv.icfpcontest.org/api/problem/submit' > publish-problem/responses/${1}.json
else
    echo "$0 publish_time"
fi
