#!/bin/sh
if [ $# -eq 2 ]; then
  cp answers/$2.dat answers/$1.dat;
fi

if [ $# -ge 1 ]; then
    curl --compressed -L -H Expect: -H "X-API-Key: 49-99eab0ca16efde61012b3a535bab0edb" -F "problem_id=$1" -F "solution_spec=@answers/$1.dat" "http://2016sv.icfpcontest.org/api/solution/submit" > responses/$1.json
    if [ ! -s responses/$1.json ]; then
        echo "Removing empty result"
        rm responses/$1.json
    else
        cat responses/$1.json
    fi
else
    echo "$0 <qno>"
    echo " OR "
    echo "$0 <qno> <org-qno>"
fi
