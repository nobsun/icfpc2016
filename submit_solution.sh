#!/bin/sh
if [ $# -eq 1 ]; then
 curl --compressed -L -H Expect: -H "X-API-Key: 49-99eab0ca16efde61012b3a535bab0edb" -F "problem_id=$1" -F "solution_spec=@answers/$1.dat" "http://2016sv.icfpcontest.org/api/solution/submit" > responses/$1.json
else
 echo "$0 <qno>"
fi
