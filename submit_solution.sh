#!/bin/sh

if [ $# -eq 2 ]; then
  cp answers/$2.dat answers/$1.dat;
fi

num=$1

submit() {
    count=$1

    new=responses/$num.json.new
    curl --compressed -L -H Expect: -H "X-API-Key: 49-99eab0ca16efde61012b3a535bab0edb" -F "problem_id=$num" -F "solution_spec=@answers/$num.dat" "http://2016sv.icfpcontest.org/api/solution/submit" > $new

    if [ ! -s $new ]; then
        echo "Removing empty result"
        rm $new
    fi

    if egrep -q 'Rate limit exceede' $new ; then
        echo "Removing rate limit error result: problem_id=$num"
        rm $new
        if [ x$(expr $count '<' 3) = x1 ]; then
            submit $(expr $count + 1)
        fi
    fi
}

if [ $# -ge 1 ]; then
    submit 0

    if [ -s responses/$1.json.new ]; then
        mv responses/$1.json.new responses/$1.json
        cat responses/$1.json
    fi
else
    echo "$0 <qno>"
    echo " OR "
    echo "$0 <qno> <org-qno>"
fi
