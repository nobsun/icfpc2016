#!/bin/sh

if [ $# -eq 2 ]; then
    if [ -r answers/${2}.dat ]; then
        ./submit_solution.sh "$@"
        sleep 3.6
    fi
else
    if [ -r answers/${1}.dat ]; then
        ./submit_solution.sh "$@"
        sleep 3.6
    fi
fi
