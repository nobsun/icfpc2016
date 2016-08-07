#!/bin/sh

bsleep=$1

[ x"$bsleep" != x ] || bsleep=3.6

date +"TIMESTAMP S: %Y-%m-%d %H:%M:%S"

./update_clearlist.sh
date +"TIMESTAMP 1: %Y-%m-%d %H:%M:%S"

./download.sh $bsleep
date +"TIMESTAMP 2: %Y-%m-%d %H:%M:%S"

./submit_simple.sh
date +"TIMESTAMP 3: %Y-%m-%d %H:%M:%S"

./submit_sames.sh "$2"
echo ""
date +"TIMESTAMP E: %Y-%m-%d %H:%M:%S"
