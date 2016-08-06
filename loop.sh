#!/bin/sh

bsleep=$1

[ x"$bsleep" != x ] || bsleep=4

./update_clearlist.sh
./download.sh $bsleep
./submit_sames.sh

