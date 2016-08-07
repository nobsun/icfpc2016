#!/bin/sh -x

# update clear problems
./update_clearlist.sh

sleep=$1
[ x"$sleep" != x ] || sleep=3600

# attack for same problems
fdupes -r problems/ | ./submit-same-answers $sleep
