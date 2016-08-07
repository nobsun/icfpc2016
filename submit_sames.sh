#!/bin/sh -x

# update clear problems
./update_clearlist.sh

# attack for same problems
fdupes -r problems/ | ./submit-same-answers
