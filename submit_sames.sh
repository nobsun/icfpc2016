#!/bin/sh -x

# update clear problems
./update_clearlist.sh

# attack for same problems
fdupes -r problems/ | sed 's@problems/\(.*\).dat@\1@' | awk '{if (key == "") {key = $0;} else {lns = lns OFS $0;}};/^$/{print "for i in " lns ";do ./submit_solution.sh $i " key "; sleep 4; done;"; key = ""; lns = ""};' | sh
