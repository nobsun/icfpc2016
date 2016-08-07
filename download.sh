#!/bin/sh

bsleep=$1

[ x"$bsleep" != x ] || bsleep=4

[ x"$(expr $bsleep '>=' 1)" = x1 ] || bsleep=1

set -x

# CLEAN
rm -f snapshots.json bloblookup.json

# GET Snapshot
curl --compressed -L -H Expect: -H 'X-API-Key: 49-99eab0ca16efde61012b3a535bab0edb' 'http://2016sv.icfpcontest.org/api/snapshot/list' > snapshots.json.new

if egrep -q '"ok":true' snapshots.json.new; then
    mv snapshots.json.new snapshots.json
fi

sleep 1.5

# GET Blob Lookup
cat snapshots.json |jq -r '.snapshots | sort_by(.snapshot_time) | .[-1].snapshot_hash' | sed "s@\(.*\)@curl --compressed -L -H Expect: -H \'X-API-Key: 49-99eab0ca16efde61012b3a535bab0edb\' \'http://2016sv.icfpcontest.org/api/blob/\1\' \> bloblookup.json.new@" | sh

if egrep -q resemblance bloblookup.json.new; then
    mv bloblookup.json.new bloblookup.json
fi

sleep $bsleep

# GEN Script
cat bloblookup.json |jq -rc '.problems[] | [.problem_id, .problem_spec_hash] | @csv' | sed 's@\(.*\),\"\(.*\)\"@if [ -r problems/\1.dat ]; then echo EXISTS SKIP \1 ; else echo NOW DOWNLOADING ... \1 ; curl --compressed -L -H Expect: -H \"X-API-Key: 49-99eab0ca16efde61012b3a535bab0edb\" \"http://2016sv.icfpcontest.org/api/blob/\2\" \> problems/\1.dat; sleep '$bsleep'; fi@' > do.sh

# Download
sh do.sh
