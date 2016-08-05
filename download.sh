#!/bin/sh -x
# CLEAN
rm snapshots.json bloblookup.json

# GET Snapshot
curl --compressed -L -H Expect: -H 'X-API-Key: 49-99eab0ca16efde61012b3a535bab0edb' 'http://2016sv.icfpcontest.org/api/snapshot/list' > snapshots.json

sleep 5

# GET Blob Lookup
cat snapshots.json |jq -r '.snapshots | sort_by(.snapshot_time) | .[-1].snapshot_hash' | sed "s@\(.*\)@curl --compressed -L -H Expect: -H \'X-API-Key: 49-99eab0ca16efde61012b3a535bab0edb\' \'http://2016sv.icfpcontest.org/api/blob/\1\' \> bloblookup.json@" | sh

sleep 5

# GEN Script
cat bloblookup.json |jq -rc '.problems[] | [.problem_id, .problem_spec_hash] | @csv' | sed 's@\(.*\),\"\(.*\)\"@echo NOW DOWNLOADING ... \1; curl --compressed -L -H Expect: -H \"X-API-Key: 49-99eab0ca16efde61012b3a535bab0edb\" \"http://2016sv.icfpcontest.org/api/blob/\2\" \> problems/\1.dat; sleep 4@' > do.sh

# Download
sh do.sh

