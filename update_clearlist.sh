#!/bin/sh -x
cat responses/*.json | jq -rc '. | if .resemblance == 1.0 then . else empty end | .problem_id' | sort -n > CLEAR.md

