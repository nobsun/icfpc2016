#! /bin/sh

set -x

fdupes -r problems/ | ./batch-submit-simples
