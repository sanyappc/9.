#!/bin/sh
# Golden tests: each tests/*.9 is loaded into the REPL with "l",
# the output must match tests/*.out.
# usage: tests/run.sh path/to/ninedot [--update]
BIN=${1:?usage: $0 path/to/ninedot [--update]}
DIR=$(dirname "$0")
fail=0
for t in "$DIR"/*.9; do
	expected="${t%.9}.out"
	actual=$(printf 'l %s\nq\n' "$t" | "$BIN")
	if [ "$2" = "--update" ]; then
		printf '%s\n' "$actual" > "$expected"
	elif [ "$actual" = "$(cat "$expected")" ]; then
		echo "ok   $t"
	else
		echo "FAIL $t"
		printf '%s\n' "$actual" | diff "$expected" - 
		fail=1
	fi
done
exit $fail
