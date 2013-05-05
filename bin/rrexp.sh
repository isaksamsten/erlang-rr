#!/bin/bash
FOLDER="$1"
OUTPUT="$2"
ITERATIONS="$3"
EX="$4"

echo "** Start experiment ($ITERATIONS iterations) **"
echo "Input folder: $FOLDER"
echo "Experiment: $EX"
echo "Output file: $OUTPUT (will be REMOVED)"
read -p "Are you sure? (y/N) " -n 1
echo
case $REPLY in
    y|Y)
	rm "$OUTPUT"
	for f in $FOLDER/*.txt; do  
	    for ((i=1; i<=$ITERATIONS; i++)); do
		echo "info: running $f iteration $i/$ITERATIONS"
		TMP=`./rr -i "$f" -o csv "$@"`
		while read -r line; do
		    echo "Iteration $i, $EX, $line" >> "$OUTPUT"
		done  <<< "$TMP";
	    done
	done;;
	*)
	exit 1;;
esac
