#!/bin/bash
FOLDER="$1"
OUTPUT="$2"
ITERATIONS="$3"

echo "** Start experiment ($ITERATIONS iterations) **"
echo "Input folder: $FOLDER"
echo "Output file: $OUTPUT (will be REMOVED)"
read -p "Are you sure? (y/N) " -n 1
echo
case $REPLY in
    y|Y)
	rm "$OUTPUT"
	for f in $FOLDER/*.txt; do  
	    for ((i=1; i<=$ITERATIONS; i++)); do
		echo "** Iteration $i ** "
		echo -n "Iteration $i," >> "$OUTPUT";
		./rr -i "$f" -o csv "$@" >> "$OUTPUT";
	    done
	done;;
	*)
	exit 1;;
esac

