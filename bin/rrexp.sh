#!/bin/bash
FOLDER="$1"
OUTPUT="$2"
ITERATIONS="$3"
EX="$4"
if [ -f "$OUTPUT" ]; then
    echo "'$OUTPUT' exists. Appending..."
fi

#rm "$OUTPUT"
for f in $FOLDER/*.txt; do  
    for ((i=1; i<=$ITERATIONS; i++)); do
	echo "info: running $f iteration $i/$ITERATIONS"
	TMP=`./rr rf -i "$f" -o csv "$@"`
	while read -r line; do
	    echo "Iteration $i, $EX, $f, $line" >> "$OUTPUT"
	done  <<< "$TMP";
    done
done
