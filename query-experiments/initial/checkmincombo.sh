#!/bin/bash

FILE=shift
if [ -z "$FILE" ]; then
	FILE="combinations-3.log"
fi

cat $FILE | awk 'BEGIN { MIN = "inf"; COMBO=""; c="" } match($0, /Selected combination: (.+)/, n) { c = n[1] } match($0, /loss value ([0-9.]+)/, m) { if ( m[1] < MIN ) { MIN = m[1]; COMBO = c; } } END { print COMBO, MIN }'
