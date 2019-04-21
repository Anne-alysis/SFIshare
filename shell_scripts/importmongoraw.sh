#!/bin/bash

for i in /Users/asallaska/SFI/dota/data/raw/full/TI5/PlayoffsDay7/*clean; do
	mongoimport --db raw --collection TI5PlayoffsDay7 --type json --file $i -j 4 --batchSize 1
done