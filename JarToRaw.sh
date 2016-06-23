#!/bin/bash

j=".results"
for i in /Volumes/BigRed/dota/Replay/MajorDemos/*dem; do
		java -jar full-dota2-exp-1.1.jar $i > $i$j
done