#!/usr/bin/env bash
pdftoppm $1 output -png
i=${2:-0}
for f in output*png
do
	printf -v fname "%03d00.png" $((i++)) &&  mv $f $fname
done
