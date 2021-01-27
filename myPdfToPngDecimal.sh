#!/usr/bin/env bash
pdftoppm $1 output -png
i=${3:-1}
for f in output*png
do
	printf -v fname "%03d%02d.png" $2 $((i++)) &&  mv $f $fname
done
