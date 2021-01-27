#!/usr/bin/env bash
#convert *png -background white -page a4 output.pdf && rm *png && myPdfToPng.sh output.pdf
convert *.png -resize 1240x1753 \
        -gravity center -extent 1240x1753 \
        -units PixelsPerInch -density 150x150 output.pdf && rm *png && myPdfToPng.sh output.pdf

