#!/bin/sh

path="$(dirname "$0")"
cd "$path"

[ -d resources ] || exit 1

rm resources/*.pdf
rm resources/*.png

../tools/graphRProj/generateMaps.sh ../R resources png
../tools/graphRProj/generateMaps.sh ../R resources pdf

convert resources/fullmap.png -font Helvetica -pointsize 600 -draw "gravity center fill rgba(0,0,0,.2) text 0,400 'Don\\'t Panic'" -define png:compression-level=9 resources/fullmap.png

