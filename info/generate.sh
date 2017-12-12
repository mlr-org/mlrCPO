#!/bin/sh

path="$(dirname "$0")"
cd "$path"

[ -d resources ] || exit 1

rm resources/*.pdf
rm resources/*.png

../tools/graphRProj/generateMaps.sh ../R resources png
../tools/graphRProj/generateMaps.sh ../R resources pdf
