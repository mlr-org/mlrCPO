#!/bin/sh

if [ -z "$1" ] ; then
  echo "Usage: $0 outpath [projpath] [ignorelist] [includelidst]"
  exit 1
fi
outpath="$1"
inpath=.
if [ ! -z "$2" ] ; then
  inpath="$2"
fi

tf1=$(mktemp /tmp/grp1.XXXXXXXXXX)

tf2=$(mktemp /tmp/grp1.XXXXXXXXXX)

grep -o "^[0-9a-zA-Z._]* = function" "$inpath"/*.R | sed 's/ = function//' | sed 's|^[^:]*/||' | sed 's/:/ /' > "$tf1"

if [ ! -z "$3" ] ; then
  mv "$tf1" "$tf2"
  grep -vf "$3" "$tf2" > "$tf1"
  rm "$tf2"
fi
if [ ! -z "$4" ] ; then
  mv "$tf1" "$tf2"
  grep -f "$4" "$tf2" > "$tf1"
  rm "$tf2"
fi

cat "$tf1" | sed 's/.* //' > "$tf2"


echo "digraph callgraph {"
# echo '  rankdir="LR";'
# echo "xx composeProperties" | while read file function ; do
cat "$tf1" | while read file function ; do
  fescaped=$(echo "$function" | sed 's/\([.\\/]\)/\\\1/g')
  fbase=$(echo "$function" | sed 's/\(...\)\..*/\1/')
  cat "$inpath"/*.R | sed -n "/^$fescaped = function/,/^[0-9a-zA-Z._]* = function/ p" | head -n -1 | sed 's/#.*//' | grep -oP '[a-zA-Z0-9._]*(?=\()' | \
      sort | uniq | sed 's/\([\]\[.\\]\)/\\\1/g' | sed 's/^\(.*\)$/^\1$/' | grep -f - "$tf2" | \
      sed 's/\(...\)\..*/\1/' | sort | uniq | \
      sed "s/^\\(.*\\)\$/  \"${fbase}\" -> \"\\1\";/"
  if [ "$fbase" = "$function" ] ; then
    if tac "$inpath"/*.R | sed -n "/^$fescaped = function/,/^[0-9a-zA-Z._]* = function/ p" | grep "#' @export" > /dev/null ; then
      shape="; penwidth = 4"
    else
      shape=
    fi
    grep -m 1 " ${fbase}\$" "$tf1" | sed 's/ .*//' | sed "s/\\(.*\\)/\"${fbase}\" [label=\"${fbase}\\\\n(\\1)\"${shape}]/"
  fi
done | grep -v '"\([^"]*\)" -> "\1"' | sort | uniq
  
echo "}"
rm -f "$tf1"
rm -f "$tf2"
