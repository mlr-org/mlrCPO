#!/bin/sh
path="$(dirname "$0")"

grp="${path}/grp.sh"

if [ -z "$3" ] ; then
  echo usage: "$0" "inpath" "outpath" "outtype"
  echo "outtype should be understood by graphviz (png, pdf, ...)"
  echo "outtype may also be 'small.png' for reduced size graphs"
  exit 1
fi

inpath="$1"
outpath="$2"
suffix="$3"

graphopts="-Nfontname=helvetica -Nmargin=0.05 -Nfontsize=18 -Nshape=box -Gsize=16,30\!"
rankdir=LR
if [ "$suffix" = "small.png" ] ; then
  graphopts="$graphopts -Gdpi=50"
  xsuffix=png
else
  graphopts="$graphopts -Gdpi=100"
  xsuffix="${suffix}"
fi


"$grp" "$outpath" "$inpath" "${path}/ignoreNoMakeCPO" "${path}/includeMakeCPO" | gvpr -i 'N[$.outdegree!=0 || $.indegree!=0]' | dot ${graphopts} -Grankdir="${rankdir}" -Granksep=0.4 "-T${xsuffix}" > "${outpath}/makeCPO.${suffix}"
"$grp" "$outpath" "$inpath" "${path}/ignorelist" "${path}/callinterface" | dot ${graphopts} -Grankdir="${rankdir}" -Granksep=2 "-T${xsuffix}" > "${outpath}/callInterface.${suffix}"
"$grp" "$outpath" "$inpath" "${path}/ignorelist" "${path}/formatcheck" | sed '/FormatCheck.R/!s/"\]/"; penwidth = 4]/' | dot ${graphopts} -Grankdir="${rankdir}" -Granksep=0.7 "-T${xsuffix}" > "${outpath}/FormatCheck.${suffix}"
"$grp" "$outpath" "$inpath" "${path}/ignoreFCI" | gvpr -i 'N[$.outdegree!=0 || $.indegree!=0]' | dot ${graphopts} -Grankdir="${rankdir}" -Granksep=0.2 "-T${xsuffix}" > "${outpath}/exported.${suffix}"
"$grp" "$outpath" "$inpath" "${path}/ignorelist" | gvpr -i 'N[$.outdegree!=0 || $.indegree!=0]' | ccomps -x | dot | gvpack -array3 | neato ${graphopts} -Goverlap=false -Gsplines=true "-T${xsuffix}" -Nshape=oval > "${outpath}/fullmap.${suffix}"

