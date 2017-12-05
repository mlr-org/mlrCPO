#!/bin/sh
path="$(dirname "$0")"

grp="${path}/grp.sh"

if [ -z "$3" ] ; then
  echo usage: "$0" "inpath" "outpath" "outtype"
  echo "outtype should be understood by graphviz (png, pdf, ...)"
  exit 1
fi

inpath="$1"
outpath="$2"
suffix="$3"

graphopts="-Nmargin=0 -Nfontsize=18"

"$grp" "$outpath" "$inpath" "${path}/ignoreNoMakeCPO" "${path}/includeMakeCPO" | gvpr -i 'N[$.outdegree!=0 || $.indegree!=0]' | dot ${graphopts} -Grankdir=LR -Granksep=0.4 "-T${suffix}" > "${outpath}/makeCPO.${suffix}"
"$grp" "$outpath" "$inpath" "${path}/ignorelist" "${path}/callinterface" | dot ${graphopts} -Grankdir=LR -Granksep=2 "-T${suffix}" > "${outpath}/callInterface.${suffix}"
"$grp" "$outpath" "$inpath" "${path}/ignorelist" "${path}/formatcheck" | sed '/FormatCheck.R/!s/"\]/"; penwidth = 5]/' | dot ${graphopts} -Grankdir=LR -Granksep=1 "-T${suffix}" > "${outpath}/FormatCheck.${suffix}"
"$grp" "$outpath" "$inpath" "${path}/ignoreFCI" | gvpr -i 'N[$.outdegree!=0 || $.indegree!=0]' | dot ${graphopts} -Grankdir=LR -Granksep=0.6 "-T${suffix}" > "${outpath}/exported.${suffix}"
"$grp" "$outpath" "$inpath" "${path}/ignorelist" | gvpr -i 'N[$.outdegree!=0 || $.indegree!=0]' | ccomps -x | dot | gvpack -array3 | neato ${graphopts} -Goverlap=false -Gsplines=true "-T${suffix}" > "${outpath}/fullmap.${suffix}"
