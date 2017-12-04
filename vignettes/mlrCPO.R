## ---- echo = FALSE-------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")

## ------------------------------------------------------------------------
library("mlrCPO")
df = data.frame(a = 1:3, b = -(1:3) * 10)

## ------------------------------------------------------------------------
print(cpoPca)  # example CPOConstructor
class(cpoPca)

## ------------------------------------------------------------------------
names(formals(cpoPca))
print(cpoPca, verbose = TRUE)  # verbose print includes function info

## ------------------------------------------------------------------------
(cpo = cpoScale()) # construct CPO with default Hyperparameter values
class(cpo)  # CPOs that are not compound are "CPOPrimitive"
getCPOObjectType(cpo)  # Object type: CPO, CPOInverter, CPORetrafo, NULLCPO
getCPOOperatingType(cpo)  # Operating on feature, target, both?
print(cpo, verbose = TRUE)  # detailed printing

## ------------------------------------------------------------------------
getParamSet(cpo)
getHyperPars(cpo)
setHyperPars(cpo, scale.center = FALSE)
getCPOId(cpo)
setCPOId(cpo, "MYID")
getCPOName(cpo)
getCPOAffect(cpo)  # empty, since no affect set
getCPOAffect(cpoPca(affect.pattern = "Width$"))
getCPOProperties(cpo)  # see properties explanation below
getCPOPredictType(cpo)

## ------------------------------------------------------------------------
(sc = cpoScale())
getParamSet(sc)

## ------------------------------------------------------------------------
(sc = cpoScale(export = "export.none"))
getParamSet(sc)

## ------------------------------------------------------------------------
(sc = cpoScale(scale = FALSE, export = "export.unset"))
getParamSet(sc)

