## ---- results = "asis", echo = FALSE-------------------------------------

cat("hi", file = "/tmp/wri", append = TRUE)

knitr::opts_chunk$set(collapse = TRUE, comment = "#>")

path = names(knitr::opts_knit$get("encoding"))[1]

rpath = gsub("\\.[^.]*$", ".R", path)

knitr::knit_hooks$set(document = function(x) {
  lines = readLines(rpath)
  lines = gsub(" *(\n|$)", "\\1", lines)
  cat(lines, file = rpath, sep = "\n", append = FALSE)
  x
})



base = dirname(path)
file = basename(path)

allfiles = list.files(path = base, pattern = ".*\\.Rmd$")

stopifnot(file %in% allfiles)

fileinfolist = list()

for (cf in allfiles) {
  ismain = TRUE
  if (grepl("^z_", cf)) {
    infoslot = gsub("^z_", "", cf)
    infoslot = gsub("_terse\\.Rmd$", "", infoslot)
    subslot = "compact"
  } else {
    infoslot = gsub("^a_", "", cf)
    infoslot = gsub("\\.Rmd$", "", infoslot)
    subslot = "main"
  }

  content = scan(paste(base, cf, sep = "/"), what = "character", quiet = TRUE)
  pos = min(c(which(content == "title:"), Inf))
  if (is.infinite(pos)) {
    stop(sprintf("parsing error: %s", cf))
  }
  infolist = list(title = content[pos + 1], url = cf, iscurrent = cf == file)

  applist = list(infolist)
  names(applist) = subslot
  fileinfolist[[infoslot]] = c(fileinfolist[[infoslot]], applist)
}

linkify = function(info, title) {
  if (info$iscurrent) {
    title
  } else {
    sprintf("[%s](%s)", title, gsub("\\.Rmd$", ".html", info$url))
  }
}

for (idx in seq_along(fileinfolist)) {

  content = fileinfolist[[sort(names(fileinfolist))[idx]]]
  if (!is.null(content$compact)) {
    if (paste(content$main$title, "(No Output)") != content$compact$title) {
      stop(sprintf("File %s and its compact version %s have incompatible titles\nThe compact version must be paste(main_title, \"(No Output)\")",
        content$main$url, content$compact$url))
    }
    line = sprintf("%s (%s)", linkify(content$main, content$main$title), linkify(content$compact, "compact version"))
  } else {
    line = linkify(content$main, content$main$title)
  }
  cat(sprintf("%s. %s\n", idx, line))
}




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
getCPOClass(cpo)  # CPO Class: CPO, CPOInverter, CPORetrafo, NULLCPO
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
getCPOConstructor(cpo)  # the constructor used to create the CPO

## ------------------------------------------------------------------------
(sc = cpoScale())
getParamSet(sc)

## ------------------------------------------------------------------------
(sc = cpoScale(export = "export.none"))
getParamSet(sc)

## ------------------------------------------------------------------------
(sc = cpoScale(scale = FALSE, export = "export.unset"))
getParamSet(sc)

## ------------------------------------------------------------------------
head(iris) %>>% cpoPca()
head(getTaskData(applyCPO(cpoPca(), iris.task)))

## ------------------------------------------------------------------------
scale1 = cpoScale()
scale2 = cpoScale()

## ---- error = TRUE-------------------------------------------------------
scale1 %>>% scale2  # error! parameters 'center' and 'scale' occur in both

## ------------------------------------------------------------------------
compound = setCPOId(scale1, "scale1") %>>% setCPOId(scale2, "scale2")
composeCPO(setCPOId(scale1, "scale1"), setCPOId(scale2, "scale2"))  # same
class(compound)
print(compound, verbose = TRUE)
getCPOName(compound)
getParamSet(compound)
getHyperPars(compound)
setHyperPars(compound, scale1.center = TRUE, scale2.center = FALSE)

## ---- error = TRUE-------------------------------------------------------
getCPOId(compound)  # error: no ID for compound CPOs
getCPOAffect(compound)  # error: no affect for compound CPOs

## ------------------------------------------------------------------------
as.list(compound)
pipeCPO(as.list(compound))  # chainCPO: list CPO -> CPO

## ------------------------------------------------------------------------
lrn = makeLearner("classif.logreg")
(cpolrn = cpo %>>% lrn)  # the new learner has the CPO hyperparameters
attachCPO(compound, lrn)  # attaching compound CPO

## ------------------------------------------------------------------------
# CPO learner decomposition
getLearnerCPO(cpolrn)  # the CPO
getLearnerBare(cpolrn)  # the Learner

## ------------------------------------------------------------------------
transformed = iris %>>% cpo
head(transformed)
(ret = retrafo(transformed))

## ------------------------------------------------------------------------
# General methods that work on retrafo
getCPOName(ret)
getHyperPars(ret)
getCPOClass(ret)  # CPO Class: CPO, CPOInverter, CPORetrafo, NULLCPO
getCPOOperatingType(ret)  # Operating on feature, target, both?
getCPOPredictType(ret)
getCPOTrainedCapability(ret)  # can this be used for retrafo and/or invert?

## ------------------------------------------------------------------------
# retrafos are stored as attributes
attributes(transformed)$retrafo

## ------------------------------------------------------------------------
(state = getCPOTrainedState(retrafo(iris %>>% cpoScale())))
state$control$center[1] = 1000  # will now subtract 1000 from the first column
new.retrafo = makeCPOTrainedFromState(cpoScale, state)
head(iris %>>% new.retrafo)

## ------------------------------------------------------------------------
head(iris) %>>% retrafo(transformed)

## ---- eval = FALSE-------------------------------------------------------
#  # same:
#  applyCPO(retrafo(transformed), head(iris))
#  predict(retrafo(transformed), head(iris))

## ------------------------------------------------------------------------
data = head(iris) %>>% cpoPca()
retrafo(data)
data2 = data %>>% cpoScale()
# retrafo(data2) is the same as retrafo(data %>>% pca %>>% scale)
retrafo(data2)
# to interrupt this chain, set retrafo to NULL
retrafo(data) = NULL
data2 = data %>>% cpoScale()
retrafo(data2)

## ------------------------------------------------------------------------
compound.retrafo = retrafo(head(iris) %>>% compound)
compound.retrafo
(retrafolist = as.list(compound.retrafo))
retrafolist[[1]] %>>% retrafolist[[2]]
pipeCPO(retrafolist)

## ---- eval = FALSE-------------------------------------------------------
#  # there is currently no example targetbound cpo
#  logtransform = makeCPOTargetOp("logtransform",
#    properties.target = "regr", constant.invert = TRUE,
#    cpo.train = NULL,
#    cpo.train.invert = NULL,
#    cpo.retrafo = {
#      target[[1]] = log(target[[1]])
#      target
#    }, cpo.invert = { exp(target) })
#
#
#  log.retrafo = retrafo(bh.task %>>% logtransform())  # get a target-bound retrafo
#  getCPOKind(log.retrafo)  # logtransform is *stateless*, so it is a retrafo *and* an inverter
#  getCPOBound(log.retrafo)
#
#  inverter(bh.task %>>% log.retrafo)

## ---- eval = FALSE-------------------------------------------------------
#  inverter(tagInvert(bh.task) %>>% log.retrafo)

## ---- eval = FALSE-------------------------------------------------------
#  log.bh = bh.task %>>% logtransform()
#  log.prediction = predict(train("regr.lm", log.bh), log.bh)
#  invert(retrafo(log.bh), log.prediction)  # not implemented :-/
#  invert(retrafo(log.bh), log.prediction$data["response"])  # not implemented :-/

## ---- error = TRUE-------------------------------------------------------
getCPOProperties(cpoDummyEncode())
train("classif.geoDA", bc.task)  # gives an error

## ------------------------------------------------------------------------
train(cpoDummyEncode(reference.cat = TRUE) %>>% makeLearner("classif.geoDA"), bc.task)
getLearnerProperties("classif.geoDA")
getLearnerProperties(cpoDummyEncode(TRUE) %>>% makeLearner("classif.geoDA"))

