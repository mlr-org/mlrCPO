## ---- results = "asis", echo = FALSE-------------------------------------

path = names(knitr::opts_knit$get("encoding"))[1]

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




## ---- echo = FALSE-------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")

## ------------------------------------------------------------------------
library("mlrCPO")

## ------------------------------------------------------------------------
cpoScale  # a cpo constructor

## ------------------------------------------------------------------------
print(cpoScale, verbose = TRUE)  # more information
cpoScale()  # create a CPO object that scales and centers data

## ------------------------------------------------------------------------
head(iris %>>% cpoScale())

## ------------------------------------------------------------------------
head(getTaskData(iris.task %>>% cpoScale()))

## ------------------------------------------------------------------------
cpoScale() %>>% cpoPca()

## ------------------------------------------------------------------------
listCPO()$name

## ------------------------------------------------------------------------
transformed = iris %>>% cpoPca()
ret = retrafo(transformed)
print(ret)

## ------------------------------------------------------------------------
head(transformed)
head(iris) %>>% ret

## ------------------------------------------------------------------------
head(iris) %>>% cpoPca()

## ------------------------------------------------------------------------
trans.a = iris %>>% cpoScale()
trans.b = trans.a %>>% cpoPca()
ret = retrafo(trans.b)

## ------------------------------------------------------------------------
head(trans.b)
head(iris) %>>% ret

## ------------------------------------------------------------------------
print(cpoScale)

## ------------------------------------------------------------------------
do.center = cpoScale(scale = FALSE, center = TRUE)
print(do.center, verbose = TRUE)  # note the 'scale.' prefix of parameters

## ------------------------------------------------------------------------
do.scale = setHyperPars(do.center,
  scale.scale = TRUE, scale.center = FALSE)

## ------------------------------------------------------------------------
cpo = cpoScale() %>>% cpoPca()
lrn = cpo %>>% makeLearner("classif.logreg")
print(lrn)

## ------------------------------------------------------------------------
(clrn = cpoModelMatrix() %>>% makeLearner("classif.logreg"))

## ------------------------------------------------------------------------
getParamSet(clrn)

## ------------------------------------------------------------------------
ps = makeParamSet(
    makeDiscreteParam(
        "model.matrix.formula",
        values = list(first = ~0 + ., second = ~0 + .^2, third = ~0 + .^3)))

tuneParams(clrn, pid.task, cv5, par.set = ps,
           control = makeTuneControlGrid(),
           show.info=TRUE)

## ------------------------------------------------------------------------
cpm = cpoMultiplex(list(cpoScale, cpoPca))
print(cpm, verbose = TRUE)

## ------------------------------------------------------------------------
head(iris %>>% setHyperPars(cpm, selected.cpo = "scale"))

## ------------------------------------------------------------------------
head(iris %>>% setHyperPars(cpm, selected.cpo = "scale", scale.center = FALSE))

## ------------------------------------------------------------------------
head(iris %>>% setHyperPars(cpm, selected.cpo = "pca"))

## ------------------------------------------------------------------------
cpa = cpoWrap()
print(cpa, verbose = TRUE)

## ------------------------------------------------------------------------
head(iris %>>% setHyperPars(cpa, wrap.cpo = cpoScale()))

## ------------------------------------------------------------------------
head(iris %>>% setHyperPars(cpa, wrap.cpo = cpoPca()))

## ------------------------------------------------------------------------
getParamSet(cpoWrap() %>>% makeLearner("classif.logreg"))

## ------------------------------------------------------------------------
scale = cpoScale(id = "scale")
scale.pca = scale %>>% cpoPca()
cbinder = cpoCbind(scaled = scale, pcad = scale.pca, original = NULLCPO)

## ------------------------------------------------------------------------
print(cbinder)

## ------------------------------------------------------------------------
getParamSet(cbinder)

## ------------------------------------------------------------------------
head(iris %>>% cbinder)

## ------------------------------------------------------------------------
selector = cpoSelect(type = "numeric")
cbinder.select = cpoCbind(scaled = selector %>>% scale, pcad = selector %>>% scale.pca, original = NULLCPO)
cbinder.select
head(iris %>>% cbinder)

## ------------------------------------------------------------------------
head(iris %>>% cpoWrap(cbinder, affect.type = "numeric"))

## ------------------------------------------------------------------------
names(formals(makeCPO))  # see help(makeCPO) for explanation of arguments

## ------------------------------------------------------------------------
constFeatRem = makeCPO("constFeatRem",
  dataformat = "df.features",
  cpo.train = function(data, target) {
    names(Filter(function(x) {  # names of columns to keep
        length(unique(x)) > 1
      }, data))
    }, cpo.retrafo = function(data, control) {
    data[control]
  })
head(iris) %>>% constFeatRem()
print(constFeatRem, verbose = TRUE)

## ------------------------------------------------------------------------
(clrn = cpoModelMatrix() %>>% makeLearner("classif.logreg"))
getParamSet(clrn)

ps = makeParamSet(
    makeDiscreteParam(
        "model.matrix.formula",
        values = list(first = ~0 + ., second = ~0 + .^2, third = ~0 + .^3)))

tuneParams(clrn, pid.task, cv5, par.set = ps,
           control = makeTuneControlGrid(),
           show.info=TRUE)

## ---- eval = FALSE-------------------------------------------------------
#  tlrn = cpoModelMatrix() %>>%
#         cpoWrap() %>>%
#         cpoFilterGainRatio() %>>%
#         makeLearner("classif.ctree")
#  sprintf("Parameters: %s", paste(names(getParamSet(tlrn)$pars), collapse=", "))
#  ps2 = makeParamSet(
#      makeDiscreteParam(
#          "model.matrix.formula",
#          values = list(first = ~0 + ., second = ~0 + .^2)),
#      makeDiscreteParam(
#          "wrap.cpo",
#          values = list(nopca = NULLCPO,
#                        onlypca = cpoPca(),
#                        addpca = cpoCbind(NULLCPO, cpoPca()))),
#      makeDiscreteParam(
#          "gain.ratio.perc",
#          values = list(0.333, 0.667, 1.0)),
#      makeDiscreteParam("teststat", values = c("quad", "max")))
#  
#  tuneParams(tlrn, pid.task, cv5, par.set = ps2,
#             control = makeTuneControlGrid())

