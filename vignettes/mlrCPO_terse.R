## ---- echo = FALSE-------------------------------------------------------
#  knitr::opts_chunk$set(collapse = TRUE, comment = "#>")

## ------------------------------------------------------------------------
#  library("mlrCPO")
#  df = data.frame(a = 1:3, b = -(1:3) * 10)

## ------------------------------------------------------------------------
#  print(cpoPca)  # example CPOConstructor
#  class(cpoPca)

## ------------------------------------------------------------------------
#  names(formals(cpoPca))
#  print(cpoPca, verbose = TRUE)  # verbose print includes function info

## ------------------------------------------------------------------------
#  (cpo = cpoScale()) # construct CPO with default Hyperparameter values
#  class(cpo)  # CPOs that are not compound are "CPOPrimitive"
#  getCPOObjectType(cpo)  # Object type: CPO, CPOInverter, CPORetrafo, NULLCPO
#  getCPOOperatingType(cpo)  # Operating on feature, target, both?
#  print(cpo, verbose = TRUE)  # detailed printing

## ------------------------------------------------------------------------
#  getParamSet(cpo)
#  getHyperPars(cpo)
#  setHyperPars(cpo, scale.center = FALSE)
#  getCPOId(cpo)
#  setCPOId(cpo, "MYID")
#  getCPOName(cpo)
#  getCPOAffect(cpo)  # empty, since no affect set
#  getCPOAffect(cpoPca(affect.pattern = "Width$"))
#  getCPOProperties(cpo)  # see properties explanation below
#  getCPOPredictType(cpo)

## ------------------------------------------------------------------------
#  (sc = cpoScale())
#  getParamSet(sc)

## ------------------------------------------------------------------------
#  (sc = cpoScale(export = "export.none"))
#  getParamSet(sc)

## ------------------------------------------------------------------------
#  (sc = cpoScale(scale = FALSE, export = "export.unset"))
#  getParamSet(sc)

## ------------------------------------------------------------------------
#  head(iris) %>>% cpoPca()
#  head(getTaskData(applyCPO(cpoPca(), iris.task)))

## ------------------------------------------------------------------------
#  scale1 = cpoScale()
#  scale2 = cpoScale()

## ---- error = TRUE-------------------------------------------------------
#  scale1 %>>% scale2  # error! parameters 'center' and 'scale' occur in both

## ------------------------------------------------------------------------
#  compound = setCPOId(scale1, "scale1") %>>% setCPOId(scale2, "scale2")
#  composeCPO(setCPOId(scale1, "scale1"), setCPOId(scale2, "scale2"))  # same
#  class(compound)
#  print(compound, verbose = TRUE)
#  getCPOName(compound)
#  getParamSet(compound)
#  getHyperPars(compound)
#  setHyperPars(compound, scale1.center = TRUE, scale2.center = FALSE)

## ---- error = TRUE-------------------------------------------------------
#  getCPOId(compound)  # error: no ID for compound CPOs
#  getCPOAffect(compound)  # error: no affect for compound CPOs

## ------------------------------------------------------------------------
#  as.list(compound)
#  pipeCPO(as.list(compound))  # chainCPO: list CPO -> CPO

## ------------------------------------------------------------------------
#  lrn = makeLearner("classif.logreg")
#  (cpolrn = cpo %>>% lrn)  # the new learner has the CPO hyperparameters
#  attachCPO(compound, lrn)  # attaching compound CPO

## ------------------------------------------------------------------------
#  # CPO learner decomposition
#  getLearnerCPO(cpolrn)  # the CPO
#  getLearnerBare(cpolrn)  # the Learner

## ------------------------------------------------------------------------
#  transformed = iris %>>% cpo
#  head(transformed)
#  (ret = retrafo(transformed))

## ------------------------------------------------------------------------
#  # General methods that work on retrafo
#  getCPOName(ret)
#  getHyperPars(ret)
#  getCPOObjectType(ret)  # Object type: CPO, CPOInverter, CPORetrafo, NULLCPO
#  getCPOOperatingType(ret)  # Operating on feature, target, both?
#  getCPOPredictType(ret)
#  getCPOInvertCapability(ret)  # is it a retrafo, inverter, or hybrid?

## ------------------------------------------------------------------------
#  # retrafos are stored as attributes
#  attributes(transformed)

## ------------------------------------------------------------------------
#  (state = getRetrafoState(retrafo(iris %>>% cpoScale())))
#  state$control$center[1] = 1000  # will now subtract 1000 from the first column
#  new.retrafo = makeRetrafoFromState(cpoScale, state)
#  head(iris %>>% new.retrafo)

## ------------------------------------------------------------------------
#  head(iris) %>>% retrafo(transformed)

## ---- eval = FALSE-------------------------------------------------------
#  # same:
#  applyCPO(retrafo(transformed), head(iris))
#  predict(retrafo(transformed), head(iris))

## ------------------------------------------------------------------------
#  data = head(iris) %>>% cpoPca()
#  retrafo(data)
#  data2 = data %>>% cpoScale()
#  # retrafo(data2) is the same as retrafo(data %>>% pca %>>% scale)
#  retrafo(data2)
#  # to interrupt this chain, set retrafo to NULL
#  retrafo(data) = NULL
#  data2 = data %>>% cpoScale()
#  retrafo(data2)

## ------------------------------------------------------------------------
#  compound.retrafo = retrafo(head(iris) %>>% compound)
#  compound.retrafo
#  (retrafolist = as.list(compound.retrafo))
#  retrafolist[[1]] %>>% retrafolist[[2]]
#  pipeCPO(retrafolist)

## ---- eval = FALSE-------------------------------------------------------
#  # there is currently no example targetbound cpo
#  logtransform = makeCPOTargetOpExtended("logtransform", .data.dependent = FALSE,
#    .stateless = TRUE, .type = "regr",
#    cpo.trafo = {
#      target[[1]] = log(target[[1]])
#      target
#    }, cpo.retrafo = { print(match.call()) })
#
#
#  log.retrafo = retrafo(bh.task %>>% logtransform())  # get a target-bound retrafo
#  getCPOKind(log.retrafo)  # logtransform is *stateless*, so it is a retrafo *and* an inverter
#  getCPOBound(log.retrafo)
#
#  inverter(bh.task %>>% log.retrafo)

## ---- eval = FALSE-------------------------------------------------------
#  inverter(tagInvert(bh.task) %>>% log.retrafo)
#  # currently not implemented :-/

## ---- eval = FALSE-------------------------------------------------------
#  log.bh = bh.task %>>% logtransform()
#  log.prediction = predict(train("regr.lm", log.bh), log.bh)
#  invert(retrafo(log.bh), log.prediction)  # not implemented :-/
#  invert(retrafo(log.bh), log.prediction$data["response"])  # not implemented :-/

## ---- error = TRUE-------------------------------------------------------
#  getCPOProperties(cpoDummyEncode())
#  train("classif.geoDA", bc.task)  # gives an error

## ------------------------------------------------------------------------
#  train(cpoDummyEncode(reference.cat = TRUE) %>>% makeLearner("classif.geoDA"), bc.task)
#  getLearnerProperties("classif.geoDA")
#  getLearnerProperties(cpoDummyEncode(TRUE) %>>% makeLearner("classif.geoDA"))

## ------------------------------------------------------------------------
#  NULLCPO
#  is.nullcpo(NULLCPO)
#  NULLCPO %>>% cpoScale()
#  NULLCPO %>>% NULLCPO
#  print(as.list(NULLCPO))
#  pipeCPO(list())

## ------------------------------------------------------------------------
#  cpa = cpoWrap()
#  print(cpa, verbose = TRUE)
#  head(iris %>>% setHyperPars(cpa, wrap.cpo = cpoScale()))
#  head(iris %>>% setHyperPars(cpa, wrap.cpo = cpoPca()))
#  # attaching the cpo applicator to a learner gives this learner a "cpo" hyperparameter
#  # that can be set to any CPO.
#  getParamSet(cpoWrap() %>>% makeLearner("classif.logreg"))

## ------------------------------------------------------------------------
#  cpm = cpoMultiplex(list(cpoScale, cpoPca))
#  print(cpm, verbose = TRUE)
#  head(iris %>>% setHyperPars(cpm, selected.cpo = "scale"))
#  # every CPO's Hyperparameters are exported
#  head(iris %>>% setHyperPars(cpm, selected.cpo = "scale", scale.center = FALSE))
#  head(iris %>>% setHyperPars(cpm, selected.cpo = "pca"))

## ------------------------------------------------------------------------
#  s.and.p = cpoCase(logical.param: logical,
#  .export = list(cpoScale(id = "scale"),
#    cpoPca(id = "pca")),
#  cpo.build = function(data, target, logical.param, scale, pca) {
#    if (logical.param || mean(data[[1]]) > 10) {
#      scale %>>% pca
#    } else {
#      pca %>>% scale
#    }
#    })
#  print(s.and.p, verbose = TRUE)

## ------------------------------------------------------------------------
#  scale = cpoScale(id = "scale")
#  scale.pca = scale %>>% cpoPca()
#  cbinder = cpoCbind(scaled = scale, pcad = scale.pca, original = NULLCPO)

## ------------------------------------------------------------------------
#  # cpoCbind recognises that "scale.scale" happens before "pca.pca" but is also fed to the
#  # result directly. The summary draws a (crude) ascii-art graph.
#  print(cbinder, verbose = TRUE)
#  head(iris %>>% cbinder)

## ------------------------------------------------------------------------
#  # the unnecessary copies of "Species" are unfortunate. Remove them with cpoSelect:
#  selector = cpoSelect(type = "numeric")
#  cbinder.select = cpoCbind(scaled = selector %>>% scale, pcad = selector %>>% scale.pca, original = NULLCPO)
#  cbinder.select
#  head(iris %>>% cbinder)

## ------------------------------------------------------------------------
#  # alternatively, we apply the cbinder only to numerical data
#  head(iris %>>% cpoWrap(cbinder, affect.type = "numeric"))

## ------------------------------------------------------------------------
#  listCPO()$name

## ------------------------------------------------------------------------
#  df %>>% cpoScale()
#  df %>>% cpoScale(scale = FALSE)  # center = TRUE

## ------------------------------------------------------------------------
#  df %>>% cpoPca()

## ------------------------------------------------------------------------
#  head(iris %>>% cpoDummyEncode())
#  head(iris %>>% cpoDummyEncode(reference.cat = TRUE))

## ------------------------------------------------------------------------
#  head(iris %>>% cpoSelect(pattern = "Width"))
#  # selection is additive
#  head(iris %>>% cpoSelect(pattern = "Width", type = "factor"))

## ------------------------------------------------------------------------
#  head(iris) %>>% cpoDropConstants()  # drops 'species'
#  head(iris) %>>% cpoDropConstants(abs.tol = 0.2)  # also drops 'Petal.Width'

## ------------------------------------------------------------------------
#  levels(iris$Species)

## ------------------------------------------------------------------------
#  irisfix = head(iris) %>>% cpoFixFactors()  # Species only has level 'setosa' in train
#  levels(irisfix$Species)

## ------------------------------------------------------------------------
#  rf = retrafo(irisfix)
#  iris[c(1, 100, 140), ]
#  iris[c(1, 100, 140), ] %>>% rf

## ------------------------------------------------------------------------
#  impdata = df
#  impdata[[1]][1] = NA
#  impdata

## ------------------------------------------------------------------------
#  impdata %>>% cpoMissingIndicators()
#  impdata %>>% cpoCbind(NULLCPO, dummy = cpoMissingIndicators())

## ------------------------------------------------------------------------
#  head(iris %>>% cpoApplyFun(function(x) sqrt(x) - 10, affect.type = "numeric"))

## ------------------------------------------------------------------------
#  head(iris[sample(nrow(iris), 10), ] %>>% cpoAsNumeric())

## ------------------------------------------------------------------------
#  iris2 = iris
#  iris2$Species = factor(c("a", "b", "c", "b", "b", "c", "b", "c",
#                          as.character(iris2$Species[-(1:8)])))
#  head(iris2, 10)
#  head(iris2 %>>% cpoCollapseFact(max.collapsed.class.prevalence = 0.2), 10)

## ------------------------------------------------------------------------
#  head(iris %>>% cpoModelMatrix(~0 + Species:Petal.Width))
#  # use . + ... to retain originals
#  head(iris %>>% cpoModelMatrix(~0 + . + Species:Petal.Width))

## ------------------------------------------------------------------------
#  head(iris %>>% cpoScaleRange(-1, 1))

## ------------------------------------------------------------------------
#  head(iris %>>% cpoScaleMaxAbs(0.1))

## ------------------------------------------------------------------------
#  head(iris %>>% cpoSpatialSign())

## ------------------------------------------------------------------------
#  impdata %>>% cpoImpute(cols = list(a = imputeMedian()))

## ---- error = TRUE-------------------------------------------------------
#  impdata %>>% cpoImpute(cols = list(b = imputeMedian()))  # NAs remain
#  impdata %>>% cpoImputeAll(cols = list(b = imputeMedian()))  # error, since NAs remain

## ---- error = TRUE-------------------------------------------------------
#  missing.task = makeRegrTask("missing.task", impdata, target = "b")
#  # the following gives an error, since 'cpoImpute' does not make sure all missings are removed
#  # and hence does not add the 'missings' property.
#  train(cpoImpute(cols = list(a = imputeMedian())) %>>% makeLearner("regr.lm"), missing.task)

## ------------------------------------------------------------------------
#  # instead, the following works:
#  train(cpoImputeAll(cols = list(a = imputeMedian())) %>>% makeLearner("regr.lm"), missing.task)

## ------------------------------------------------------------------------
#  impdata %>>% cpoImputeConstant(10)
#  getTaskData(missing.task %>>% cpoImputeMedian())
#  # The specialised impute CPOs are:
#  listCPO()[listCPO()$category == "imputation" & listCPO()$subcategory == "specialised",
#            c("name", "description")]

## ------------------------------------------------------------------------
#  head(getTaskData(iris.task %>>% cpoFilterFeatures(method = "variance", perc = 0.5)))
#  head(getTaskData(iris.task %>>% cpoFilterVariance(perc = 0.5)))
#  # The specialised filter CPOs are:
#  listCPO()[listCPO()$category == "featurefilter" & listCPO()$subcategory == "specialised",
#            c("name", "description")]

## ------------------------------------------------------------------------
#  names(formals(makeCPO))  # see help(makeCPO) for explanation of arguments

## ------------------------------------------------------------------------
#  constFeatRem = makeCPO("constFeatRem",
#    dataformat = "df.features",
#    cpo.trafo = function(data, target) {
#      names(Filter(function(x) {  # names of columns to keep
#          length(unique(x)) > 1
#        }, data))
#      }, cpo.retrafo = function(data, control) {
#      data[control]
#    })
#  head(iris) %>>% constFeatRem()
#  print(constFeatRem, verbose = TRUE)

## ------------------------------------------------------------------------
#  constFeatRem = makeCPO("constFeatRem",
#    dataformat = "df.features",
#    cpo.trafo = function(data, target) {
#      cols.keep = names(Filter(function(x) {
#      length(unique(x)) > 1
#        }, data))
#      # the following function will do both the trafo and retrafo
#      result = function(data) {
#        data[cols.keep]
#      }
#      result
#    }, cpo.retrafo = NULL)
#  head(iris) %>>% constFeatRem()
#  print(constFeatRem, verbose = TRUE)

## ------------------------------------------------------------------------
#  (clrn = cpoModelMatrix() %>>% makeLearner("classif.logreg"))
#  getParamSet(clrn)
#
#  ps = makeParamSet(
#      makeDiscreteParam(
#          "model.matrix.formula",
#          values = list(first = ~0 + ., second = ~0 + .^2, third = ~0 + .^3)))
#
#  tuneParams(clrn, pid.task, cv5, par.set = ps,
#             control = makeTuneControlGrid(),
#             show.info=TRUE)

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

## ---- results = "asis"---------------------------------------------------
cat(knitr::knit_child("mlrCPO.Rmd", options = list(eval = FALSE)), sep = "\n")

