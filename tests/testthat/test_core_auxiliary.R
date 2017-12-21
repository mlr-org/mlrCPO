context("auxiliary UI stuff")


test_that("auxiliary functions work", {

  x = 10
  y = 20

  c(A = x, B = y, C = x + y, D = z) %<=% list(x = 1, z = 99)

  expect_equal(A, 1)
  expect_equal(B, 20)
  expect_equal(C, 21)
  expect_equal(D, 99)

  list(A = x, B = y, C = z) %<=% list(
                              x = expression(1 + 1),
                              y = function() 1 + 1,
                              z = quote(1 + 1))

  expect_identical(A, expression(1 + 1))
  expect_identical(B, function() 1 + 1)
  expect_identical(C, quote(1 + 1))

})

test_that("new operators work", {

  x = cpoScale()
  y = cpoPca()
  w = cpoSelect()
  z = pid.task

  z %<>>% y %<<<% x %>>% cpoSelect("numeric")

  expect_equal(z, pid.task %>>% cpoScale() %>>% cpoSelect("numeric") %>>% cpoPca())

  expect_identical(retrafo(z), retrafo(pid.task %>>% cpoScale() %>>% cpoSelect("numeric") %>>% cpoPca()))

  expect_identical(y, cpoScale() %>>% cpoSelect("numeric") %>>% cpoPca())


  expect_identical(pid.task %>|% cpoPca(), retrafo(pid.task %>>% cpoPca()))

  expect_identical(pid.task %>|% cpoPca() %>>% cpoScale(), retrafo(pid.task %>>% cpoPca() %>>% cpoScale()))

  expect_identical(cpoPca() %>>% cpoScale() %|<% pid.task, retrafo(pid.task %>>% cpoPca() %>>% cpoScale()))

})

test_that("index reference in cpoSelect is relative to data cols", {

  expectedcols = getTaskData(cpo.df4l, target.extra = TRUE)$data

  for (i in seq_along(expectedcols)) {
    trans = cpo.df4l %>>% cpoSelect(index = i)
    expect_equal(getTaskData(trans, target.extra = TRUE)$data, expectedcols[i])
    expect_equal(getTaskData(cpo.df4l %>>% retrafo(trans), target.extra = TRUE)$data, expectedcols[i])
    expect_equal(dropNamed(cpo.df4 %>>% retrafo(trans), c("T1", "T2")), expectedcols[i])
  }

})


test_that("printers", {

  expect_output(print(cpoScale()), "scale(center = TRUE, scale = TRUE)", fixed = TRUE)
  expect_output(print(cpoScale(center = FALSE)), "scale(center = FALSE, scale = TRUE)", fixed = TRUE)
  expect_output(print(cpoScale(id = "test")), "test<scale>(center = TRUE, scale = TRUE)", fixed = TRUE)
  expect_output(print(cpoScale(export = "center")), "scale(center = TRUE)[not exp'd: scale = TRUE]", fixed = TRUE)
  expect_output(print(cpoScale(scale = FALSE, export = "center")), "scale(center = TRUE)[not exp'd: scale = FALSE]", fixed = TRUE)

  expect_output(print(cpoScale(), verbose = TRUE), "Trafo chain of 1 cpos:\nscale(center = TRUE, scale = TRUE)\nOperating: feature\nParamSet:", fixed = TRUE)
  expect_output({invprint = !cpoScale()}, "Trafo chain of 1 cpos:\nscale(center = TRUE, scale = TRUE)\nOperating: feature\nParamSet:", fixed = TRUE)
  expect_output(print(invprint), NA)

  expect_output(print(cpoScale(export = character(0)), verbose = TRUE),
    "Trafo chain of 1 cpos:\nscale()[not exp'd: center = TRUE, scale = TRUE]\nOperating: feature\nParamSet:", fixed = TRUE)
  expect_output({invprint = !cpoScale(export = character(0))},
    "Trafo chain of 1 cpos:\nscale()[not exp'd: center = TRUE, scale = TRUE]\nOperating: feature\nParamSet:", fixed = TRUE)
  expect_output(print(invprint), NA)

  notrans = makeCPOTargetOp("notrans", properties.target = "multilabel", pSS(param: integer[, ]), constant.invert = TRUE, cpo.train.invert = NULL,
    cpo.train = NULL, cpo.retrafo = { target }, cpo.invert = { target })

  expect_output(print(notrans), "<<CPO notrans(param)>>", fixed = TRUE)

  expect_output(print(notrans, verbose = TRUE), "<<CPO notrans(param)>>\n\ncpo.retrafo:\nfunction (param, data, target) \n{\n", fixed = TRUE)
  expect_output({invprint = !notrans}, "<<CPO notrans(param)>>\n\ncpo.retrafo:\nfunction (param, data, target) \n{\n", fixed = TRUE)
  expect_output(print(invprint), NA)

  expect_output(print(notrans(), verbose = TRUE), "Conversion: multilabel -> multilabel\nPredict type mapping:\nresponse -> response", fixed = TRUE)
  expect_output({invprint = !notrans()}, "Conversion: multilabel -> multilabel\nPredict type mapping:\nresponse -> response", fixed = TRUE)
  expect_output(print(invprint), NA)

  dotrans = makeCPOTargetOp("dotrans", pSS(param: integer[, ]), constant.invert = FALSE, cpo.train.invert = { },
    cpo.train = { }, cpo.retrafo = { target[[1]] = as.numeric(target[[1]]) ; target[1] }, cpo.invert = { target }, task.type.out = "regr",
    properties.target = "multilabel", predict.type.map = c(response = "se", prob = "response"))

  expect_output(print(dotrans(), verbose = TRUE), "Conversion: multilabel -> regr\nPredict type mapping:\nresponse -> se\nprob -> response", fixed = TRUE)


  expect_output(print(cpo.df4l %>|% notrans(1)),
    "CPO Retrafo / Inverter chain {type:multilabel} (able to predict 'response')\n[RETRAFO notrans(param = 1){type:multilabel}", fixed = TRUE)

  expect_output(print(cpo.df4l %>|% (cpoPca() %>>% dotrans(1))),
    "CPO Retrafo chain {conv:multilabel->regr}\n[RETRAFO pca(center = TRUE, scale = FALSE)] =>\n[RETRAFO dotrans(param = 1)]", fixed = TRUE)

  expect_output(print(cpo.df4l %>|% (cpoPca() %>>% dotrans(1)), verbose = TRUE),
    "CPO Retrafo chain {conv:multilabel->regr}\n[RETRAFO pca(center = TRUE, scale = FALSE, tol = <NULL>, rank = <NULL>)] =>\n[RETRAFO dotrans(param = 1)]", fixed = TRUE)

  expect_output({invprint = !cpo.df4l %>|% (cpoPca() %>>% dotrans(1))},
    "CPO Retrafo chain {conv:multilabel->regr}\n[RETRAFO pca(center = TRUE, scale = FALSE, tol = <NULL>, rank = <NULL>)] =>\n[RETRAFO dotrans(param = 1)]", fixed = TRUE)
  expect_output(print(invprint), NA)

  expect_output(print(inverter(cpo.df4l %>>% dotrans(1))),
    "CPO Inverter chain {conv:regr->multilabel} (able to predict 'response', 'prob')\n[INVERTER dotrans(param = 1){conv:regr->multilabel}", fixed = TRUE)

  expect_output(print(inverter(cpo.df4l %>>% notrans(0) %>>% dotrans(1))),
    "CPO Inverter chain {conv:regr->multilabel} (able to predict 'response')\n[INVERTER dotrans(param = 1){conv:regr->multilabel}] =>\n[INVERTER notrans(param = 0){type:multilabel}]", fixed = TRUE)

})

test_that("NULLCPO", {

  expect_identical(cpoPca() %>>% NULLCPO, cpoPca())
  expect_identical(NULLCPO %>>% cpoPca(), cpoPca())

  expect_identical(composeCPO(cpoPca(), NULLCPO), cpoPca())
  expect_identical(composeCPO(NULLCPO, cpoPca()), cpoPca())

  expect_identical(NULLCPO %>>% makeLearner("classif.logreg"), makeLearner("classif.logreg"))
  expect_identical(attachCPO(NULLCPO, makeLearner("classif.logreg")), makeLearner("classif.logreg"))

  nullsv = NULLCPO
  expect_error(NULLCPO %<>>% cpoPca(), "Cowardly refusing to assign to NULLCPO")
  expect_error(NULLCPO %<<<% cpoPca(), "Cowardly refusing to assign to NULLCPO")
  NULLCPO = nullsv  # nolint

  expect_identical(pid.task %>|% NULLCPO, NULLCPO)
  expect_identical(NULLCPO %|<% pid.task, NULLCPO)

  expect_identical(pid.task %>>% NULLCPO, pid.task)
  expect_identical(applyCPO(NULLCPO, pid.task), pid.task)

  expect_identical(predict(NULLCPO, pid.task), pid.task)
  expect_identical(invert(NULLCPO, cpo.df5), cpo.df5)

  expect_true(is.nullcpo(NULLCPO))
  expect_false(is.nullcpo(NULL))

  expect_null(getCPOTrainedState(NULLCPO))

  expect_identical(getParamSet(NULLCPO), pSS())

  expect_identical(getHyperPars(NULLCPO), namedList())

  expect_error(setCPOId(NULLCPO, "X"), "Cannot set ID of NULLCPO.")

  expect_equal(getCPOId(NULLCPO), "NULLCPO")

  expect_identical(getCPOAffect(NULLCPO), getCPOAffect(cpoPca()))
  expect_identical(getCPOAffect(NULLCPO, drop.defaults = FALSE), getCPOAffect(cpoPca(), drop.defaults = FALSE))

  expect_identical(getCPOProperties(NULLCPO), getCPOProperties(cpoSelect()))
  expect_identical(getCPOProperties(NULLCPO, get.internal = TRUE), getCPOProperties(cpoSelect(), get.internal = TRUE))
  expect_identical(getCPOProperties(NULLCPO, only.data = TRUE), getCPOProperties(cpoSelect(), only.data = TRUE))

  expect_equal(getCPOName(NULLCPO), "NULLCPO")
  expect_equal(getCPOId(NULLCPO), "NULLCPO")
  expect_equal(getCPOClass(NULLCPO), "NULLCPO")
  expect_equal(getCPOTrainedCapability(NULLCPO), c(retrafo = 0L, invert = 0L))
  expect_equal(getCPOOperatingType(NULLCPO), character(0))
  expect_equal(getCPOTrainedCPO(NULLCPO), NULLCPO)

  expect_error(getCPOConstructor(NULLCPO), "No CPOConstructor for NULLCPO")
  expect_identical(as.list(NULLCPO), list())
  expect_output(print(NULLCPO), "NULLCPO")

  ret = pid.task %>|% cpoPca()

  expect_identical(ret %>>% NULLCPO, ret)
  expect_identical(composeCPO(ret, NULLCPO), ret)
  expect_identical(NULLCPO %>>% ret, ret)
  expect_identical(composeCPO(NULLCPO, ret), ret)

  expect_identical(retrafo(train("classif.logreg", pid.task)), NULLCPO)


})

test_that("operators", {

  cons = generalMakeCPO("test", type = "target")
  cpo = cons(2)
  comb = cons() %>>% cons(id = "2nd")
  ret = retrafo(bh.task %>>% cpo)
  combret = ret %>>% ret
  combret2 = retrafo(bh.task %>>% comb)
  inv = inverter(bh.task %>>% ret)
  combinv = inv %>>% inv
  lrn = cpo %>>% makeLearner("regr.lm")


  expect_error(getCPOAffect(cons), "no applicable method")
  expect_identical(getCPOAffect(cpo), namedList())
  expect_named(getCPOAffect(cpo, FALSE), c("type", "index", "names", "pattern", "invert", "pattern.ignore.case", "pattern.perl", "pattern.fixed"))
  expect_error(getCPOAffect(comb), "Compound CPOs have no affect")

  expect_equal(getCPOClass(cons), "CPOConstructor")
  expect_equal(getCPOClass(cpo), "CPO")
  expect_equal(getCPOClass(comb), "CPO")
  expect_equal(getCPOClass(ret), "CPORetrafo")
  expect_equal(getCPOClass(combret), "CPORetrafo")
  expect_equal(getCPOClass(inv), "CPOInverter")
  expect_equal(getCPOClass(combinv), "CPOInverter")
  expect_equal(getCPOClass(NULLCPO), "NULLCPO")

  expect_identical(getCPOConstructor(ret), cons)
  expect_identical(getCPOConstructor(inv), cons)
  expect_identical(getCPOConstructor(cpo), cons)
  expect_error(getCPOConstructor(combret), "Compound .* cannot be queried")
  expect_error(getCPOConstructor(combinv), "Compound .* cannot be queried")
  expect_error(getCPOConstructor(comb), "Compound .* cannot be queried")
  expect_error(getCPOConstructor(NULLCPO), "No CPOConstructor for NULLCPO")

  expect_identical(getCPOId(cpo), "test")
  expect_error(getCPOId(comb), "Compound CPOs have no ID")
  expect_equal(getCPOId(NULLCPO), "NULLCPO")

  expect_equal(getCPOName(cons), "test")
  expect_equal(getCPOName(cpo), "test")
  expect_equal(getCPOName(comb), "test.test")
  expect_equal(getCPOName(ret), "test")
  expect_equal(getCPOName(combret), "test.test")
  expect_equal(getCPOName(inv), "test")
  expect_equal(getCPOName(combinv), "test.test")
  expect_equal(getCPOName(NULLCPO), "NULLCPO")

  expect_equal(getCPOOperatingType(cpo), "target")
  expect_equal(getCPOOperatingType(comb), "target")
  expect_equal(getCPOOperatingType(cpoPca()), "feature")
  expect_set_equal(getCPOOperatingType(comb %>>% cpoPca()), c("target", "feature"))
  expect_equal(getCPOOperatingType(NULLCPO), character(0))
  expect_equal(getCPOOperatingType(ret), "feature")
  expect_equal(getCPOOperatingType(inv), "target")
  expect_equal(getCPOOperatingType(combret), "feature")
  expect_equal(getCPOOperatingType(combinv), "target")

  expect_equal(getCPOPredictType(cpo), c(response = "response"))
  expect_equal(getCPOPredictType(generalMakeCPO("test", type = "target", predict.type.map = c(se = "se", response = "response"))()), c(se = "se", response = "response"))

  expect_identical(getCPOTrainedCPO(ret), cpo)
  expect_identical(getCPOTrainedCPO(inv), cpo)
  expect_identical(getCPOTrainedCPO(combret2), comb)
  expect_identical(getCPOTrainedCPO(NULLCPO), NULLCPO)

  expect_identical(getLearnerBare(makeLearner("regr.lm")), makeLearner("regr.lm"))
  expect_identical(getLearnerBare(makeLearner("regr.lm")), makeLearner("regr.lm"))
  expect_identical(getLearnerBare(setHyperPars(lrn, tol = 0.123)), makeLearner("regr.lm", tol = 0.123))

  expect_identical(getLearnerCPO(lrn), cpo)
  expect_identical(getLearnerCPO(setHyperPars(lrn, test.param = 10)), setHyperPars(cpo, test.param = 10))

  tunelrn = makeTuneWrapper(lrn, hout, par.set = pSS(tol: numeric[.001, .01]), control = makeTuneControlRandom(maxit = 3), show.info = FALSE)

  doublewrapped = cpoPca() %>>% tunelrn

  expect_equal(getLearnerBare(doublewrapped), tunelrn)

  expect_warning(expect_identical(getLearnerCPO(doublewrapped), cpoPca()), "had buried CPOs")

  expect_warning(expect_identical(retrafo(train(doublewrapped, bh.task)), retrafo(bh.task %>>% cpoPca())), "CPOs wrapped by other wrappers")

  wm = train("classif.logreg", pid.task)
  expect_error({retrafo(wm) = NULLCPO}, "Cannot change retrafo of a model")

  expect_error(inverter(wm), "Cannot get inverter of a model")
  expect_error({inverter(wm) = NULLCPO}, "Cannot change inverter of a model")


})

test_that("helpers", {

  cons = generalMakeCPO("test", type = "target")
  cpo = cons(2)
  comb = cons() %>>% cons(id = "2nd")
  ret = retrafo(bh.task %>>% cpo)
  combret = ret %>>% ret
  combret2 = retrafo(bh.task %>>% comb)
  inv = inverter(bh.task %>>% ret)
  combinv = inv %>>% inv
  lrn = cpo %>>% makeLearner("regr.lm")

  expect_true(is.retrafo(ret))
  expect_false(is.inverter(ret))
  expect_false(is.nullcpo(ret))

  expect_false(is.retrafo(inv))
  expect_true(is.inverter(inv))
  expect_false(is.nullcpo(inv))

  expect_true(is.retrafo(NULLCPO))
  expect_true(is.inverter(NULLCPO))
  expect_true(is.nullcpo(NULLCPO))

  expect_null(attr(clearRI(bh.task %>>% cpo), "retrafo"))
  expect_null(attr(clearRI(bh.task %>>% cpo), "inverter"))
  expect_identical(attr(bh.task %>>% cpo, "retrafo"), ret)
  expect_identical(attr(bh.task %>>% cpo, "inverter"), inv)

  afterconv = bh.task %>>% cpo
  retrafo(afterconv) = NULL
  expect_identical(retrafo(afterconv), NULLCPO)
  expect_null(attr(afterconv, "retrafo"))

  afterconv = bh.task %>>% cpo
  retrafo(afterconv) = NULLCPO
  expect_identical(retrafo(afterconv), NULLCPO)
  expect_null(attr(afterconv, "retrafo"))

  afterconv = bh.task %>>% cpo
  inverter(afterconv) = NULL
  expect_identical(inverter(afterconv), NULLCPO)
  expect_null(attr(afterconv, "inverter"))

  afterconv = bh.task %>>% cpo
  inverter(afterconv) = NULLCPO
  expect_identical(inverter(afterconv), NULLCPO)
  expect_null(attr(afterconv, "inverter"))

  expect_error({inverter(afterconv) = ret})
  expect_error({retrafo(afterconv) = inv})

  mod = train(lrn, bh.task)
  expect_error({retrafo(mod) = ret}, "Cannot change retrafo of a model")

  expect_null(nullcpoToNull(NULLCPO))
  expect_identical(nullcpoToNull(list()), list())

  expect_identical(nullToNullcpo(NULL), NULLCPO)
  expect_identical(nullToNullcpo(list()), list())

  expect_error(NULLCPO %>>% "test", "Cannot compose CPO Retrafo with object of class.*character")

})

test_that("inverter is noop when no targetbounds", {
  expect_identical(invert(inverter(bh.task %>>% cpoPca() %>>% cpoScale()), 1:10), 1:10)
  expect_identical(invert(inverter(bh.task %>>% cpoPca() %>>% cpoScale()), c("a", "b", "c")), c("a", "b", "c"))
})


test_that("applyCPO, composeCPO, attachCPO do what they should do", {

  expect_equal(applyCPO(cpoPca(), pid.task), pid.task %>>% cpoPca())

  ret = pid.task %>|% cpoPca()
  expect_equal(applyCPO(ret, pid.task), pid.task %>>% ret)

  expect_equal(composeCPO(cpoPca(), cpoScale()), cpoPca() %>>% cpoScale())
  expect_equal(composeCPO(ret, ret), ret %>>% ret)

  expect_equal(attachCPO(cpoPca(), "classif.logreg"), cpoPca() %>>% makeLearner("classif.logreg"))

})

test_that("fixFactors works", {

  testff = function(fix) {

    df = data.frame(
        a = factor(c("a", "b", "b", "a"), levels = c("c", "b", "a")),
        b = factor(c("a", "b", "b", "a"), levels = c("a", "b", "c")),
        c = factor(c("a", "b", "b", "a"), levels = c("a", "b", "c")))

    dfdroppedall = droplevels(df)
    dfdroppedone = droplevels(df, except = c("b", "c"))

    tsk = makeClassifTask("lvltask", df, target = "c", fixup.data = "no", check.data = FALSE)

    tskdroppedall = makeClassifTask("lvltask", droplevels(df, except = "c"), target = "c", fixup.data = "no", check.data = FALSE)
    tskdroppedone = makeClassifTask("lvltask", droplevels(df, except = c("b", "c")), target = "c", fixup.data = "no", check.data = FALSE)

    localenv = new.env()

    applyGMC("fixfactors", TRUE, fix.factors = fix,
      type = c("simple", "extended"),
      train = function(data, target, param) {
        if (missing(target)) {
          target = "x"
        }
        levels(dropNamed(data, target)[[1]])
      },
      retrafo = function(data, target, control, param) {
        if (!missing(target) && length(target)) {
          expect_identical(levels(data[[target]]), localenv$exptarget)
        } else {
          target = "x"
        }
        if (fix) expect_identical(localenv$explvl, control)
        expect_identical(localenv$explvl, levels(dropNamed(data, target)[[1]]))
        data
      },
      convertfrom = "classif",
      applyfun = function(cpocon, type, line, dfx) {
        for (i in 1:2) {
          if (i == 1) {
            if (fix) {
              dfdropped = dfdroppedall
              taskdropped = tskdroppedall
            } else {
              dfdropped = df
              taskdropped = tsk
            }
            cpo = cpocon()
          } else {
            if (fix) {
              dfdropped = dfdroppedone
              taskdropped = tskdroppedone
            } else {
              dfdropped = df
              taskdropped = tsk
            }
            cpo = cpocon(affect.index = 1)
          }
          localenv$explvl = c("c", "b", "a")
          localenv$exptarget = c("a", "b", "c")

          res = df %>>% cpo
          expect_identical(clearRI(res), df)
          expect_identical(clearRI(df %>>% retrafo(res)), df)

          localenv$explvl = c("b", "a")
          res = dfdroppedall %>>% cpo
          if (fix) {
            localenv$explvl = c("b", "a")
          } else {
            localenv$explvl = c("c", "b", "a")
          }
          expect_identical(clearRI(res), dfdroppedall)
          expect_identical(clearRI(df %>>% retrafo(res)), dfdropped)
          localenv$explvl = c("c", "b", "a")

          res = tsk %>>% cpo
          expect_equal(clearRI(res), tsk)
          expect_equal(clearRI(tsk %>>% retrafo(res)), tsk)
          expect_identical(clearRI(df %>>% retrafo(res)), df)

          localenv$explvl = c("b", "a")
          res = tskdroppedall %>>% cpo
          if (fix) {
            localenv$explvl = c("b", "a")
          } else {
            localenv$explvl = c("c", "b", "a")
          }
          expect_equal(clearRI(res), tskdroppedall)
          expect_equal(clearRI(tsk %>>% retrafo(res)), taskdropped)
          expect_identical(clearRI(df %>>% retrafo(res)), getTaskData(taskdropped))
        }
      })
  }

  testff(FALSE)
  testff(TRUE)

})

###

test_that("various error messages", {

  wtask = makeClassifTask(data = cpo.df1, target = "F1", weights = c(.1, .2, .3))

  expect_error(wtask %>>% cpoPca(), "CPO can not handle tasks with weights!")

  ret = cpo.df1c %>|% cpoPca()

  cpo.df1cfaux = makeRegrTask("cpo.df1cfaux", { x = cpo.df1 ; x$F1 = 2 ; x }, target = "F1")

  expect_error(cpo.df1cfaux %>>% ret, "CPO trained with task of type classif cannot operate on task of type regr")

  ret = cpo.df4l %>|% cpoPca()

  expect_error(cpo.df4[, -2] %>>% ret, "Some, but not all target columns of training data found.* T2")

  expect_error(expect_warning(applyCPO.CPO(ret, 1)), "Data fed into CPO.*not a Task or data.frame")  # very far fetched

  cpx = makeCPO("errreturn", cpo.train = NULL, cpo.retrafo = { 1 })

  expect_error(pid.task %>>% cpx(), "gave bad result.*must return a data.frame")

  cpx = makeCPOExtendedTrafo("errreturn2", dataformat = "task",
    cpo.trafo = {
      control = NULL
      makeClassifTask("gen", getTaskData(data)[-1, ], target)
    }, cpo.retrafo = {
      data
    })

  expect_error(pid.task %>>% cpx(), "must not change number of rows")

  cpx = makeCPOExtendedTrafo("errreturn3", dataformat = "task",
    cpo.trafo = {
      control = NULL
      data$task.desc$class.distribution %+=% 1
      data
    }, cpo.retrafo = { data })

  expect_error(pid.task %>>% cpx(), "changed task description item class.distribution")





})

### other features

test_that("missings tolerated in retrafo in certain conditions", {

})

test_that("exported parameters work as expected", {

})


test_that("dataformat.factor.with.ordered influences strictness of property presence check", {

})


test_that("convertNamesToItems etc", {
  # convertNamesToItems
  # convertItemsToNames
})

test_that("on.par.out.of.bounds respected", {

  # also with convertItemsToNames etc.
})

test_that("'sometimes'-properties work as expected", {

})

test_that("stateless target cpo  CPOs must be constant.invert", {

  expect_error(makeCPOTargetOp("test", cpo.train = NULL, cpo.retrafo = { target }, cpo.train.invert = NULL, cpo.invert = { target }),
    "constant.invert must be TRUE")

  expect_class(makeCPOTargetOp("test", constant.invert = TRUE,
    cpo.train = NULL, cpo.retrafo = { target }, cpo.train.invert = NULL, cpo.invert = { target }), "CPOConstructor")

})

test_that("CPOTrainedState works for all CPO types", {

})

### concrete cpos

test_that("cbind doesn't accept tocpos / rocpos", {

})

test_that("cbind accepts tocpo / rocpo when in acceptable parts of the graph", {


})

test_that("cpoSelectFreeProperties", {

})

test_that("cpoTransformParams", {


})

test_that("cpoWrapRetrafoless", {

})



test_that("multiplexer works for all types", {
  # even NULLCPO
  # property composition

})

test_that("multiplexer checks its arguments", {

})


test_that("cpoCase works for all types", {
  # even NULLCPO
  # property composition
})

test_that("cpoCase checks its arguments", {


})

test_that("cpoCase checks the generated CPO", {


})



test_that("cpoCache", {

})

