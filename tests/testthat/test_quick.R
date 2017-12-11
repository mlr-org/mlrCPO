
context("cpo unfinished tests")


test_that("getLearnerCPO, getLearnerBare: hyperparameter changes propagate", {

  combined = cpoadder.nt.o(id = "fst", summand = 10) %>>% cpomultiplier.nt.f() %>>% cpoadder.nt.f() %>>%  testlearnercpo

  expect_subset(c("multiplierF.factor", "int", "adderF.summand", "fst.summand"), names(getParamSet(combined)$pars))

  c2 = setHyperPars(combined, multiplierF.factor = 2, int = 10)

  decoupled = getLearnerBare(c2)

  expect_equal(getHyperPars(decoupled)$int, 10)

  expect_equal(getHyperPars(getLearnerCPO(c2)), list(fst.summand = 10, multiplierF.factor = 2, adderF.summand = 1))

  model = train(c2, cpo.df1c)

  rts = lapply(as.list(retrafo(model)), getCPOTrainedState)

  expect_equal(rts[[1]]$summand, 10)

  expect_equal(rts[[2]]$factor, 2)

  expect_equal(rts[[3]]$summand, 1)
})

test_that("warning about buried cpos", {

  combined = cpoadder.nt.o(id = "fst", summand = 10) %>>% cpomultiplier.nt.f() %>>% cpoadder.nt.f() %>>%  testlearnercpo

  f1 = function(data, target, args) {
    data[[1]] = data[[1]] * 10
    return(list(data = data, control = list()))
  }

  f2 = function(data, target, args, control) {
    data[[1]] = data[[1]] / 10
    return(data)
  }
  combined.wrapped = makePreprocWrapper(combined, train = f1, predict = f2, par.set = makeParamSet(), par.vals = list())

  expect_error(cpoadder.nt.o(id = "adderF") %>>% combined.wrapped, '"adderF\\.summand" occurs in both')

  combined2 = cpoadder.nt.o(id = "snd", summand = 20) %>>% combined.wrapped

  expect_subset(c("multiplierF.factor", "int", "adderF.summand", "fst.summand", "snd.summand"), names(getParamSet(combined2)$pars))

  combined2 = setHyperPars(combined2, env = environment(), int = 100, snd.summand = 3, fst.summand = 2, multiplierF.factor = 3)



  expect_warning({gotten.cpo = getLearnerCPO(combined2)}, "Learner.*buried CPOs")

  expect_identical(gotten.cpo, cpoadder.nt.o(id = "snd", summand = 3))

  cpotest.parvals = list()
  model = train(combined2, cpo.df1c)
  expect_equal(cpotest.parvals, list(((1 + 3) * 10 + 2) * 3 + 1))

  expect_warning(retrafo(model), "CPOs wrapped by other wrappers")
})

test_that("functional cpo 'data' is removed from functional environment; warning about 'data' reference", {
  data = 10

  cpo = makeCPOExtended("testdataremove", test: integer[, ], .dataformat = "df.all", cpo.trafo = {
    expect_equal(data, cpo.df1)
    cpo.retrafo = function(xx) {
      expect_equal(data, NULL)
      expect_equal(xx, cpo.df1)
      xx
    }
    data
    }, cpo.retrafo = NULL)

  expect_warning({ret = retrafo(cpo.df1 %>>% cpo(1))}, "references a data variable")

  expect_equal(cpo.df1 %>>% ret, cpo.df1)

})

test_that("functional retrafo recursion after getCPOTrainedState works", {

  cpo = makeCPOExtended("testrecursion", test: integer[, ], .dataformat = "df.all", cpo.trafo = {
    addendum = 1
    cpo.retrafo = function(data) {
      data[[1]] = data[[1]] + 1
      data[[2]] = data[[2]] + addendum
      if (data[[1]][1] == 2) {
        return(Recall(data))
      }
      if (data[[1]][1] == 3) {
        return(cpo.retrafo(data))
      }
      data
    }
    data
  }, cpo.retrafo = NULL)

  df = data.frame(a = 1:3, b = 0:2)

  ret = retrafo(df %>>% cpo(1))

  expect_equal(df %>>% ret, data.frame(a = 4:6, b = 3:5))

  state = getCPOTrainedState(ret)

  state$addendum = -1

  expect_equal(df %>>% makeCPOTrainedFromState(cpo, state), data.frame(a = 4:6, b = (-3):(-1)))

})

test_that("cpo state if cpo.retrafo missing from cpo.retrafo env;  fails if cpo.retrafo in 'cpo.retrafo's environment mismatches", {

  cpo.retrafo = NULL

  retr1 = (function() {
    cpo.retrafo = function(data) data
    cpo.retrafo
  })()

  retr2 = (function(){
    function(data) {
      if (data[[1]][1] == 2) {
        expect_function(cpo.retrafo)
      } else {
        expect_null(cpo.retrafo)
      }
      data
    }
  })()

  retr3 = (function() {
    cpo.retrafo = function(data) {
      data[[1]] = data[[1]] + 1
      data
    }
    function(data) {
      data
    }})()

  retr4 = (function() {
    cpo.retrafo = 10
    function(data) data
  })()


  cpo = makeCPOExtended("testrecursion", test: integer[, ], .dataformat = "df.all", cpo.trafo = {
    cpo.retrafo = switch(test, retr1, retr2, retr3, retr4)
    data
  }, cpo.retrafo = NULL)

  ret = retrafo(cpo.df1 %>>% cpo(1))
  expect_equal(cpo.df1 %>>% ret, cpo.df1)
  expect_equal(cpo.df1 %>>% makeCPOTrainedFromState(cpo, getCPOTrainedState(ret)), cpo.df1)

  df = data.frame(a = 1)
  ret = retrafo(df %>>% cpo(2))
  expect_equal(df %>>% ret, df)
  df = data.frame(a = 2)
  expect_equal(df %>>% makeCPOTrainedFromState(cpo, getCPOTrainedState(ret)), df)

  ret = retrafo(cpo.df1 %>>% cpo(3))
  expect_equal(cpo.df1 %>>% ret, cpo.df1)
  expect_error(cpo.df1 %>>% makeCPOTrainedFromState(cpo, getCPOTrainedState(ret)), "Could not get coherent state.*'cpo.retrafo'.*not identical")

  ret = retrafo(cpo.df1 %>>% cpo(4))
  expect_equal(cpo.df1 %>>% ret, cpo.df1)
  expect_error(cpo.df1 %>>% makeCPOTrainedFromState(cpo, getCPOTrainedState(ret)), "Could not get coherent state.*'cpo.retrafo'.*not identical")

})

test_that("complaint about missing parameter", {

  cpo = makeCPOObject("testparammissing", test: integer[, ], cpo.trafo = { control = 0; data },
                cpo.retrafo = { data })

  expect_error(train(cpo() %>>% testlearnercpo, cpo.df1c), "test of CPO testparammissing is missing")

  expect_identical(getLearnerCPO(cpo() %>>% testlearnercpo), cpo())

  expect_error(cpo.df1c %>>% cpo(), "test of CPO testparammissing is missing")

  cpo = makeCPOFunctional("testparammissing", test: integer[, ], cpo.trafo = { cpo.retrafo = identity ; data })

  expect_error(train(cpo() %>>% testlearnercpo, cpo.df1c), "test of CPO testparammissing is missing")

  expect_error(cpo.df1c %>>% cpo(), "test of CPO testparammissing is missing")

})

test_that("web demo", {
  train.task = subsetTask(bh.task, 1:200)
  test.task = subsetTask(bh.task, 201:400)

  logtransform = makeCPOTargetOp("logtransform", constant.invert = TRUE, properties.target = "regr",
    cpo.train = NULL, cpo.train.invert = NULL,
    cpo.retrafo = {
      target[[1]] = log(target[[1]])
      target
    }, cpo.invert = { exp(target) })

  expect_identical(getTaskData(train.task %>>% logtransform(), target.extra = TRUE)$target,
    log(getTaskData(train.task, target.extra = TRUE)$target))

  log.trans.learner = logtransform() %>>% makeLearner("regr.lm")

  model.trans = train(log.trans.learner, train.task)

  pred.trans = predict(model.trans, test.task)

  train.task.trans = train.task %>>% logtransform()
  model = train(makeLearner("regr.lm"), train.task.trans)

  rt = retrafo(train.task.trans)
  test.task.trans = test.task %>>% rt
  pred.trans.logdomain = predict(model, test.task.trans)

  test.task.trans = test.task %>>% rt
  pred.trans.logdomain = predict(model, test.task.trans)

  inv = inverter(test.task.trans)
  pred.trans.manual = invert(inv, pred.trans.logdomain)

  pred.trans.manual.notruth = invert(rt, pred.trans.logdomain)

  expect_identical(pred.trans$data, pred.trans.manual$data)

  expect_identical(pred.trans.manual$data[c("id", "response")], pred.trans.manual.notruth$data)

})

