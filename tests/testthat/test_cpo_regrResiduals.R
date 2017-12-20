
context("cpoRegrResiduals test")


test_that("cpoRegrResiduals trafo works as expected", {

  residuals = train("regr.lm", bh.task)$learner.model$residuals

  trafo = bh.task %>>% cpoRegrResiduals("regr.lm")

  expect_equal(getTaskData(trafo, target.extra = TRUE)$target, unname(residuals))

  expect_equal(clearRI(bh.task %>>% retrafo(trafo)), clearRI(trafo))

  set.seed(123)
  preds = predict(train("regr.randomForest", bh.task, 1:100), bh.task)$data$response
  residuals = getTaskData(bh.task, target.extra = TRUE)$target - preds

  set.seed(123)
  trafo = subsetTask(bh.task, 1:100) %>>% cpoRegrResiduals("regr.randomForest")

  expect_equal(getTaskData(trafo, target.extra = TRUE)$target, residuals[1:100])

  expect_equal(getTaskData(bh.task %>>% retrafo(trafo), target.extra = TRUE)$target, residuals)

})

test_that("cpoRegrResiduals has expected properties and parameters", {

  expect_set_equal(intersect(getLearnerProperties("regr.lm"), cpo.dataproperties),
    intersect(getCPOProperties(cpoRegrResiduals("regr.lm"))$handling, cpo.dataproperties))


  expect_set_equal(intersect(getLearnerProperties("regr.randomForest"), cpo.dataproperties),
    intersect(getCPOProperties(cpoRegrResiduals("regr.randomForest"))$handling, cpo.dataproperties))

  delreq = function(ps) {
    ps$pars = lapply(ps$pars, function(x) {
      x$requires = NULL
      x
    })
    ps
  }

  expect_equal(delreq(getParamSet(makeLearner("regr.lm"))), delreq(getParamSet(cpoRegrResiduals("regr.lm", id = NULL))))

  expect_equal(delreq(getParamSet(makeLearner("regr.randomForest"))), delreq(getParamSet(cpoRegrResiduals("regr.randomForest", id = NULL))))
  expect_equal(delreq(getParamSet(makeLearner("regr.randomForest")))$pars[c("ntree", "se.boot")],
    delreq(getParamSet(cpoRegrResiduals("regr.randomForest", id = NULL, export = c("ntree", "se.boot"))))$pars)

  tlrn = setHyperPars(makeLearner("regr.lm"), tol = .2)
  expect_equal(getHyperPars(cpoRegrResiduals(tlrn, id = NULL)), getHyperPars(tlrn))

  cpo = cpoRegrResiduals(setHyperPars(makeLearner("regr.randomForest"), ntree = 100, se.boot = 101), export = "ntree")
  expect_equal(getHyperPars(cpo), list(regr.residuals.ntree = 100))

  expect_equal(getBareHyperPars(cpo), list(ntree = 100, se.boot = 101))

})

test_that("cpoRegrResiduals hyperparameters are used", {

  lrn = setHyperPars(makeLearner("regr.rpart"), minsplit = 3, minbucket = 5, maxdepth = 4)

  crr = cpoRegrResiduals(lrn, export = c("minsplit", "minbucket"), affect.index = 1:4)

  set.seed(123)
  trafo = bh.task %>>% setHyperPars(crr, regr.residuals.minsplit = 4)

  set.seed(123)
  model = train(makeLearner("regr.rpart", minsplit = 4, minbucket = 5, maxdepth = 4), subsetTask(bh.task, features = 1:4))

  model$time = 0

  ctrl = getCPOTrainedState(retrafo(trafo))$control
  ctrl$time = 0

  expect_identical(getHyperPars(ctrl$learner), getHyperPars(model$learner))  # in particular

  expect_equal(ctrl, model) # in general

})


test_that("cpoRegrResiduals inverts as expected", {

  trafo = bh.task %>>% cpoRegrResiduals("regr.lm")

  expect_equal(getCPOTrainedCapability(retrafo(trafo)), c(retrafo = 1, invert = -1))

  expect_error(invert(inverter(trafo), 1), "prediction to be inverted has different length from original task used for retrafo")

  expect_equal(invert(inverter(subsetTask(bh.task, 1:10) %>>% retrafo(trafo)), 1:10),
    unname(1:10 - getCPOTrainedState(retrafo(trafo))$control$learner.model$residuals[1:10] + getTaskData(bh.task, target.extra = TRUE)$target[1:10]))

  expect_equal(unname(invert(inverter(subsetTask(bh.task, 1:10) %>>% retrafo(trafo)), getCPOTrainedState(retrafo(trafo))$control$learner.model$residuals[1:10])),
    getTaskData(bh.task, target.extra = TRUE)$target[1:10])


  set.seed(123)
  trafo = subsetTask(bh.task, 1:10) %>>% cpoRegrResiduals("regr.randomForest")

  retrafo = subsetTask(bh.task, 11:20) %>>% retrafo(trafo)

  expect_equal(invert(inverter(retrafo), getTaskData(retrafo, target.extra = TRUE)$target), getTaskData(bh.task, 11:20, target.extra = TRUE)$target)

  retrafo = getTaskData(bh.task, 21:30, target.extra = TRUE)$data %>>% retrafo(trafo)

  expect_equal(clearRI(retrafo), getTaskData(bh.task, 21:30, target.extra = TRUE)$data)

  set.seed(123)
  prediction = predict(train("regr.randomForest", subsetTask(bh.task, 1:10)), subsetTask(bh.task, 21:30))$data$response

  expect_equal(invert(inverter(retrafo), getTaskData(bh.task, 21:30, target.extra = TRUE)$target - prediction),
    getTaskData(bh.task, 21:30, target.extra = TRUE)$target)

})

test_that("cpoRegrResiduals se inversion", {

  expect_error(setPredictType(makeLearner("regr.rpart"), "se"), "Trying to predict standard errors")

  expect_error(cpoRegrResiduals("regr.rpart", predict.se = TRUE), "must support properties 'se'")

  expect_equal(getCPOPredictType(cpoRegrResiduals("regr.lm")), c(response = "response"))

  expect_equal(getCPOPredictType(cpoRegrResiduals("regr.lm", predict.se = TRUE)), c(response = "response", se = "se"))

  trafo = subsetTask(bh.task, 100:200) %>>% cpoRegrResiduals("regr.lm", predict.se = TRUE)

  model = train(setPredictType(makeLearner("regr.lm"), "se"), subsetTask(bh.task, 100:200))

  ctrl = getCPOTrainedState(retrafo(trafo))$control
  ctrl$time = 0
  model$time = 0
  expect_equal(ctrl, model)

  expect_equal(predict(model, subsetTask(bh.task, 100:200))$data[c("response", "se")],
    getCPOTrainedState(inverter(trafo))$control)

  expect_equal(predict(model, subsetTask(bh.task, 1:99))$data[c("response", "se")],
    getCPOTrainedState(inverter(subsetTask(bh.task, 1:99) %>>% retrafo(trafo)))$control)

  expect_equal(predict(model, subsetTask(bh.task, 1:99))$data[c("response", "se")],
    getCPOTrainedState(inverter(getTaskData(bh.task, 1:99, target.extra = TRUE)$data %>>% retrafo(trafo)))$control)

  inv = inverter(getTaskData(bh.task, 201:300, target.extra = TRUE)$data %>>% retrafo(trafo))

  expect_equal(getCPOPredictType(inv), c(response = "response", se = "se"))

  prd = predict(model, subsetTask(bh.task, 201:300))$data
  res = getTaskData(bh.task, 201:300, target.extra = TRUE)$target - prd$response
  pse = prd$se
  expect_equal(invert(inv, res), getTaskData(bh.task, 201:300, target.extra = TRUE)$target)

  expect = cbind(getTaskData(bh.task, 201:300, target.extra = TRUE)$target, sqrt(pse^2 + 1))
  expect_equal(unname(invert(inv, cbind(res, 1), predict.type = "se")), expect)  # the main test!

})


test_that("cpoRegrResiduals handles discrete parameters correctly", {

  # mlr apparently has no learner with discrete vector learner params

  lrx = makeRLearnerRegr(cl = "testCpoRegrResiduals", package = character(0),
    par.set = makeParamSet(
        makeDiscreteVectorLearnerParam("dvlp1", len = 2, values = c(a = "b", b = "a", c = "x"), when = "both"),
        makeDiscreteVectorLearnerParam("dvlp2", len = 2, values = c(a = environment(), b = function() NULL), when = "both")),
    name = "testing", properties = c("numerics", "factors", "ordered", "missings"))
  lrx$fix.factors.prediction = FALSE

  trainLearner.testCpoRegrResiduals = function(.learner, .task, .subset, .weights, ...) {
    list(...)
  }

  predictLearner.testCpoRegrResiduals = function(.learner, .model, .newdata, ...) {
    rep(1, nrow(.newdata))
  }

  registerS3method("trainLearner", "testCpoRegrResiduals", trainLearner.testCpoRegrResiduals)
  registerS3method("predictLearner", "testCpoRegrResiduals", predictLearner.testCpoRegrResiduals)

  # test that this learner does what it is supposed to
  expect_equal(train(lrx, bh.task)$learner.model, list())

  expect_equal(train(setHyperPars(lrx, dvlp1 = list("a", "a")), bh.task)$learner.model, list(dvlp1 = list("a", "a")))

  expect_equal(train(setHyperPars(lrx, dvlp1 = list("a", "b"), dvlp2 = list(a = function() NULL, b = function() NULL)), bh.task)$learner.model,
    list(dvlp1 = list("a", "b"), dvlp2 = list(a = function() NULL, b = function() NULL)))

  expect_equal(predict(train(lrx, bh.task), bh.task)$data$response, rep(1, getTaskSize(bh.task)))


  # test that the dvlp are given to the crr as they should
  crr = cpoRegrResiduals(setHyperPars(lrx, dvlp1 = list("a", "x"), dvlp2 = list(a = function() NULL, b = function() NULL)))

  x = bh.task %>>% crr

  expect_equal(getTaskData(x, target.extra = TRUE)$target, getTaskData(bh.task, target.extra = TRUE)$target - 1)

  expect_equal(getCPOTrainedState(retrafo(x))$control$learner.model, list(dvlp1 = list("a", "x"), dvlp2 = list(a = function() NULL, b = function() NULL)))

  expect_equal(getCPOTrainedState(bh.task %>|% setHyperPars(crr, regr.residuals.dvlp1 = list("b", "a")))$control$learner.model,
    list(dvlp1 = list("b", "a"), dvlp2 = list(a = function() NULL, b = function() NULL)))

  expect_equal(getCPOTrainedState(bh.task %>|% setHyperPars(crr, regr.residuals.dvlp1 = list("x", "x")))$control$learner.model,
    list(dvlp1 = list("x", "x"), dvlp2 = list(a = function() NULL, b = function() NULL)))

  expect_equal(getCPOTrainedState(bh.task %>|% setHyperPars(crr, regr.residuals.dvlp2 = list(environment(), function() NULL)))$control$learner.model,
    list(dvlp1 = list("a", "x"), dvlp2 = list(environment(), function() NULL)))

})
