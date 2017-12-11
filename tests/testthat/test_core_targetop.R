context("target operation CPOs")



test_that("capabilities behave as expected when breaking and rebuilding CPOTrained", {

  # vars named by capabilities: 1 - yes, 0 - indif, -1 - no
  # [retrafo].[invert]

  task = cpo.df5r
  pred = getTaskData(task, target.extra = TRUE)$target

  indif.indif = NULLCPO
  yes.yes = retrafo(task %>>% generalMakeCPO("yes.no", type = "target", ci = TRUE)())
  yes.indif = retrafo(task %>>% generalMakeCPO("yes.indif", type = "simple", dataformat = "split")())
  yes.no = retrafo(task %>>% generalMakeCPO("yes.no", type = "target")())
  no.yes = inverter(task %>>% generalMakeCPO("yes.no", type = "target")())

  testCapability = function(cpotrained, expected.cap) {
    caps = getCPOTrainedCapability(cpotrained)
    expect_identical(caps, expected.cap)
    if (caps["retrafo"] != -1) {
      task %>>% cpotrained
      getTaskData(task, target.extra = TRUE)$data %>>% cpotrained
    } else {
      expect_error(task %>>% cpotrained, "no applicable method")
    }

    if (caps["invert"] != -1) {
      expect_identical(invert(cpotrained, pred), pred)
    } else {
      expect_error(invert(cpotrained, pred), "not possible.*data-dependent inverter")
    }
  }

  testCapability((indif.indif), c(retrafo = 0L, invert = 0L))
  testCapability((yes.yes), c(retrafo = 1L, invert = 1L))
  testCapability((yes.indif), c(retrafo = 1L, invert = 0L))
  testCapability((yes.no), c(retrafo = 1L, invert = -1L))
  testCapability((no.yes), c(retrafo = -1L, invert = 1L))

  testCapability((yes.yes %>>% yes.indif), c(retrafo = 1L, invert = 1L))
  testCapability((yes.indif %>>% yes.yes), c(retrafo = 1L, invert = 1L))
  testCapability((yes.yes %>>% yes.no), c(retrafo = 1L, invert = -1L))
  testCapability((yes.no %>>% yes.yes), c(retrafo = 1L, invert = -1L))
  testCapability((yes.no %>>% yes.indif), c(retrafo = 1L, invert = -1L))
  testCapability((yes.indif %>>% yes.no), c(retrafo = 1L, invert = -1L))

  testCapability((no.yes %>>% yes.indif), c(retrafo = -1L, invert = 1L))
  testCapability((yes.indif %>>% no.yes), c(retrafo = -1L, invert = 1L))

  testCapability((no.yes %>>% yes.yes), c(retrafo = -1L, invert = 1L))
  testCapability((yes.yes %>>% no.yes), c(retrafo = -1L, invert = 1L))

  expect_error(yes.no %>>% no.yes, "neither do retrafo nor invert")

  testCapability(pipeCPO(as.list(yes.yes %>>% yes.indif %>>% no.yes %>>% yes.yes)[c(2, 4)]), c(retrafo = 1L, invert = 1L))

})

test_that("composing CPO with conversion etc. behaves as expected", {

  reg.to.clas.const = generalMakeCPO("reg.to.clas", ci = TRUE,
    type = "target",
    retrafo = function(data, control, param, target) {
      tcol = grep("^target\\.", colnames(data))
      data[[tcol]] = as.factor(data[[tcol]])
      data
    },
    invert = function(target, predict.type, control, param) {
      target[[1]] = as.numeric(as.character(target[[1]]))
      target
    },
    convertfrom = "regr", convertto = "classif")

  reg.to.reg.const = generalMakeCPO("reg.to.reg", ci = TRUE,
    type = "target",
    retrafo = function(data, control, param, target) {
      data
    }, convertfrom = "regr", convertto = "regr")

  clas.to.reg.const = generalMakeCPO("clas.to.reg", ci = TRUE,
    type = "target",
    train = function(data, target, param) {
      levels(data[[target]])
    },
    retrafo = function(data, control, param, target) {
      tcol = grep("^target\\.", colnames(data))
      data[[tcol]] = as.numeric(data[[tcol]])
      data
    },
    invert = function(target, predict.type, control, param) {
      beforefac = round(target[[1]])
      beforefac = pmax(beforefac, 1)
      beforefac = pmin(beforefac, length(control))
      target = factor(beforefac, levels = seq_along(control))
      levels(target) = control
      data.frame(target.target = target)
    }, convertfrom = "classif", convertto = "regr")

  clas.to.multilab.const = generalMakeCPO("clas.to.multilab", ci = TRUE,
    type = "target",
    train = function(data, target, param) {
      levels(data[[target]])
    },
    retrafo = function(data, control, param, target) {
      tcol = grep("^target\\.", colnames(data))
      newcols = sapply(levels(data[[tcol]]), function(lvl) {
        data[[tcol]] == lvl
      })
      cbind(data[-tcol], target = newcols)
    },
    invert = function(target, predict.type, control, param) {
      colnames(target) = control
      data.frame(target = factor(apply(target, 1, function(x) c(colnames(target)[x], colnames(target)[1])[1]), levels = control))
    },
    convertfrom = "classif", convertto = "multilabel")

  clas.to.surv.const = generalMakeCPO("clas.to.surv", ci = TRUE,
    type = "target",
    train = function(data, target, param) {
      levels(data[[target]])
    },
    retrafo = function(data, control, param, target) {
      tcol = grep("^target\\.", colnames(data))
      data[[tcol]] = as.numeric(data[[tcol]])
      cbind(data, target.surv = TRUE)
    },
    invert = function(target, predict.type, control, param) {
      beforefac = round(target[[1]])
      beforefac = pmax(beforefac, 1)
      beforefac = pmin(beforefac, length(control))
      target = factor(beforefac, levels = seq_along(control))
      levels(target) = control
      data.frame(target.target = target)
    }, convertfrom = "classif", convertto = "surv")

  reg.to.surv.const = generalMakeCPO("reg.to.surv", ci = TRUE,
    type = "target",
    retrafo = function(data, control, param, target) {
      cbind(data, target.surv = TRUE)
    }, convertfrom = "regr", convertto = "surv")

  reg.to.clas.new.const = generalMakeCPO("reg.to.clas.new", ci = TRUE,
    type = "target",
    properties.adding = character(0), properties.needed = "twoclass",
    train = function(data, target, param) {
      list(mean = mean(data[[target]]), sd = sd(data[[target]]))
    },
    retrafo = function(data, control, param, target) {
      tcol = grep("^target\\.", colnames(data))
      data[[tcol]] = as.factor(as.numeric(data[[tcol]] > control$mean) - 1)
      data
    },
    invert = function(target, predict.type, control, param) {
      target[[1]] = c(control$mean - control$sd, control$mean + control$sd)[target[[1]]]
      target
    },
    convertfrom = "regr", convertto = "classif")

  surv.to.clas.const = generalMakeCPO("surv.to.clas", ci = TRUE,
    type = "target",
    properties.adding = character(0), properties.needed = "twoclass",
    train = function(data, target, param) {
      list(mean = mean(data[[target[1]]]), sd = sd(data[[target[1]]]))
    },
    retrafo = function(data, control, param, target) {
      tcol = grep("^target\\.", colnames(data))
      data[[tcol[1]]] = as.factor(as.numeric((data[[tcol[1]]] + !data[[tcol[2]]]) > control$mean) - 1)
      data[[tcol[2]]] = NULL
      data
    },
    invert = function(target, predict.type, control, param) {
      target[[1]] = c(control$mean - control$sd, control$mean + control$sd)[target[[1]]]
      target
    },
    convertfrom = "surv", convertto = "classif")

  multilab.to.clas.const = generalMakeCPO("mutilab.to.class", ci = TRUE,
    type = "target",
    train = function(data, target, param) {
      gsub("^target\\.", "", target)
    },
    retrafo = function(data, control, param, target) {
      tcol = grep("^target\\.", colnames(data))
      tx = data[tcol]
      data[tcol] = NULL
      tcolred = gsub("^target\\.", "", target)
      cbind(data, target.target = factor(apply(tx, 1, function(x) c(tcolred[x], tcolred[1])[1]), levels = tcolred))
    },
    invert = function(target, predict.type, control, param) {
      sapply(control, function(lvl) target == lvl)
    },
    convertfrom = "multilabel", convertto = "classif")


  reg.to.clas = reg.to.clas.const()
  reg.to.reg = reg.to.reg.const()
  reg.to.clas.new = reg.to.clas.new.const()
  clas.to.reg = clas.to.reg.const()
  clas.to.multilab = clas.to.multilab.const()
  clas.to.surv = clas.to.surv.const()
  reg.to.surv = reg.to.surv.const()
  surv.to.clas = surv.to.clas.const()
  multilab.to.clas = multilab.to.clas.const()

  expect_equal(reg.to.clas$convertfrom, "regr")
  expect_equal(reg.to.clas$convertto, "classif")

  expect_equal((reg.to.reg %>>% reg.to.clas)$convertfrom, "regr")
  expect_equal((reg.to.reg %>>% reg.to.clas)$convertto, "classif")

  expect_equal(((reg.to.reg %>>% reg.to.clas) %>>% clas.to.multilab)$convertfrom, "regr")
  expect_equal(((reg.to.reg %>>% reg.to.clas) %>>% clas.to.multilab)$convertto, "multilabel")

  expect_equal((reg.to.reg %>>% (reg.to.clas %>>% clas.to.multilab))$convertfrom, "regr")
  expect_equal((reg.to.reg %>>% (reg.to.clas %>>% clas.to.multilab))$convertto, "multilabel")

  expect_error(reg.to.clas %>>% reg.to.reg, "creates data with propert.*classif")
  expect_error(clas.to.multilab %>>% reg.to.reg, "creates data with propert.*multilabel")

  lrn = makeLearner("classif.randomForest")

  expect_equal(getLearnerType(lrn), "classif")

  expect_equal(getLearnerType(reg.to.clas %>>% lrn), "regr")

  expect_equal(getLearnerType(reg.to.reg %>>% reg.to.clas %>>% lrn), "regr")
  expect_equal(getLearnerType(reg.to.reg %>>% (reg.to.clas %>>% lrn)), "regr")

  expect_error(reg.to.clas %>>% makeLearner("classif.logreg"), "reg.to.clas.*multiclass.*can not handle")

  expect_equal(getLearnerType(reg.to.reg %>>% makeLearner("regr.randomForest")), "regr")

  expect_equal(getLearnerType(reg.to.reg %>>% reg.to.clas %>>% clas.to.multilab %>>% makeLearner("multilabel.randomForestSRC")), "regr")

  expect_error(reg.to.reg %>>% lrn, "outputs type regr with learner of type classif")

  expect_error(reg.to.clas %>>% (reg.to.clas %>>% lrn), "outputs type classif with learner of type regr")


  expect_equal(getTaskDesc(bh.task %>>% reg.to.clas)$type, "classif")

  expect_equal(getTaskDesc(bh.task %>>% reg.to.clas %>>% clas.to.multilab)$type, "multilabel")

  tname = getTaskTargetNames(cpo.df5c)
  expected = dropNamed(cpo.df5, tname)
  expected = cbind(expected, data.frame(a = cpo.df5[[tname]] == "a", b = cpo.df5[[tname]] == "b"))

  expect_equal(getTaskData(cpo.df5c %>>% clas.to.multilab), expected)

  chained = iris.task %>>% clas.to.reg %>>% reg.to.reg %>>% reg.to.clas %>>% clas.to.multilab

  expected = as.data.frame(t(sapply(rep(c(1, 2, 3), each = 50), function(x) 1:3 == x)))
  colnames(expected) = 1:3

  expect_identical(getTaskData(chained, target.extra = TRUE)$target, expected)

  retr = retrafo(chained)
  inv = inverter(chained)
  inv2 = inverter(iris.task %>>% retr)

  expect_identical(inv, inv2)

  expect_equal(retr$convertfrom, "classif")
  expect_equal(inv$convertfrom, "classif")

  expect_equal(retr$convertto, "multilabel")
  expect_equal(inv$convertto, "multilabel")

  inv.tmp = inverter(iris.task %>>% clas.to.multilab)

  indf = data.frame(`1` = c(FALSE, FALSE, TRUE), `2` = c(FALSE, FALSE, TRUE), `3` = c(TRUE, FALSE, FALSE))
  expt = factor(c("virginica", "setosa", "setosa"), levels = levels(iris[[5]]))
  expect_identical(invert(inv.tmp, indf), expt)

  inv.tmp = inverter(iris.task %>>% clas.to.reg)
  expect_identical(invert(inv.tmp, c(3, 1, 1)), expt)

  res = bh.task %>>% reg.to.clas

  expect_identical(invert(inverter(res), getTaskData(res, target.extra = TRUE)$target),
    getTaskData(bh.task, target.extra = TRUE)$target)


  expect_identical(invert(retr, indf), expt)

  expect_identical(invert(inv, indf), expt)

  expect_equal(as.list(retr)[[1]]$convertfrom, "classif")
  expect_equal(as.list(retr)[[1]]$convertto, "regr")
  expect_equal(as.list(retr)[[2]]$convertfrom, "regr")
  expect_equal(as.list(retr)[[3]]$convertto, "classif")
  expect_equal((as.list(retr)[[1]] %>>% as.list(retr)[[3]])$convertfrom, "classif")
  expect_equal((as.list(retr)[[1]] %>>% as.list(retr)[[3]])$convertto, "classif")

  expect_equal(as.list(inv)[[1]]$convertfrom, "classif")
  expect_equal(as.list(inv)[[1]]$convertto, "regr")
  expect_equal(as.list(inv)[[2]]$convertfrom, "regr")
  expect_equal(as.list(inv)[[3]]$convertto, "classif")
  expect_equal((as.list(inv)[[1]] %>>% as.list(inv)[[3]])$convertfrom, "classif")
  expect_equal((as.list(inv)[[1]] %>>% as.list(inv)[[3]])$convertto, "classif")

  expect_equal((as.list(inv)[[1]] %>>% as.list(retr)[[3]])$convertfrom, "classif")
  expect_equal((as.list(retr)[[1]] %>>% as.list(inv)[[3]])$convertto, "classif")

  expect_identical(invert(as.list(retr)[[1]] %>>% as.list(inv)[[3]], as.factor(as.numeric(iris[[5]]))),
    iris[[5]])

  expect_equal((((as.list(retr)[[1]] %>>% as.list(inv)[[2]]) %>>% as.list(inv)[[3]]) %>>% as.list(retr)[[4]])$convertfrom, "classif")
  expect_equal((((as.list(retr)[[1]] %>>% as.list(inv)[[2]]) %>>% as.list(inv)[[3]]) %>>% as.list(retr)[[4]])$convertto, "multilabel")

  expect_equal(((as.list(retr)[[1]] %>>% as.list(inv)[[2]]) %>>% (as.list(inv)[[3]] %>>% as.list(retr)[[4]]))$convertfrom, "classif")
  expect_equal(((as.list(retr)[[1]] %>>% as.list(inv)[[2]]) %>>% (as.list(inv)[[3]] %>>% as.list(retr)[[4]]))$convertto, "multilabel")




  testinv = function(data, cpo, lrn) {

    rd = makeResampleInstance("Holdout", data)

    train.set = subsetTask(data, rd$train.inds[[1]])
    test.set = subsetTask(data, rd$test.inds[[1]])

    mod = train(cpo %>>% lrn, train.set)
    prd = predict(mod, test.set)
    prd.df = predict(mod, newdata = getTaskData(test.set, target.extra = TRUE)$data)

    tmptrain = train.set %>>% cpo
    tmptst = test.set %>>% retrafo(tmptrain)
    mod2 = train(lrn, tmptrain)
    prd2 = predict(mod2, tmptst)
    tmptst.df = getTaskData(test.set, target.extra = TRUE)$data %>>% retrafo(tmptrain)
    prd2.df = predict(mod2, newdata = tmptst.df)

    expect_identical(invert(retrafo(tmptrain), prd2.df)$data, prd.df$data)

    expect_identical(invert(inverter(tmptst), prd2)$data, prd$data)

    expect_identical(invert(retrafo(tmptrain), prd2)$data$response, prd$data$response)

    expect_true(!is.na(holdout(cpo %>>% lrn, data, show.info = FALSE)$aggr))
  }

  options(mlr.debug.seed = 3)
  testinv(iris.task, clas.to.reg, makeLearner("regr.lm"))
  testinv(pid.task, clas.to.reg, makeLearner("regr.lm"))
  testinv(bh.task, reg.to.clas, makeLearner("classif.randomForestSRC", seed = 1, mtry = 1, ntree = 1))
  testinv(iris.task, clas.to.multilab, makeLearner("multilabel.randomForestSRC", seed = -1, mtry = 1, ntree = 1))
  testinv(iris.task, clas.to.surv, makeLearner("surv.coxph"))
  testinv(bh.task, reg.to.surv, makeLearner("surv.coxph"))
  testinv(iris.task, clas.to.reg %>>% reg.to.surv, makeLearner("surv.coxph"))

  testinv(bh.task, reg.to.clas.new, makeLearner("classif.logreg"))
  testinv(lung.task, surv.to.clas, makeLearner("classif.logreg"))
  testinv(yeast.task, multilab.to.clas, makeLearner("classif.randomForestSRC", seed = 1, mtry = 1, ntree = 1))

  testinv(bh.task, reg.to.clas.new %>>% clas.to.multilab %>>% multilab.to.clas %>>% clas.to.surv, makeLearner("surv.coxph"))

})

test_that("composing CPO, CPOTrained with conversion etc. behaves as expected", {

  se.adder = generalMakeCPO("se.adder", ci = TRUE,
    type = "target",
    retrafo = function(data, control, param, target) {
      data
    },
    invert = function(target, predict.type, control, param) {
      if (predict.type == "se") {
        expect_identical(predict.type, "se")
        cbind(target, xx = 1)
      } else {
        expect_identical(predict.type, "response")
        target
      }
    }, convertfrom = "regr", convertto = "regr",
    predict.type.map = c(response = "response", se = "response"))()

  se.remover = generalMakeCPO("se.remover", ci = TRUE,
    type = "target",
    retrafo = function(data, control, param, target) {
      data
    },
    invert = function(target, predict.type, control, param) {
      expect_identical(predict.type, "response")
      target[1]
    }, convertfrom = "regr", convertto = "regr",
    predict.type.map = c(response = "se"))()

  se.from.prob = generalMakeCPO("se.from.prob", ci = TRUE,
    type = "target",
    properties.adding = character(0), properties.needed = "twoclass",
    train = function(data, target, param) {
      list(mean = mean(data[[target]]), sd = sd(data[[target]]))
    },
    retrafo = function(data, control, param, target) {
      tcol = grep("^target\\.", colnames(data))
      data[[tcol]] = as.factor(as.numeric(data[[tcol]] > control$mean) - 1)
      data
    },
    invert = function(target, predict.type, control, param) {
      posn = c(control$mean - control$sd, control$mean + control$sd)
      if (predict.type == "se") {
        expect_identical(predict.type, "se")
        means = apply(target, 1, weighted.mean, x = posn)
        sd = vnapply(seq_along(means), function(i) {
          sqrt(sum(target[i, ] * (posn - means[i])^2))
        })
        data.frame(pred = means, sd = sd)
      } else {
        expect_identical(predict.type, "response")
        target[[1]] = posn[target[[1]]]
        target
      }
    },
    convertfrom = "regr", convertto = "classif",
    predict.type.map = c(response = "response", se = "prob"))(affect.index = c(1, 3))

  prob.from.response = generalMakeCPO("prob.from.response", ci = TRUE,
    type = "target",
    train = function(data, target, param) {
      levels(data[[target]])
    },
    invert = function(target, predict.type, control, param) {
      if (predict.type == "prob") {
        expect_identical(predict.type, "prob")
        probs = c(1 / (1 + length(control)), 1 - 1 / (1 + length(control)))
        sapply(control, function(lvl) probs[1 + (target == lvl)])
      } else {
        expect_identical(predict.type, "response")
        target
      }
    }, convertfrom = "classif", predict.type.map = c(response = "response", prob = "response"))()

  loud.prob.from.response = generalMakeCPO("prob.from.response", ci = TRUE,
    type = "target",
    train = function(data, target, param) {
      levels(data[[target]])
    },
    invert = function(target, predict.type, control, param) {
      cat(predict.type)
      cat("\n")
      if (predict.type == "prob") {
        expect_identical(predict.type, "prob")
        probs = c(1 / (1 + length(control)), 1 - 1 / (1 + length(control)))
        sapply(control, function(lvl) probs[1 + (target == lvl)])
      } else {
        expect_identical(predict.type, "response")
        target
      }
    }, convertfrom = "classif", predict.type.map = c(response = "response", prob = "response"))()


  expect_identical(getCPOPredictType(prob.from.response), c(response = "response", prob = "response"))
  expect_identical(getCPOPredictType(prob.from.response %>>% cpoPca()), c(response = "response", prob = "response"))
  expect_identical(getCPOPredictType(cpoScale() %>>% prob.from.response %>>% cpoPca()), c(response = "response", prob = "response"))

  expect_identical(getCPOPredictType(se.from.prob %>>% prob.from.response), c(response = "response", se = "response"))
  expect_identical(getCPOPredictType((se.from.prob %>>% cpoScale()) %>>% (prob.from.response %>>% cpoPca())), c(response = "response", se = "response"))

  tsk = bh.task %>>% (se.from.prob %>>% cpoScale()) %>>% (prob.from.response %>>% cpoPca())

  expect_identical(getCPOPredictType(retrafo(tsk)), c(response = "response", se = "response"))
  expect_identical(getCPOPredictType(inverter(tsk)), c(response = "response", se = "response"))

  rlist = as.list(retrafo(tsk))

  expect_identical(getCPOPredictType(rlist[[1]]), c(response = "response", se = "prob"))
  expect_identical(getCPOPredictType(rlist[[2]]), c(response = "response", prob = "prob", se = "se"))

  expect_identical(getCPOPredictType(rlist[[3]]), c(response = "response", prob = "response"))

  expect_identical(getCPOPredictType(rlist[[1]] %>>% rlist[[3]]), c(response = "response", se = "response"))


  ilist = as.list(inverter(bh.task %>>% se.remover %>>% (se.from.prob %>>% cpoScale()) %>>% (prob.from.response %>>% cpoPca())))

  expect_identical(getCPOPredictType((ilist[[1]] %>>% ilist[[2]]) %>>% ilist[[3]]), c(response = "response"))
  expect_identical(getCPOPredictType(ilist[[1]] %>>% (ilist[[2]] %>>% ilist[[3]])), c(response = "response"))


  expect_identical((ilist[[1]] %>>% (ilist[[2]] %>>% ilist[[3]]))$element$prev.predict.type, getCPOPredictType(ilist[[1]] %>>% ilist[[2]]))
  expect_identical(((ilist[[1]] %>>% ilist[[2]]) %>>% ilist[[3]])$element$prev.predict.type, getCPOPredictType(ilist[[1]] %>>% ilist[[2]]))

  expect_error(se.remover %>>% setCPOId(se.remover, "b"), "se.remover needs a predict.type being one of 'se'.*can only deliver.*response")

  expect_set_equal(intersect(getLearnerProperties(se.from.prob %>>% makeLearner("classif.logreg")), c("prob", "se")), "se")

  expect_set_equal(intersect(getLearnerProperties(se.from.prob %>>% makeLearner("classif.geoDA")), c("prob", "se")), character(0))

  expect_set_equal(intersect(getLearnerProperties(se.remover %>>% (se.from.prob %>>% makeLearner("classif.logreg"))), c("prob", "se")), character(0))

  expect_equal(getLearnerType(se.from.prob %>>% makeLearner("classif.logreg")), "regr")
  expect_equal(getLearnerType(prob.from.response %>>% makeLearner("classif.logreg")), "classif")

  expect_error(se.remover %>>% (se.from.prob %>>% makeLearner("classif.geoDA")), "for 'response' prediction, the Learner must have 'prob' prediction")

  expect_error(se.remover %>>% makeLearner("regr.ctree"), "for 'response' prediction, the Learner must have 'se' prediction")

  expect_equal(getLearnerPredictType(cpoScale() %>>% makeLearner("classif.logreg", predict.type = "prob")), "prob")
  expect_equal(getLearnerPredictType(se.from.prob %>>% makeLearner("classif.logreg", predict.type = "prob")), "response")

  inv = inverter(pid.task %>>% loud.prob.from.response)
  expect_output(invert(inv, getTaskData(pid.task, target.extra = TRUE)$target), "^response$")
  expect_output(invert(inv, getTaskData(pid.task, target.extra = TRUE)$target, predict.type = "prob"), "^prob$")

  inv2 = inverter(pid.task %>>% loud.prob.from.response %>>% loud.prob.from.response)
  expect_output(invert(inv2, getTaskData(pid.task, target.extra = TRUE)$target), "^response\nresponse$")
  expect_output(invert(inv2, getTaskData(pid.task, target.extra = TRUE)$target, predict.type = "prob"), "^response\nprob$")


  testinv = function(data, cpo, lrn, predict.type.lrn, predict.type.outer, measure) {
    lrn = setPredictType(lrn, predict.type.lrn)

    rd = makeResampleInstance("Holdout", data)

    train.set = subsetTask(data, rd$train.inds[[1]])
    test.set = subsetTask(data, rd$test.inds[[1]])

    mod = train(setPredictType(cpo %>>% lrn, predict.type.outer), train.set)
    prd = predict(mod, test.set)
    prd.df = predict(mod, newdata = getTaskData(test.set, target.extra = TRUE)$data)

    tmptrain = train.set %>>% cpo
    tmptst = test.set %>>% retrafo(tmptrain)
    mod2 = train(lrn, tmptrain)
    prd2 = predict(mod2, tmptst)
    tmptst.df = getTaskData(test.set, target.extra = TRUE)$data %>>% retrafo(tmptrain)
    prd2.df = predict(mod2, newdata = tmptst.df)

    expect_identical(invert(retrafo(tmptrain), prd2.df, predict.type.outer)$data, prd.df$data)

    expect_identical(invert(inverter(tmptst), prd2, predict.type.outer)$data, prd$data)

    expect_identical(invert(retrafo(tmptrain), prd2, predict.type.outer)$data$response, prd$data$response)

    expect_true(!is.na(holdout(setPredictType(cpo %>>% lrn, predict.type.outer), data, show.info = FALSE, measures = list(measure))$aggr))
  }

  testinv(bh.task, se.from.prob, makeLearner("classif.logreg"), "prob", "se", mse)
  testinv(bh.task, se.remover %>>% se.from.prob, makeLearner("classif.logreg"), "prob", "response", mse)
  testinv(bh.task, se.adder %>>% se.remover %>>% se.from.prob, makeLearner("classif.logreg"), "prob", "response", mse)
  testinv(pid.task, prob.from.response, makeLearner("classif.logreg"), "response", "prob", auc)


})

### target column related



test_that("change target names in targetbound target", {

  cpo.df5renamed = cpo.df5
  names(cpo.df5renamed)[1] = "xN1"

  cpo.df5reordered = cpo.df5[c(2:4, 1, 5:9)]
  cpo.df5reorderedtask = makeRegrTask("df5reord", cpo.df5reordered, "N1")

  cpo.df5.reord.renamed = cpo.df5renamed[c(2:4, 1, 5:9)]

  localenv = new.env()
  localenv$action = TRUE

  applyGMC("testtargetrename", TRUE, type = c("target", "target.extra"),
    convertfrom = "regr",
    retrafo = function(data, target, control, param) {
      if (localenv$action) {
        colnames(data) = gsub("^target\\.", "target.x", colnames(data))
      }
      data
    },
    applyfun = function(cpocon, type, line, dfx) {
      for (i in 1:2) {
        if (i == 1) {
          cpo = cpocon()
        } else {
          if (dfx == "task") {
            next
          }
          cpo = cpocon(affect.index = c(1, 3))
        }

        localenv$action = TRUE
        if (dfx == "df.all") {
          expect_error(cpo.df5r %>>% cpo, "did not contain target column N1.*change names or number.*dataformat")
          expect_error(cpo.df5reorderedtask %>>% cpo, "did not contain target column N1.*change names or number.*dataformat")
        } else {
          trans = cpo.df5r %>>% cpo
          expect_equal(getTaskData(trans), cpo.df5renamed)
          expect_equal(getTaskTargetNames(trans), "xN1")
          ntrans = cpo.df5r %>>% retrafo(trans)
          expect_equal(getTaskData(ntrans), cpo.df5renamed)
          expect_equal(getTaskTargetNames(ntrans), "xN1")
          expect_equal(clearRI(cpo.df5 %>>% retrafo(trans)), cpo.df5renamed)

          trans = cpo.df5reorderedtask %>>% cpo
          expect_equal(getTaskData(trans), cpo.df5.reord.renamed)
          expect_equal(getTaskData(cpo.df5reorderedtask %>>% retrafo(trans)), cpo.df5.reord.renamed)
          expect_equal(clearRI(cpo.df5reordered %>>% retrafo(trans)), cpo.df5.reord.renamed)
          expect_equal(clearRI(cpo.df5 %>>% retrafo(trans)), cpo.df5renamed)
          expect_equal(getTaskData(cpo.df5r %>>% retrafo(trans)), cpo.df5renamed)
          localenv$action = FALSE
          expect_error(cpo.df5r %>>% retrafo(trans), "column name mismatch|after retrafo differ.*after trafo.*N1.*xN1")
        }
      }
    })


  cpo.df4renamed = cpo.df4
  colnames(cpo.df4renamed) = gsub("T", "xT", colnames(cpo.df4renamed))
  cpo.df4renreord = cpo.df4renamed[c(1, 3, 4, 6:10, 2, 5)]
  cpo.df4ren2reord = cpo.df4renreord
  colnames(cpo.df4ren2reord) = gsub("T", "xT", colnames(cpo.df4renreord))
  cpo.df4half = cpo.df4[c(3, 6:10, 1, 2, 4, 5)]
  cpo.df4halfren = cpo.df4half
  colnames(cpo.df4halfren) = gsub("T", "xT", colnames(cpo.df4halfren))
  cpo.df4quarter = cpo.df4[c(3, 6:10, 1, 4, 2, 5)]
  cpo.df4quarterren2 = cpo.df4quarter
  colnames(cpo.df4quarterren2) = gsub("T", "xxT", colnames(cpo.df4quarterren2))

  cpo.df4plus = cbind(cpo.df4, xT1 = 1:3)
  cpo.df4plustask = makeMultilabelTask(data = cpo.df4plus, target = c("T1", "T2"))

  applyGMC("testtargetrename2", TRUE, type = c("target", "target.extra"),
    convertfrom = "multilabel",
    retrafo = function(data, target, control, param) {
      colnames(data) = gsub("^target\\.", "target.x", colnames(data))
      data
    },
    applyfun = function(cpocon, type, line, dfx) {
      for (i in 1:2) {
        if (i == 1) {
          cpo = cpocon()
          tasktarget = cpo.df4renamed
          tasktarget2 = cpo.df4ren2reord
        } else {
          cpo = cpocon(affect.index = c(1, 3))
          tasktarget = cpo.df4halfren
          tasktarget2 = cpo.df4quarterren2
        }
        if (dfx == "df.all") {
          expect_error(cpo.df4l %>>% cpo, "did not contain target columns T1, T2.*change names or number.*dataformat")
        } else {
          trans = cpo.df4l %>>% cpo
          expect_equal(getTaskTargetNames(trans), c("xT1", "xT2"))
          ntrans = cpo.df4l %>>% retrafo(trans)
          expect_equal(getTaskTargetNames(ntrans), c("xT1", "xT2"))
          if (dfx == "task") {
            expect_equal(getTaskData(trans), tasktarget)
            expect_equal(getTaskData(ntrans), tasktarget)
            expect_equal(clearRI(cpo.df4 %>>% retrafo(trans)), tasktarget)
            expect_equal(getTaskData(makeMultilabelTask(data = cpo.df4renreord, target = c("xT1", "xT2")) %>>% cpo), tasktarget2)
            expect_error(cpo.df4plustask %>>% cpo, "column names xT1 duplicated|with unique column names")
          } else {
            expect_equal(getTaskData(trans), cpo.df4renreord)
            expect_equal(getTaskData(ntrans), cpo.df4renreord)
            expect_equal(clearRI(cpo.df4 %>>% retrafo(trans)), cpo.df4renreord)
            expect_equal(getTaskData(makeMultilabelTask(data = cpo.df4renreord, target = c("xT1", "xT2")) %>>% cpo), cpo.df4ren2reord)
            expect_error(cpo.df4plustask %>>% cpo, "column names xT1 duplicated|introduced duplicate column names")
          }
        }
      }
    })

  cpo.df4x0 = cpo.df4[c(1, 3, 4, 6:10, 2, 5)]
  cpo.df4x1 = cbind(cpo.df4x0, T3 = FALSE)
  cpo.df4x2 = cbind(cpo.df4x0, T3 = FALSE, T4 = TRUE)
  cpo.df4x3 = cbind(cpo.df4x0, T3 = FALSE, T4 = TRUE, T5 = FALSE)
  cpo.df4half1 = cbind(cpo.df4half, T3 = FALSE)
  cpo.df4half2 = cbind(cpo.df4half, T3 = FALSE, T4 = TRUE)
  cpo.df4half3 = cbind(cpo.df4half, T3 = FALSE, T4 = TRUE, T5 = FALSE)
  cpo.df4quarter1 = cbind(cpo.df4quarter, T3 = FALSE)
  cpo.df4quarter2 = cbind(cpo.df4quarter, T3 = FALSE, T4 = TRUE)
  cpo.df4quarter3 = cbind(cpo.df4quarter, T3 = FALSE, T4 = TRUE, T5 = FALSE)
  cpo.df41 = cbind(cpo.df4, T3 = FALSE)
  cpo.df42 = cbind(cpo.df4, T3 = FALSE, T4 = TRUE)
  cpo.df43 = cbind(cpo.df4, T3 = FALSE, T4 = TRUE, T5 = FALSE)

  target3 = c("T1", "T2", "T3")
  target4 = c(target3, "T4")
  target5 = c(target4, "T5")

  applyGMC("testtargetrenumber", TRUE, type = c("target", "target.extra"),
    convertfrom = "multilabel",
    retrafo = function(data, target, control, param) {
      if (localenv$action) {
        for (i in seq_len(param) + 2) {
          newdf = data.frame(rep(i %% 2 == 0, 3))
          colnames(newdf) = paste0("target.T", i)
          data = cbind(data, newdf)
        }
      }
      data
    },
    applyfun = function(cpocon, type, line, dfx) {
      origcomp1 = cpo.df4x1
      origcomp2 = cpo.df4x2
      origcomp3 = cpo.df4x3
      for (i in 1:2) {
        if (i == 1) {
          cpo = cpocon()
          taskcmp1 = cpo.df41
          taskcmp2 = cpo.df42
          taskcmp3 = cpo.df43

        } else {
          cpo = cpocon(affect.index = c(1, 3))
          taskcmp1 = cpo.df4half1
          taskcmp2 = cpo.df4half2
          taskcmp3 = cpo.df4half3
          if (dfx == "task") {
            origcomp1 = cpo.df4quarter1
            origcomp2 = cpo.df4quarter2
            origcomp3 = cpo.df4quarter3
          }
        }
        localenv$action = TRUE
        if (dfx == "df.all") {
          expect_error(cpo.df4l %>>% cpo, "must not change non-target column names.")
        } else {
          expect_equal(getTaskData(cpo.df4l %>>% setHyperPars(cpo, testtargetrenumber.param = 0)), cpo.df4)
          comp = switch(dfx, task = taskcmp1, cpo.df4x1)
          trans = cpo.df4l %>>% cpo
          expect_equal(getTaskData(trans), comp)
          expect_equal(getTaskTargetNames(trans), target3)
          ntrans = cpo.df4l %>>% retrafo(trans)
          expect_equal(getTaskData(ntrans), comp)
          expect_equal(getTaskTargetNames(ntrans), target3)
          expect_equal(clearRI(cpo.df4 %>>% retrafo(trans)), comp)
          expect_equal(clearRI(cpo.df4x0 %>>% retrafo(trans)), origcomp1)
          comp = switch(dfx, task = taskcmp2, cpo.df4x2)
          trans = cpo.df4l %>>% setHyperPars(cpo, testtargetrenumber.param = 2)
          expect_equal(getTaskData(trans), comp)
          expect_equal(getTaskTargetNames(trans), target4)
          ntrans = cpo.df4l %>>% retrafo(trans)
          expect_equal(getTaskData(ntrans), comp)
          expect_equal(getTaskTargetNames(ntrans), target4)
          expect_equal(clearRI(cpo.df4 %>>% retrafo(trans)), comp)
          expect_equal(clearRI(cpo.df4x0 %>>% retrafo(trans)), origcomp2)
          comp = switch(dfx, task = taskcmp3, cpo.df4x3)
          trans = cpo.df4l %>>% setHyperPars(cpo, testtargetrenumber.param = 3)
          expect_equal(getTaskTargetNames(trans), target5)
          expect_equal(getTaskData(trans), comp)
          ntrans = cpo.df4l %>>% retrafo(trans)
          expect_equal(getTaskData(ntrans), comp)
          expect_equal(getTaskTargetNames(ntrans), target5)
          expect_equal(clearRI(cpo.df4 %>>% retrafo(trans)), comp)
          expect_equal(clearRI(cpo.df4x0 %>>% retrafo(trans)), origcomp3)

          localenv$action = FALSE
          expect_error(cpo.df4x0 %>>% retrafo(trans), "column name mismatch|after retrafo differ.*Was 'T1', 'T2', is now 'T1', 'T2', 'T3'.*")
        }
      }
    })

})

test_that("assertTask checks tasks", {

  at = function(x) assertTask(x, "tests", "nocpo")

  expect_error(at(1), "was not a task")
  expect_error(at(list(task.desc = list())), "was not a task")

  pt = pid.task
  pt$env = NULL
  expect_error(at(pt), "had no environment")

  pt = pid.task
  class(pt) %c=% "testtask"
  expect_error(at(pt), "must have classes ClassifTask, ")

  pt = pid.task
  class(pt$task.desc) %c=% "testtask"
  expect_error(at(pt), "must have classes ClassifTaskDesc, ")

  pt = pid.task
  pt$task.desc$id %c=% pt$task.desc$id
  expect_error(at(pt), "^[^;]*id must be a character[^;]*$")

  pt = subsetTask(pid.task)
  pt$env$data = list(1:10)
  expect_error(at(pt), "^[^;]*data must be a data.frame[^;]*$")

  pt = subsetTask(pid.task)
  colnames(pt$env$data)[1] = colnames(pt$env$data)[2]
  expect_error(at(pt), "^[^;]*data must be a data.frame[^;]*$")

  pt = pid.task
  pt$task.desc$target = list(1:10)
  expect_error(at(pt), "^[^;]*target must be a character[^;]*$")

  pt = pid.task
  pt$task.desc$n.feat = as.character(pt$task.desc$n.feat)
  expect_error(at(pt), "^[^;]*must have numeric 'n.feat'[^;]*$")

  pt = pid.task
  pt$task.desc$target = paste0("x", pt$task.desc$target)
  expect_error(at(pt), "target must be a subset of task columns")

  pt = pid.task
  pt$task.desc$n.feat["numerics"] %+=% 1
  expect_error(at(pt), "^[^;]*'numerics' features listed.*wrong[^;]*$")

  pt = pid.task
  pt$task.desc$n.feat["factors"] %+=% 1
  expect_error(at(pt), "^[^;]*'factors' features listed.*wrong[^;]*$")

  pt = pid.task
  pt$task.desc$n.feat["ordered"] %+=% 1
  expect_error(at(pt), "^[^;]*'ordered' features listed.*wrong[^;]*$")

  pt = subsetTask(pid.task)
  pt$env$data[[1]][1] = NA
  expect_error(at(pt), "^[^;]*has.missings' slot in task.desc[^;]*$")

  pt = subsetTask(pid.task)
  pt$env$data =   pt$env$data[-1, ]
  expect_error(at(pt), "^[^;]*size' slot in task.desc[^;]*$")

  pt = pid.task
  pt$task.desc$has.weights = TRUE
  expect_error(at(pt), "^[^;]*has.weights' slot in task.desc is wrong[^;]*$")

  pt = pid.task
  pt$task.desc$has.blocking = TRUE
  expect_error(at(pt), "^[^;]*'has.blocking' slot in task.desc is wrong[^;]*$")

  pt = pid.task
  pt$task.desc$type = "abc"
  expect_error(at(pt), "^[^;]*task type must be one of[^;]*$")

  pt = pid.task
  pt$type = "regr"
  expect_error(at(pt), "^[^;]*task type and task.desc type must be the same[^;]*$")

  pt = pid.task
  pt$task.desc$target %c=% "glucose"
  pt$task.desc$n.feat["numerics"] %-=% 1
  expect_error(at(pt), "^[^;]*classif but has 2 targets[^;]*$")

  pt = pid.task
  pt$task.desc$target = character(0)
  pt$task.desc$n.feat["factors"] %+=% 1
  expect_error(at(pt), "^[^;]*classif but has 0 targets[^;]*$")

  pt = yeast.task
  pt$task.desc$target = pt$task.desc$target[1]
  expect_error(at(pt), "^[^;]*multilabel and must have more than one target[^;]*$")

  pt = lung.task
  pt$task.desc$target = pt$task.desc$target[1]
  expect_error(at(pt), "^[^;]*surv and must have exactly two targets[^;]*$")


  pt = iris.task
  pt$task.desc$class.levels = c("x", "y", "z")
  expect_error(at(pt), "^[^;]*class levels in task.desc are not the factor levels of the target column[^;]*$")

  pt = iris.task
  pt$task.desc$negative = "setosa"
  expect_error(at(pt), "^[^;]*must be NA for multiclass tasks[^;]*$")

  pt = pid.task
  pt$task.desc$negative = "setosa"
  expect_error(at(pt), "^[^;]*must be both class levels of the target[^;]*$")

  pt = pid.task
  pt$task.desc$negative = pt$task.desc$positive
  expect_error(at(pt), "^[^;]*must be both class levels of the target[^;]*$")

  pt = subsetTask(pid.task)
  pt$env$data = droplevels(pt$env$data[1, ])
  pt$task.desc$size = 1
  pt$task.desc$class.levels = "pos"
  expect_error(at(pt), "^[^;]*must be the class level,.*must be not_<positive>[^;]*$")
  pt$task.desc$positive = "pos"
  pt$task.desc$negative = "not_pos"
  at(pt)

  pt = subsetTask(lung.task)
  pt$env$data$status = as.numeric(pt$env$data$status)
  expect_error(at(pt), "^[^;]*event column must be logical[^;]*$")

  pt = subsetTask(lung.task)
  pt$env$data$time = as.factor(pt$env$data$time)
  expect_error(at(pt), "^[^;]*time column must be numeric[^;]*$")

  pt = yeast.task
  pt$task.desc$class.levels = pt$task.desc$class.levels[1:4]
  expect_error(at(pt), "^[^;]*class.levels in task.desc must equal target names[^;]*$")

})

# interchanging names between target and nontarget column for retrafoless?

test_that("switching classif levels in targetbound switches positive", {

  localenv = new.env()
  localenv$action = TRUE

  cpo.df5rev = cpo.df5
  cpo.df5rev$F1 = factor(cpo.df5rev$F1, levels = rev(levels(cpo.df5rev$F1)))

  tid = getTaskId(cpo.df5c)

  cpo.df5c.ortho = cpo.df5c
  cpo.df5c.trans = makeClassifTask(id = tid, data = cpo.df5, target = "F1", positive = levels(cpo.df5$F1)[2])
  cpo.df5crev.ortho = makeClassifTask(id = tid, data = cpo.df5rev, target = "F1")
  cpo.df5crev.trans = makeClassifTask(id = tid, data = cpo.df5rev, target = "F1", positive = levels(cpo.df5rev$F1)[2])

  cpo.df5c.ortho
  cpo.df5c.trans
  cpo.df5crev.ortho
  cpo.df5crev.trans

  applyGMC("testtargetrename", TRUE, type = c("target", "target.extra"),
    convertfrom = "classif",
    retrafo = function(data, target, control, param) {
      expect_equal(levels(data[[target]])[1], localenv$top)
      if (localenv$action) {
        data[[target]] = factor(data[[target]], levels = rev(levels(data[[target]])))
      }
      data
    },
    applyfun = function(cpocon, type, line, dfx) {
      for (i in 1:2) {
        if (i == 1) {
          cpo = cpocon()
        } else {
          cpo = cpocon(affect.index = c(1, 3))
        }
        localenv$top = "a"
        trans = cpo.df5c.ortho %>>% cpo
        ntrans = cpo.df5c.ortho %>>% retrafo(trans)
        expect_equal(clearRI(trans), cpo.df5crev.ortho)
        expect_equal(clearRI(ntrans), cpo.df5crev.ortho)

        localenv$top = "b"
        ntrans = cpo.df5crev.ortho %>>% retrafo(trans)
        expect_equal(clearRI(ntrans), cpo.df5c.ortho)
        localenv$top = if (dfx == "task") "b" else "a"
        ntrans = cpo.df5crev.trans %>>% retrafo(trans)
        expect_equal(clearRI(ntrans), cpo.df5c.trans)
        localenv$top = if (dfx == "task") "a" else "b"
        ntrans = cpo.df5c.trans %>>% retrafo(trans)
        expect_equal(clearRI(ntrans), cpo.df5crev.trans)

        localenv$top = "b"
        ntrans = cpo.df5crev.ortho %>>% cpo
        expect_equal(clearRI(ntrans), cpo.df5c.ortho)
        localenv$top = if (dfx == "task") "b" else "a"
        ntrans = cpo.df5crev.trans %>>% cpo
        expect_equal(clearRI(ntrans), cpo.df5c.trans)
        localenv$top = if (dfx == "task") "a" else "b"
        ntrans = cpo.df5c.trans %>>% cpo
        expect_equal(clearRI(ntrans), cpo.df5crev.trans)
      }
    })

})

test_that("classif class names / number of classes changes", {

  localenv = new.env()
  localenv$action = 1


  tid = getTaskId(cpo.df5c)

  longdf5 = rbind(cpo.df5, cpo.df5, cpo.df5)
  longdf5ren = longdf5
  levels(longdf5ren$F1) = c("x", "y")
  longdf5add = longdf5
  longdf5add$F1 = factor(c("a", "b", "c", "a", "b", "c", "a", "b", "c"), levels = c("a", "b", "c"))

  longdf5c = makeClassifTask(id = tid, data = longdf5, target = "F1")
  longdf5renc = makeClassifTask(id = tid, data = longdf5ren, target = "F1")
  longdf5addc = makeClassifTask(id = tid, data = longdf5add, target = "F1")



  applyGMC("testtargetrename", TRUE, type = c("target", "target.extra"),
    convertfrom = "classif",
    retrafo = function(data, target, control, param) {
      if (localenv$action == 1) {
        levels(data[[target]]) = c("x", "y")
      } else if (localenv$action == 2) {
        data[[target]] = factor(c("a", "b", "c"), levels = c("a", "b", "c"))

      }
      data
    },
    properties.needed = "multiclass",
    applyfun = function(cpocon, type, line, dfx) {
      for (i in 1:2) {
        if (i == 1) {
          cpo = cpocon()
        } else {
          cpo = cpocon(affect.index = c(1, 3))
        }
        localenv$action = 0
        trans = longdf5c %>>% cpo
        ntrans = longdf5c %>>% retrafo(trans)
        expect_equal(clearRI(trans), longdf5c)
        expect_equal(clearRI(ntrans), longdf5c)

        localenv$action = 1
        trans = longdf5c %>>% cpo
        ntrans = longdf5c %>>% retrafo(trans)
        expect_equal(clearRI(trans), longdf5renc)
        expect_equal(clearRI(ntrans), longdf5renc)

        localenv$action = 2
        trans = longdf5c %>>% cpo
        ntrans = longdf5c %>>% retrafo(trans)
        expect_equal(clearRI(trans), longdf5addc)
        expect_equal(clearRI(ntrans), longdf5addc)

        localenv$action = 2
        trans = longdf5c %>>% cpo
        localenv$action = 0
        ntrans = longdf5c %>>% retrafo(trans)
        expect_equal(clearRI(trans), longdf5addc)
        expect_equal(clearRI(ntrans), longdf5c)
      }
    })

})

### conversion related

test_that("convert data.frame as if it were 'cluster'", {

  testClusterTarget = function(df, adding, type, doextra = FALSE) {

    incluster = makeClusterTask("[CPO CONSTRUCTED]", df)

    outdf = cbind(df, adding)
    outdfreord = cbind(df[c(-1, -3)], df[c(1, 3)], adding)

    outtask = constructTask(outdf, colnames(adding), type, "[CPO CONSTRUCTED]")
    outtaskreord = constructTask(outdfreord, colnames(adding), type, "[CPO CONSTRUCTED]")

    localenv = new.env()
    localenv$action = TRUE

    if (doextra) {
      applyGMC("clusterconvertfail", TRUE, type = c("target", "target.extra"),
        convertfrom = "cluster", convertto = "cluster",
        retrafo = function(data, target, control, param) {
          if (localenv$action) {
            colnames(adding) = paste0("target.", colnames(adding))
            cbind(data, adding)
          } else {
            data
          }
        },
        applyfun = function(cpocon, type, line, dfx) {
          for (i in 1:2) {
            if (i == 1) {
              cpo = cpocon()
            } else {
              cpo = cpocon(affect.index = c(1, 3))
            }
            localenv$action = TRUE
            expect_error(df %>>% cpo, if (dfx == "df.all") "must not change non-target column names" else "Cluster task cannot have target columns")
            localenv$action = FALSE
            trans = df %>>% cpo
            expect_equal(clearRI(trans), incluster)
            expect_equal(clearRI(df %>>% retrafo(trans)), df)
            expect_equal(clearRI(incluster %>>% retrafo(trans)), incluster)
            localenv$action = TRUE
            expect_error(df %>>% retrafo(trans), if (dfx == "df.all") "must not change non-target column names" else "Cluster task cannot have target columns")
            expect_error(incluster %>>% retrafo(trans), if (dfx == "df.all") "must not change non-target column names" else "Cluster task cannot have target columns")
          }
        })
    }

    applyGMC("clusterconvert", TRUE, type = c("target", "target.extra"),
      convertfrom = "cluster", convertto = type,
      retrafo = function(data, target, control, param) {
        if (localenv$action) {
          colnames(adding) = paste0("target.", colnames(adding))
          cbind(data, adding)
        } else {
          data
        }
      },
      applyfun = function(cpocon, type, line, dfx) {
        if (dfx == "df.all") return(NULL)
        for (i in 1:2) {
          dfexp = outdf
          taskexp = outtask
          if (i == 1) {
            cpo = cpocon()
          } else {
            cpo = cpocon(affect.index = c(1, 3))
            if (dfx == "task") {
              dfexp = outdfreord
              taskexp = outtaskreord
            }
          }
          localenv$action = TRUE

          trans = df %>>% cpo
          expect_equal(clearRI(trans), taskexp)
          expect_equal(clearRI(incluster %>>% retrafo(trans)), taskexp)
          expect_equal(clearRI(df %>>% retrafo(trans)), dfexp)

          trans = incluster %>>% cpo
          expect_equal(clearRI(trans), taskexp)
          expect_equal(clearRI(incluster %>>% retrafo(trans)), taskexp)
          expect_equal(clearRI(df %>>% retrafo(trans)), dfexp)

          localenv$action = FALSE
          expect_error(df %>>% cpo, "Assertion on 'target' failed")

          expect_error(df %>>% retrafo(trans), "Assertion on 'target' failed")
          expect_error(incluster %>>% retrafo(trans), "Assertion on 'target' failed")
        }
      })
  }

  testClusterTarget(cpo.df5, data.frame(xx = 1:3), "regr", TRUE)

  testClusterTarget(cpo.df5, data.frame(xx = c("a", "a", "b")), "classif")
  testClusterTarget(cpo.df5, data.frame(xx = c("a", "b", "c")), "classif")
  testClusterTarget(cpo.df5, data.frame(xx = 1:3, yy = c(TRUE, TRUE, FALSE)), "surv")
  testClusterTarget(cpo.df5, data.frame(xx = c(TRUE, FALSE, FALSE), yy = c(TRUE, TRUE, FALSE)), "multilabel")

})

test_that("tocpo respects properties", {

  localenv = new.env()
  localenv$action = TRUE

  cpo.df5x = cpo.df5
  cpo.df5x$F1 = factor(c("a", "b", "c"))
  cpo.df5xc = makeClassifTask(id = "cpo.df5", data = cpo.df5x, target = "F1")

  applyGMC("propcheck", TRUE,
    convertfrom = "classif",
    properties.target = c("classif", "twoclass"), properties.needed = character(0),
    retrafo = function(data, target, control, param) {
      if (localenv$action) {
        data[[target]] = factor(c("a", "b", "c"))
      }
      data
    },
    applyfun = function(cpocon, type, line, dfx) {
      cpo = cpocon()
      if (type %in% c("target", "target.extra")) {
        localenv$action = FALSE
        trans = cpo.df5c %>>% cpo
        expect_equal(clearRI(trans), cpo.df5c)
        localenv$action = TRUE
        expect_error(cpo.df5c %>>% cpo, "has property multiclass that propcheck.*properties.needed")
        expect_error(cpo.df5c %>>% retrafo(trans), "has property multiclass that propcheck.*properties.needed")
      }
      expect_error(cpo.df5xc %>>% cpo, "has property multiclass that propcheck can not handle")
    })

  applyGMC("propcheck", TRUE, type = c("target", "target.extra"),
    convertfrom = "classif",
    properties.target = c("classif", "twoclass"), properties.needed = "multiclass",
    properties.adding = "twoclass",
    retrafo = function(data, target, control, param) {
      if (localenv$action) {
        data[[target]] = factor(c("a", "b", "c"))
      }
      data
    },
    applyfun = function(cpocon, type, line, dfx) {
      cpo = cpocon()
      localenv$action = TRUE
      trans = cpo.df5c %>>% cpo
      expect_equal(clearRI(trans), cpo.df5xc)
      localenv$action = FALSE
      expect_error(cpo.df5c %>>% cpo, "has property twoclass that propcheck.*properties.adding")
      expect_error(cpo.df5c %>>% retrafo(trans), "has property twoclass that propcheck.*properties.adding")
    })

})


### other tocpo

test_that("tocpo sees the correct cols when using affect args", {

  expectedcols = getTaskData(cpo.df4l, target.extra = TRUE)$data

  applyGMC("affectcheck", TRUE,
    retrafo = function(data, target, control, param) {
      if (!missing(target)) {
        dat2 = dropNamed(data, target)
      } else {
        dat2 = data
      }
      expect_identical(dat2[[1]], expectedcols[[param]])
      data
    },
    convertfrom = "multilabel",
    applyfun = function(cpocon, type, line, dfx) {
      if (type == "retrafoless") return(NULL)
      for (i in seq_along(expectedcols)) {
        cpo = cpocon(i, affect.index = i)
        trans = cpo.df4l %>>% cpo
        expect_equal(clearRI(trans), cpo.df4l)
        expect_equal(clearRI(cpo.df4l %>>% retrafo(trans)), cpo.df4l)
        expect_equal(clearRI(cpo.df4 %>>% retrafo(trans)), cpo.df4)
      }
    })
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
