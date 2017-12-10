context("general operation CPOs")


test_that("generalMakeCPO works", {

  expect_class(regr.num.task %>>% generalMakeCPO("test",
      train = function(data, target, param) {
        NULL
      },
      retrafo = function(data, control, param) {
        data
      })(2), "Task")


  localenv = new.env()

  # test that all generalMakeCPO with all options give the data in the same format.
  # strict == whether to separate factors and ordereds
  testDataInputFormat = function(task, strict = FALSE) {
    truetarget = getTaskData(task, features = character(0))
    if (length(truetarget) == 0) {
      truetarget$pred = 1L
    }


    task %>>% generalMakeCPO("setup", keepformat = FALSE,
      dataformat.factor.with.ordered = !strict,
      train = function(data, target, param) {
        localenv$withtarget.split = data
      },
      retrafo = function(data, control, param) {
        localenv$notarget.split = data
        data
      })()

    task %>>% generalMakeCPO("setup2", keepformat = TRUE,
      dataformat.factor.with.ordered = !strict,
      train = function(data, target, param) {
        localenv$withtarget = data
      },
      retrafo = function(data, control, param) {
        localenv$notarget = data
        data
      })()


    dataformats = c("df.features", "split", "df.all", "task")
    present = names(Filter(identity, getTaskDesc(task)$n.feat))
    if (!strict && "ordered" %in% present) {
      present = c("factors", setdiff(present, "ordered"))
    }

    if (length(present) == 1) {
      dataformats %c=% switch(present, numerics = "numeric", factors = "factor", present)
    }

    #expected.target = names(localenv$withtarget[grepl("^target\\.", names(localenv$withtarget))])
    #expected.target = gsub("^target\\.", "", names(expected.target))
    expected.target = getTaskTargetNames(task)
    if (length(expected.target)) {
      expected.target = paste0("target.", expected.target)
    }


    checkFulldata = function(data, target) {
      if (curdf == "split") {
        comparendum = localenv$withtarget.split
      } else {
        comparendum = localenv$withtarget
      }
      if (curdf %in% c("df.all", "task")) {
        expect_equal(length(comparendum), length(data))
        data = data[colnames(comparendum)]
      }
      expect_identical(data, comparendum)
      expect_identical(expected.target, target)
    }

    checkHalfdata = function(data) {
      if (curdf == "split") {
        comparendum = localenv$notarget.split
      } else {
        comparendum = localenv$notarget
      }
      expect_identical(data, comparendum)
    }

    cursl = FALSE
    curtype = "simple"
    isfocpo = FALSE
    curdf = "df.features"

    doingdf = FALSE


    applyGMC("compare", strict, convertfrom = getTaskDesc(task)$type,
      train = function(data, target, param) {
        if (cursl && curtype != "target" || doingdf) {
          checkHalfdata(data)
        } else {
          checkFulldata(data, target)
        }
        TRUE
      },
      retrafo = function(data, control, param, target) {
        if (!isfocpo && !doingdf) {
          checkFulldata(data, target)
        } else {
          assert(missing(target))
          checkHalfdata(data)
        }
        data
      },
      traininvert = function(data, control, param) {
        checkHalfdata(data)
        TRUE
      },
      invert = function(target, predict.type, control, param) {
        expect_identical(target[[1]], truetarget[[1]])
        target
      }, applyfun = function(cpocon, type, line, dfx) {
        doingdf <<- FALSE  # nolint
        cursl <<- line$sl  # nolint
        curtype <<- type  # nolint
        isfocpo <<- type %in% c("simple", "extended")  # nolint
        curdf <<- dfx  # nolint
        istocpo = type %in% c("target", "target.extended")
        cpo = cpocon()
        ret = retrafo(task %>>% cpo)
        inv = inverter(task %>>% ret)
        expect_equal(getCPOTrainedCapability(ret)["invert"], c(invert = istocpo * ifelse(line$ci, 1, -1)))
        if (line$ci) {
          invert(ret, truetarget)
        }
        invert(inv, truetarget)
        if (getTaskDesc(task)$type != "cluster") {
          # if it is cluster, we check below.
          getTaskData(task) %>>% ret
        }

        doingdf <<- TRUE  # nolint
        getTaskData(task, target.extra = TRUE)$data %>>% ret
        doingdf <<- FALSE  # nolint
      })
  }


  testDataInputFormat(subsetTask(multiclass.task, c(1, 51, 101)), FALSE)
  testDataInputFormat(subsetTask(multiclass.task, c(1, 51, 101)), TRUE)
  testDataInputFormat(cpo.df1c)
  testDataInputFormat(cpo.df1cc, FALSE)
  testDataInputFormat(cpo.df1cc, TRUE)
  testDataInputFormat(cpo.df3l)
  testDataInputFormat(cpo.df4l)
  testDataInputFormat(cpo.df4l2)
  testDataInputFormat(cpo.df5r, FALSE)
  testDataInputFormat(cpo.df5r, TRUE)

})

test_that("cpo.trafo has expected effects", {

  checkTrafo = function(task, strict = FALSE) {

    barrelroll = c(seq_len(getTaskSize(task))[-1], 1)

    is.retrafoless = FALSE
    posname = NULL

    applyGMC("testtrafo", strict, convertfrom = getTaskDesc(task)$type,
      retrafo = function(data, control, param, target) {
        if (is.retrafoless) {
          return(data[barrelroll, ])
        }
        if (missing(target)) {
          # focpo
          pos = which(gsub("^[^.]*\\.", "", colnames(data)) == posname)
        } else {
          # tocpo
          pos = target
        }
        if (length(pos)) {
          pos = pos[1]
          data[[pos]] = data[[pos]][barrelroll]
        }
        data
      }, applyfun = function(cpocon, type, line, dfx) {
        is.retrafoless <<- type == "retrafoless"  # nolint

        isfocpo = type %in% c("simple", "extended")
        exp1 = getTaskData(task)
        if (is.retrafoless) {
          exp1 = exp1[barrelroll, ]
          exp2 = exp1[barrelroll, ]
        } else {
          if (isfocpo) {
            posn = min(which(!colnames(exp1) %in% getTaskTargetNames(task)))
            posname <<- colnames(exp1)[posn]  # nolint
          } else {
            exp1 = getTaskData(task)
            posn = getTaskTargetNames(task)[1]
          }
          if (length(posn)) {
            exp1[[posn]] = exp1[[posn]][barrelroll]
            exp2 = exp1
            exp2[[posn]] = exp1[[posn]][barrelroll]
          } else {
            exp2 = exp1
          }
        }

        cpo = cpocon()
        ret = task %>>% cpo
        expect_identical(getTaskData(ret), exp1)
        if (!is.retrafoless) {
          expect_identical(getTaskData(task %>>% retrafo(ret)), exp1)
        }

        ret = task %>>% cpo %>>% cpo

        expect_identical(getTaskData(ret), exp2)
        if (!is.retrafoless) {
          expect_identical(getTaskData(task %>>% retrafo(ret)), exp2)
        }
      })
  }

  checkTrafoColChangeErrors = function(task, strict = FALSE) {

    barrelroll = c(seq_len(getTaskSize(task))[-1], 1)

    action = 1

    applyGMC("testtrafofail", strict, convertfrom = getTaskDesc(task)$type,
      type = c("target", "target.extended", "retrafoless"),
      dataformats = c("df.all", "task"),
      retrafo = function(data, control, param, target) {
        if (action == 4) {
          return(rbind(data, data))
        }
        if (action == 3) {
          colnames(data)[colnames(data) %in% target] = rev(target)
          return(data)
        }
        if (action == 2) {
          return(cbind(data[-1], data[1]))
        }
        if (action == 0) {
          return(data)
        }
        if (length(target)) {
          pos = which(!colnames(data) %in% target)[1]
        } else {
          pos = 1
        }

        data[[pos]] = data[[pos]][barrelroll]
        data
      }, applyfun = function(cpocon, type, line, dfx) {
        is.retrafoless = type == "retrafoless"
        action <<- 1 + is.retrafoless  # nolint
        cpo = cpocon()
        if (is.retrafoless) {
          expect_error(task %>>% cpo, "columns may not be changed")
          expect_error(getTaskData(task, target.extra = TRUE)$data %>>% cpo, "columns may not be changed")
          if (length(getTaskTargetNames(task)) > 1) {
            action <<- 3  # nolint
            expect_error(task %>>% cpo, "must not change target names")
          }
          action <<- 4  # nolint
          task %>>% cpo
        } else {
          expect_error(task %>>% cpo, "must not change non-target columns")
          action <<- 0  # nolint
          ret = retrafo(task %>>% cpo)

          action <<- 1  # nolint
          expect_error(task %>>% ret, "must not change non-target columns")
          if (getTaskDesc(task)$type != "cluster") {
            expect_error(getTaskData(task) %>>% ret, "must not change non-target columns")
          }
          action <<- 4  # nolint
          expect_error(task %>>% cpo, "must not change number of rows")
        }
      })
  }


  checkTrafo(subsetTask(multiclass.task, c(1, 51, 101)), FALSE)
  checkTrafo(subsetTask(multiclass.task, c(1, 51, 101)), TRUE)
  checkTrafo(cpo.df1c)
  checkTrafo(cpo.df1cc, FALSE)
  checkTrafo(cpo.df1cc, TRUE)
  checkTrafo(cpo.df3l)
  checkTrafo(cpo.df4l)
  checkTrafo(cpo.df4l2)
  checkTrafo(cpo.df5r, FALSE)
  checkTrafo(cpo.df5r, TRUE)

  checkTrafoColChangeErrors(subsetTask(multiclass.task, c(1, 51, 101)), FALSE)
  checkTrafoColChangeErrors(subsetTask(multiclass.task, c(1, 51, 101)), TRUE)
  checkTrafoColChangeErrors(cpo.df1c)
  checkTrafoColChangeErrors(cpo.df1cc, FALSE)
  checkTrafoColChangeErrors(cpo.df1cc, TRUE)
  checkTrafoColChangeErrors(cpo.df3l)
  checkTrafoColChangeErrors(cpo.df4l)
  checkTrafoColChangeErrors(cpo.df4l2)
  checkTrafoColChangeErrors(subsetTask(regr.num.task, 1:10), FALSE)
  checkTrafoColChangeErrors(subsetTask(regr.num.task, 1:10), TRUE)

})

test_that("information from cpo.trafo propagates", {

  localenv = environment()

  t1 = cpo.df1c
  t1exp = getTaskData(t1)[[1]][1]
  t2 = subsetTask(cpo.df1c, 2:3)
  t2exp = getTaskData(t2)[[1]][1]
  t3 = subsetTask(cpo.df1c, 3)
  t3exp = getTaskData(t3)[[1]][1]

  expect_false(t1exp == t2exp)
  expect_false(t2exp == t3exp)
  expect_false(t1exp == t3exp)

  applyGMC("testtrafo", TRUE, convertfrom = getTaskDesc(t1)$type,
    train = function(data, target, param) {
      data[[1]][1]
    },
    retrafo = function(data, control, param, target) {
      expect_identical(control, localenv$expected)
      data
    },
    traininvert = function(data, control, param) {
      expect_identical(control, localenv$expected)
      control
    },
    applyfun = function(cpocon, type, line, dfx) {
      cpo = cpocon()
      localenv$expected = t1exp
      ret = retrafo(t1 %>>% cpo)

      if (line$sl) {
        localenv$expected = t2exp
      } else {
        localenv$expected = t1exp
      }
      t2 %>>% ret
      if (line$sl) {
        localenv$expected = t3exp
      } else {
        localenv$expected = t1exp
      }
      getTaskData(t3) %>>% ret
    })


})

test_that("cpo.invert has information from cpo.trafo / cpo.retrafo", {

  localenv = environment()

  t1 = cpo.df1c
  t1exp = getTaskData(t1)[[1]][1]
  t2 = subsetTask(cpo.df1c, 2:3)
  t2exp = getTaskData(t2)[[1]][1]
  t3 = subsetTask(cpo.df1c, 3)
  t3exp = getTaskData(t3)[[1]][1]

  trg = getTaskData(t1, target.extra = TRUE)$target

  expect_false(t1exp == t2exp)
  expect_false(t2exp == t3exp)
  expect_false(t1exp == t3exp)

  applyGMC("testtrafo", TRUE, convertfrom = getTaskDesc(t1)$type,
    type = c("target", "target.extra"),
    traininvert = function(data, control, param) {
      data[[1]][1]
    },
    invert = function(target, predict.type, control, param) {
      expect_identical(control, localenv$expected)
      target
    },
    applyfun = function(cpocon, type, line, dfx) {
      if (line$sl && line$ci) {
        return(NULL)
      }
      cpo = cpocon()
      localenv$expected = t1exp
      init = t1 %>>% cpo
      ret = retrafo(init)

      inv0 = inverter(init)

      inv1 = inverter(t2 %>>% ret)

      inv2 = inverter(getTaskData(t3) %>>% ret)

      localenv$expected = t1exp
      if (line$ci) {
        invert(ret, trg)
      } else {
        expect_error(invert(ret, trg), "data-dependent inverter")
      }

      invert(inv0, trg)

      if (line$ci) {
        localenv$expected = t1exp
      } else {
        localenv$expected = t2exp
      }
      invert(inv1, trg)

      if (line$ci) {
        localenv$expected = t1exp
      } else {
        localenv$expected = t3exp
      }
      invert(inv2, trg)
    })
})

test_that("presence of control object is checked", {

  cpo = makeCPOTargetOp("errorCPO.target.fni",
    properties.target = c("classif", "twoclass"),
    cpo.train = { }, cpo.retrafo = { target }, cpo.train.invert = { function() NULL }, cpo.invert = NULL)

  expect_error(cpo.df1c %>>% cpo(), "cpo.invert as created by cpo.train.invert")

  cpo = makeCPOTargetOp("errorCPO.target.fri",
    properties.target = c("classif", "twoclass"),
    cpo.train = { cpo.train.invert = function(...) NULL }, cpo.retrafo = NULL, cpo.train.invert = NULL, cpo.invert = function(...) NULL)

  expect_error(cpo.df1c %>>% cpo(), "did not create.*cpo.retrafo")

  cpo = makeCPOTargetOp("errorCPO.target.fri",
    properties.target = c("classif", "twoclass"),
    cpo.train = { cpo.retrafo = function(...) NULL }, cpo.retrafo = NULL, cpo.train.invert = NULL, cpo.invert = function(...) NULL)

  expect_error(cpo.df1c %>>% cpo(), "did not create.*cpo.train.invert")

  cpo = makeCPOTargetOp("errorCPO.target.fri",
    properties.target = c("classif", "twoclass"),
    cpo.train = { cpo.retrafo = function() NULL; cpo.train.invert = function(...) NULL }, cpo.retrafo = NULL, cpo.train.invert = NULL, cpo.invert = function(...) NULL)

  expect_error(cpo.df1c %>>% cpo(), "cpo.retrafo as created by cpo.train")

  cpo = makeCPOTargetOp("errorCPO.target.fri",
    properties.target = c("classif", "twoclass"), constant.invert = TRUE,
    cpo.train = { cpo.retrafo = function(...) NULL; cpo.train.invert = function() NULL }, cpo.retrafo = NULL, cpo.train.invert = NULL, cpo.invert = NULL)

  expect_error(cpo.df1c %>>% cpo(), "cpo.train did not create.*cpo.invert")


  cpo = makeCPOTargetOp("errorCPO.target.fri",
    properties.target = c("classif", "twoclass"), constant.invert = TRUE,
    cpo.train = { cpo.retrafo = function(...) NULL; cpo.invert = function() NULL }, cpo.retrafo = NULL, cpo.train.invert = NULL, cpo.invert = NULL)

  expect_error(cpo.df1c %>>% cpo(), "cpo.invert as created by cpo.train")

  cpo = makeCPOExtendedTargetOp("errorCPO.extended.target",
    properties.target = c("classif", "twoclass"),
    cpo.trafo = {
      control = NULL
      target
    }, cpo.retrafo = {
      control.invert = NULL
      target
    }, cpo.invert = { target })

  expect_error(cpo.df1c %>>% cpo(), "did not create.*control.invert")

  cpo = makeCPOExtendedTargetOp("errorCPO.extended.target",
    properties.target = c("classif", "twoclass"),
    cpo.trafo = {
      control.invert = NULL
      target
    }, cpo.retrafo = {
      control.invert = NULL
      target
    }, cpo.invert = { target })

  expect_error(cpo.df1c %>>% cpo(), "did not create.*control'")

  cpo = makeCPOExtendedTargetOp("errorCPO.extended.target",
    properties.target = c("classif", "twoclass"),
    cpo.trafo = {
      control.invert = NULL
      control = NULL
      target
    }, cpo.retrafo = {
      target
    }, cpo.invert = { target })

  expect_error(cpo.df1c %>>% retrafo(cpo.df1c %>>% cpo()), "did not create.*control.invert")

  cpo = makeCPOExtendedTargetOp("errorCPO.extended.target",
    properties.target = c("classif", "twoclass"),
    cpo.trafo = {
      control.invert = NULL
      control = NULL
      target
    }, cpo.retrafo = {
      cpo.invert = function(...) NULL
      target
    }, cpo.invert = NULL)

  expect_error(cpo.df1c %>>% cpo(), "did not create.*cpo.invert")

  cpo = makeCPOExtendedTargetOp("errorCPO.extended.target",
    properties.target = c("classif", "twoclass"),
    cpo.trafo = {
      cpo.invert = function() NULL
      control = NULL
      target
    }, cpo.retrafo = {
      cpo.invert = function(...) NULL
      target
    }, cpo.invert = NULL)

  expect_error(cpo.df1c %>>% cpo(), "cpo.invert as created by cpo.trafo")


  cpo = makeCPOExtendedTargetOp("errorCPO.extended.target",
    properties.target = c("classif", "twoclass"),
    cpo.trafo = {
      cpo.invert = function(...) NULL
      control = NULL
      target
    }, cpo.retrafo = {
      cpo.invert = function() NULL
      target
    }, cpo.invert = NULL)

  expect_error(cpo.df1c %>>% retrafo(cpo.df1c %>>% cpo()), "cpo.invert as created by cpo.retrafo")

  cpo = makeCPOExtendedTargetOp("errorCPO.extended.target",
    properties.target = c("classif", "twoclass"),
    cpo.trafo = {
      cpo.invert = function(...) NULL
      control = NULL
      target
    }, cpo.retrafo = NULL, cpo.invert = NULL)

  expect_error(cpo.df1c %>>% retrafo(cpo.df1c %>>% cpo()), "cpo.trafo did not create .*cpo.retrafo")

  cpo = makeCPOExtendedTargetOp("errorCPO.extended.target",
    properties.target = c("classif", "twoclass"),
    cpo.trafo = {
      cpo.invert = function(...) NULL
      cpo.retrafo = function() NULL
      target
    }, cpo.retrafo = NULL, cpo.invert = NULL)

  expect_error(cpo.df1c %>>% retrafo(cpo.df1c %>>% cpo()), "cpo.retrafo as created by cpo.trafo")

})

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
          expect_equal(getTaskData(cpo.df5r %>>% retrafo(trans)), cpo.df5renamed)
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
          if (dfx == "task") {
            expect_equal(getTaskData(trans), tasktarget)
            expect_equal(getTaskData(cpo.df4l %>>% retrafo(trans)), tasktarget)
            expect_equal(clearRI(cpo.df4 %>>% retrafo(trans)), tasktarget)
            expect_equal(getTaskData(makeMultilabelTask(data = cpo.df4renreord, target = c("xT1", "xT2")) %>>% cpo), tasktarget2)
          } else {
            expect_equal(getTaskData(trans), cpo.df4renreord)
            expect_equal(getTaskData(cpo.df4l %>>% retrafo(trans)), cpo.df4renreord)
            expect_equal(clearRI(cpo.df4 %>>% retrafo(trans)), cpo.df4renreord)
            expect_equal(getTaskData(makeMultilabelTask(data = cpo.df4renreord, target = c("xT1", "xT2")) %>>% cpo), cpo.df4ren2reord)
          }
          cpo.df4plustask %>>% cpo

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
          expect_equal(getTaskData(cpo.df4l %>>% retrafo(trans)), comp)
          expect_equal(clearRI(cpo.df4 %>>% retrafo(trans)), comp)
          expect_equal(clearRI(cpo.df4x0 %>>% retrafo(trans)), origcomp1)
          comp = switch(dfx, task = taskcmp2, cpo.df4x2)
          trans = cpo.df4l %>>% setHyperPars(cpo, testtargetrenumber.param = 2)
          expect_equal(getTaskData(trans), comp)
          expect_equal(getTaskData(cpo.df4l %>>% retrafo(trans)), comp)
          expect_equal(clearRI(cpo.df4 %>>% retrafo(trans)), comp)
          expect_equal(clearRI(cpo.df4x0 %>>% retrafo(trans)), origcomp2)
          comp = switch(dfx, task = taskcmp3, cpo.df4x3)
          trans = cpo.df4l %>>% setHyperPars(cpo, testtargetrenumber.param = 3)
          expect_equal(getTaskData(trans), comp)
          expect_equal(getTaskData(cpo.df4l %>>% retrafo(trans)), comp)
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
  expect_error(at(pt), "^[^;]*has.missings slot in task.desc[^;]*$")

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

test_that("changing target names to clash gives error", {

})

# interchanging names between target and nontarget column for retrafoless?

test_that("switching classif levels in targetbound switches positive", {

})

test_that("targetbound changes data in a different way on 'retrafo' fails", {

})

test_that("classif number of classes changes", {

})

test_that("classif class names change", {

})

### conversion related

test_that("convert data.frame as if it were 'cluster'", {

})

test_that("properties.target is respected", {

})


### other tocpo

test_that("tocpo sees the correct cols when using affect args", {

})

### other features

test_that("missings tolerated in retrafo in certain conditions", {

})

test_that("dataformat.factor.with.ordered influences strictness of property presence check", {

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

test_that("index reference in affect and cpoSelect is relative to data cols", {

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

test_that("printers", {

})

test_that("NULLCPO", {

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
  expect_identical(getCPOAffect(NULLCPO), list())
  expect_identical(getCPOAffect(NULLCPO, FALSE), list())
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
})

test_that("inverter is noop when no targetbounds", {
  expect_identical(invert(inverter(bh.task %>>% cpoPca() %>>% cpoScale()), 1:10), 1:10)
  expect_identical(invert(inverter(bh.task %>>% cpoPca() %>>% cpoScale()), c("a", "b", "c")), c("a", "b", "c"))
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

