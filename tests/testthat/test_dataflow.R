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

test_that("composing CPOTrained with conversion etc. behaves as expected", {



  # predict.type map
})

test_that("'sometimes'-properties work as expected", {


})


test_that("tocpo sees the correct cols when using affect args", {

})

test_that("changing target names to clash gives error", {

})

test_that("targetbound changes data in a different way on 'retrafo' fails", {

})


test_that("target stays in its position unless names changed", {

})

test_that("properties.target is respected", {


})


test_that("getters and setters", {


})

test_that("operators", {

  cons = generalMakeCPO("test", type = "target")
  cpo = cons()
  comb = cons() %>>% cons(id = "2nd")
  ret = retrafo(bh.task %>>% cpo)
  combret = ret %>>% ret
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


})

test_that("printers", {


})

test_that("helpers", {
  # is.*
  # nulltonullcpo etc


})



test_that("NULLCPO", {

})

test_that("targetbound type conversion", {
# ("classif", "multilabel", "regr", "surv", "cluster", "costsens")

})


test_that("'bound' well behaved: after splitting, uniting; for trafos and retrafos", {

})

test_that("change task names in targetbound datasplit 'no' fails", {

})

test_that("change task names in targetbound target", {


})

test_that("switching classif levels in targetbound switches positive", {

})

test_that("targetbound functional", {

})

test_that("invert() works", {

})

test_that("inverter is noop when no targetbounds", {

})

test_that("truth is kept", {

})

test_that("classif number of classes changes", {

})

test_that("classif class names change", {

})

test_that("convert data.frame as if it were 'cluster'", {

})

test_that("incompatibility of cpo prediction and predict type detected", {

})

test_that("chaining inverters with incompatible conversion gives error", {

})

test_that("no complaint about missing 'control' in stateless cpo", {

})

test_that("after attaching CPO, predict.type stays the same if possible", {

})

test_that("chaining retrafo to learner that doesn't support the predict.type it needs fails", {

})

test_that("cpo.trafo-less CPOs, must be stateless", {

})

test_that("predict.type map works as expected", {


})
test_that("dataformat.factor.with.ordered influences strictness of property presence check", {

})

test_that("index reference in affect and cpoSelect is relative to data cols", {

})

test_that("missings tolerated in retrafo in certain conditions", {

})

test_that("attached learner properties change with tocpo", {

})

test_that("new operators work", {

})

test_that("convertNamesToItems etc", {
  # convertNamesToItems
  # convertItemsToNames
})

test_that("on.par.out.of.bounds respected", {

  # also with convertItemsToNames etc.
})


# interchanging names between target and nontarget column for retrafoless?



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

