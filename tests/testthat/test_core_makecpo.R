context("CPO constructors")


test_that("generalMakeCPO works", {

  testthat::skip_on_cran()

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
        if (!isfocpo && (!doingdf || getTaskType(task) == "cluster")) {
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
          expect_identical(clearRI(getTaskData(task) %>>% retrafo(ret)), exp1)
          expect_identical(clearRI(getTaskData(task, target.extra = TRUE)$data %>>% retrafo(ret)), dropNamed(exp1, getTaskTargetNames(task)))
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
  checkTrafoColChangeErrors(subsetTask(multiclass.task, c(1, 51, 101)), FALSE)
  checkTrafoColChangeErrors(subsetTask(multiclass.task, c(1, 51, 101)), TRUE)

  testthat::skip_on_cran()

  checkTrafo(cpo.df1c)
  checkTrafo(cpo.df1cc, FALSE)
  checkTrafo(cpo.df1cc, TRUE)
  checkTrafo(cpo.df3l)
  checkTrafo(cpo.df4l)
  checkTrafo(cpo.df4l2)
  checkTrafo(cpo.df5r, FALSE)
  checkTrafo(cpo.df5r, TRUE)

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
    type = c("target", "target.extended"),
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

  expect_error(cpo.df1c %>>% cpo(), "cpo.invert as created by cpo.train.invert does not have \\(only\\) the required")

  cpo = makeCPOTargetOp("errorCPO.target.fri",
    properties.target = c("classif", "twoclass"),
    cpo.train = { cpo.train.invert = function(...) NULL }, cpo.retrafo = NULL, cpo.train.invert = NULL, cpo.invert = function(...) NULL)

  expect_error(cpo.df1c %>>% cpo(), "did not create.*cpo.retrafo.*object")

  cpo = makeCPOTargetOp("errorCPO.target.fri",
    properties.target = c("classif", "twoclass"),
    cpo.train = { cpo.retrafo = NULL; cpo.train.invert = function(...) NULL }, cpo.retrafo = NULL, cpo.train.invert = NULL, cpo.invert = function(...) NULL)

  expect_error(cpo.df1c %>>% cpo(), "did not create.*cpo.retrafo function")

  cpo = makeCPOTargetOp("errorCPO.target.fri",
    properties.target = c("classif", "twoclass"),
    cpo.train = { cpo.retrafo = 1; cpo.train.invert = function(...) NULL }, cpo.retrafo = NULL, cpo.train.invert = NULL, cpo.invert = function(...) NULL)

  expect_error(cpo.df1c %>>% cpo(), "cpo.retrafo.*must be a function")

  cpo = makeCPOTargetOp("errorCPO.target.fri",
    properties.target = c("classif", "twoclass"),
    cpo.train = { cpo.retrafo = function(target, ...) target; cpo.train.invert = function(a, ...) NULL }, cpo.retrafo = NULL, cpo.train.invert = NULL, cpo.invert = function(...) NULL)

  expect_class(cpo.df1c %>>% cpo(), "Task")

  cpo = makeCPOTargetOp("errorCPO.target.fri",
    properties.target = c("classif", "twoclass"),
    cpo.train = { cpo.retrafo = function(target, ...) target; cpo.train.invert = function(a, b) NULL }, cpo.retrafo = NULL, cpo.train.invert = NULL, cpo.invert = function(...) NULL)

  expect_error(cpo.df1c %>>% cpo(), "cpo.train.invert.*must have exactly one argument")

  cpo = makeCPOTargetOp("errorCPO.target.fri",
    properties.target = c("classif", "twoclass"),
    cpo.train = { cpo.retrafo = function(...) NULL }, cpo.retrafo = NULL, cpo.train.invert = NULL, cpo.invert = function(...) NULL)

  expect_error(cpo.df1c %>>% cpo(), "did not create.*cpo.train.invert")

  cpo = makeCPOTargetOp("errorCPO.target.fri",
    properties.target = c("classif", "twoclass"),
    cpo.train = { cpo.retrafo = function() NULL; cpo.train.invert = function(...) NULL }, cpo.retrafo = NULL, cpo.train.invert = NULL, cpo.invert = function(...) NULL)

  expect_error(cpo.df1c %>>% cpo(), "cpo.retrafo as created by cpo.train does not have \\(only\\) the required")

  cpo = makeCPOTargetOp("errorCPO.target.fri",
    properties.target = c("classif", "twoclass"), constant.invert = TRUE,
    cpo.train = { cpo.retrafo = function(...) NULL; cpo.train.invert = function() NULL }, cpo.retrafo = NULL, cpo.train.invert = NULL, cpo.invert = NULL)

  expect_error(cpo.df1c %>>% cpo(), "cpo.train did not create.*cpo.invert")


  cpo = makeCPOTargetOp("errorCPO.target.fri",
    properties.target = c("classif", "twoclass"), constant.invert = TRUE,
    cpo.train = { cpo.retrafo = function(...) NULL; cpo.invert = function() NULL }, cpo.retrafo = NULL, cpo.train.invert = NULL, cpo.invert = NULL)

  expect_error(cpo.df1c %>>% cpo(), "cpo.invert as created by cpo.train does not have \\(only\\) the required")

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

  expect_error(cpo.df1c %>>% cpo(), "cpo.invert as created by cpo.trafo does not have \\(only\\) the required")


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

  expect_error(cpo.df1c %>>% retrafo(cpo.df1c %>>% cpo()), "cpo.invert as created by cpo.retrafo does not have \\(only\\) the required")

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

  expect_error(cpo.df1c %>>% retrafo(cpo.df1c %>>% cpo()), "cpo.retrafo as created by cpo.trafo does not have \\(only\\) the required")

})


test_that("retrafoless cannot flip binary classif task", {

  localenv = new.env()

  flipcpotaskflip = makeCPORetrafoless("flipcpo",
    dataformat = "task",
    cpo.trafo = {
      expect_equal(localenv$pos, data$task.desc$positive)
      data = subsetTask(data)
      tmp = data$task.desc$positive
      data$task.desc$positive = data$task.desc$negative
      data$task.desc$negative = tmp
      data
    })

  flipcpotaskrev = makeCPORetrafoless("flipcpo",
    dataformat = "task",
    cpo.trafo = {
      expect_equal(localenv$pos, data$task.desc$positive)
      data = subsetTask(data)
      data$env$data[[target]] = factor(data$env$data[[target]], levels = rev(levels(data$env$data[[target]])))
      data
    })

  flipcpodfrev = makeCPORetrafoless("flipcpo",
    dataformat = "df.all",
    cpo.trafo = {
      expect_equal(localenv$pos, levels(data[[target]])[1])
      data[[target]] = factor(data[[target]], levels = rev(levels(data[[target]])))
      data
    })


  ortho = pid.task
  trans = subsetTask(pid.task)
  tmp = trans$task.desc$negative
  trans$task.desc$negative = trans$task.desc$positive
  trans$task.desc$positive = tmp

  ortho.rev = subsetTask(ortho)
  ortho.rev$env$data = flipTaskTarget(ortho$env$data, getTaskTargetNames(ortho))

  trans.rev = subsetTask(trans)
  trans.rev$env$data = flipTaskTarget(trans$env$data, getTaskTargetNames(trans))

  expect_equal(isLevelFlipped(ortho), !isLevelFlipped(trans))
  expect_equal(isLevelFlipped(ortho.rev), !isLevelFlipped(trans.rev))
  expect_equal(isLevelFlipped(ortho.rev), isLevelFlipped(trans))
  expect_equal(isLevelFlipped(ortho), isLevelFlipped(trans.rev))



  localenv$pos = pid.task$task.desc$positive
  expect_error(ortho %>>% flipcpotaskflip(), "changed task target feature order")
  expect_error(ortho %>>% flipcpotaskrev(), "changed task target feature order")
  expect_error(ortho %>>% flipcpodfrev(), "must not change target class levels")

  expect_error(ortho.rev %>>% flipcpotaskflip(), "changed task target feature order")
  expect_error(ortho.rev %>>% flipcpotaskrev(), "changed task target feature order")
  expect_error(ortho.rev %>>% flipcpodfrev(), "must not change target class levels")

  localenv$pos = setdiff(c("pos", "neg"), pid.task$task.desc$positive)
  expect_error(trans %>>% flipcpotaskflip(), "changed task target feature order")
  expect_error(trans %>>% flipcpotaskrev(), "changed task target feature order")
  expect_error(trans %>>% flipcpodfrev(), "must not change target class levels")

  expect_error(trans.rev %>>% flipcpotaskflip(), "changed task target feature order")
  expect_error(trans.rev %>>% flipcpotaskrev(), "changed task target feature order")
  expect_error(trans.rev %>>% flipcpodfrev(), "must not change target class levels")

})

test_that("task flipping during conversion works as expected", {

  ortho = pid.task
  trans = subsetTask(pid.task)
  tmp = trans$task.desc$negative
  trans$task.desc$negative = trans$task.desc$positive
  trans$task.desc$positive = tmp

  ortho.rev = subsetTask(ortho)
  ortho.rev$env$data = flipTaskTarget(ortho$env$data, getTaskTargetNames(ortho))

  trans.rev = subsetTask(trans)
  trans.rev$env$data = flipTaskTarget(trans$env$data, getTaskTargetNames(trans))

  expect_equal(isLevelFlipped(ortho), !isLevelFlipped(trans))
  expect_equal(isLevelFlipped(ortho.rev), !isLevelFlipped(trans.rev))
  expect_equal(isLevelFlipped(ortho.rev), isLevelFlipped(trans))
  expect_equal(isLevelFlipped(ortho), isLevelFlipped(trans.rev))

  applyGMC("clas.to.reg", FALSE, ci = TRUE,
    type = c("target", "target.extended"),
    dataformat = c("df.features", "split", "df.all"),
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
    }, convertfrom = "classif", convertto = "regr",
    applyfun = function(regrconv, type, line, dfx) {

      res.ortho = getTaskTargets(ortho %>>% regrconv())
      expect_subset(res.ortho, c(1, 2))

      res.trans = getTaskTargets(trans %>>% regrconv())
      expect_subset(res.trans, c(1, 2))

      expect_equal(getTaskTargets(ortho.rev %>>% regrconv()), res.ortho)
      expect_equal(getTaskTargets(trans.rev %>>% regrconv()), res.trans)
    })

})
