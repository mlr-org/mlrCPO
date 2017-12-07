


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


    task %>>% generalMakeCPO("setup",
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
      if (curdf %in% c("df.all", "task")) {
        expect_equal(length(localenv$withtarget), length(data))
        data = data[colnames(localenv$withtarget)]
      }
      expect_identical(data, localenv$withtarget)
      expect_identical(expected.target, target)
    }

    checkHalfdata = function(data) {
      expect_identical(data, localenv$notarget)
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
        expect_identical(data, localenv$notarget)
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

    applyGMC("testtrafo", strict, convertfrom = getTaskDesc(task)$type,
      retrafo = function(data, control, param, target) {
        if (is.retrafoless) {
          return(data[barrelroll, ])
        }
        if (missing(target)) {
          # focpo
          pos = 1
        } else {
          # tocpo
          pos = target
        }
        data[[pos]] = data[[pos]][barrelroll]
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
          } else {
            exp1 = getTaskData(task)
            posn = getTaskTargetNames(task)
          }
          exp1[[posn]] = exp1[[posn]][barrelroll]
          exp2 = exp1
          exp2[[posn]] = exp1[[posn]][barrelroll]
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
            action <<- 3
            expect_error(task %>>% cpo, "must not change target names")
          }
        } else {
          expect_error(task %>>% cpo, "must not change non-target columns")
          action <<- 0  # nolint
          ret = retrafo(task %>>% cpo)

          action <<- 1
          expect_error(task %>>% ret, "must not change non-target columns")
          if (getTaskDesc(task)$type != "cluster") {
            expect_error(getTaskData(task) %>>% ret, "must not change non-target columns")
          }
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

  applyGMC("testtrafo", TRUE, convertfrom = getTaskDesc(task)$type,
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

  applyGMC("testtrafo", TRUE, convertfrom = getTaskDesc(task)$type,
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

test_that("cpo.invert has information from cpo.trafo when taken as invert", {


})

test_that("cpo.invert has information from cpo.retrafo when taken as invert", {


})

test_that("presence of control object is checked", {

})

test_that("return values are checked", {
  # changing number of rows only allowed in retrafoless
  # changing column layout not allowed in retrafoless
  # changing number of columns not allowed in retrafoless
  # changing target forbidden in feature op
  # changing data forbidden in target op
})

test_that("capabilities behave as expected when breaking and rebuilding CPOTrained", {


})

test_that("tocpo sees the correct rows when using affect args", {

})

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

test_that("getters and setters", {


})

test_that("operators", {


})

test_that("printers", {


})

test_that("helpers", {
  # is.*
  # nulltonullcpo etc


})

test_that("datasplit", {


})

test_that("changing target names to clash gives error", {

})

test_that("target stays in its position unless names changed", {

})

test_that("properties.target is respected", {


})

test_that("target conversion works as expected", {


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

test_that("composing CPO with conversion etc. behaves as expected", {

  # throws errors when not compatible
  # convertfrom, convertto are updated
  # predict.type map
})

test_that("composing CPOTrained with conversion etc. behaves as expected", {
  # capabilities updated
  # convertfrom, convertto updated
  # errors when incompatible
  # predict.type map
})

test_that("'sometimes'-properties work as expected", {


})


test_that("cpoCache", {

})


test_that("NULLCPO", {

})

test_that("targetbound: target changes", {

})

test_that("target bound: changing data causes fail", {

})

test_that("targetbound type conversion", {
# ("classif", "multilabel", "regr", "surv", "cluster", "costsens")

})

test_that("targetbound datasplit", {

})

test_that("targetbound retrafo data change works", {

})

test_that("targetbound changes data in a different way on 'retrafo' fails", {

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

test_that("inferPredictionTypePossibilities, getResponseType", {

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
