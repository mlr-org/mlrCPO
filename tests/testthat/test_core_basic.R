
context("cpo base")

test_that("CPOs can be created", {

  expect_class(makeCPOFunctional("testCPO", cpo.trafo = { }), "CPOConstructor")

  expect_class(makeCPOFunctional("testCPO", a: integer[, 1], cpo.trafo = { }), "CPOConstructor")

  expect_class(makeCPOFunctional("testCPO", a = 1: integer[, 1], .par.vals = list(a = 0), cpo.trafo = { }), "CPOConstructor")

  expect_class(makeCPOFunctional("testCPO", .par.set = pSSLrn(a: integer[0, 1]), .par.vals = list(a = 0), cpo.trafo = { }), "CPOConstructor")


  expect_class(makeCPOObject("testCPO", cpo.trafo = { }, cpo.retrafo = { }), "CPOConstructor")

  expect_class(makeCPOObject("testCPO", a: integer[, 1], cpo.trafo = { }, cpo.retrafo = { }), "CPOConstructor")

  expect_class(makeCPOObject("testCPO", a = 1: integer[, 1], .par.vals = list(a = 0), cpo.trafo = { }, cpo.retrafo = { }), "CPOConstructor")

  expect_class(makeCPOObject("testCPO", .par.set = pSSLrn(a: integer[0, 1]), .par.vals = list(a = 0), cpo.trafo = { }, cpo.retrafo = { }), "CPOConstructor")

})

test_that("New makeCPO Interface", {

  expect_class(makeCPO("testCPO", cpo.train = { }, cpo.retrafo = { }), "CPOConstructor")

  expect_class(makeCPO("testCPO", par.set = pSS(a: integer[, 1]), cpo.train = { }, cpo.retrafo = { }), "CPOConstructor")

  expect_class(makeCPO("testCPO", par.set = pSS(a: integer[, 1]), list(a = 1), cpo.train = { }, cpo.retrafo = { }), "CPOConstructor")

  expect_class(makeCPO("testCPO", par.set = pSS(a: integer[, 1]), cpo.train = NULL, cpo.retrafo = { }), "CPOConstructor")

  expect_class(makeCPO("testCPO", par.set = pSS(a: integer[, 1]), cpo.train = { }, cpo.retrafo = NULL), "CPOConstructor")

  expect_class(makeCPOTargetOp("testCPO", par.set = pSS(a: integer[, 1]),
    cpo.train = { }, cpo.retrafo = { }, cpo.train.invert = { }, cpo.invert = { }), "CPOConstructor")

  expect_class(makeCPOTargetOp("testCPO", par.set = pSS(a: integer[, 1]),
    cpo.train = { }, cpo.retrafo = { }, cpo.train.invert = { }, cpo.invert = NULL), "CPOConstructor")

  expect_class(makeCPOTargetOp("testCPO", par.set = pSS(a: integer[, 1]),
    cpo.train = { }, cpo.retrafo = NULL, cpo.train.invert = NULL, cpo.invert = NULL), "CPOConstructor")

  expect_class(makeCPOTargetOp("testCPO", par.set = pSS(a: integer[, 1]),
    cpo.train = { }, cpo.retrafo = NULL, cpo.train.invert = NULL, cpo.invert = { }), "CPOConstructor")

  expect_class(makeCPOTargetOp("testCPO", par.set = pSS(a: integer[, 1]), constant.invert = TRUE,
    cpo.train = NULL, cpo.retrafo = { }, cpo.train.invert = NULL, cpo.invert = { }), "CPOConstructor")

  expect_class(makeCPOTargetOp("testCPO", par.set = pSS(a: integer[, 1]), constant.invert = TRUE,
    cpo.train = { }, cpo.retrafo = { }, cpo.train.invert = NULL, cpo.invert = { }), "CPOConstructor")

  expect_class(makeCPOTargetOp("testCPO", par.set = pSS(a: integer[, 1]), constant.invert = TRUE,
    cpo.train = { }, cpo.retrafo = NULL, cpo.train.invert = NULL, cpo.invert = NULL), "CPOConstructor")

  expect_class(makeCPORetrafoless("testCPO", par.set = pSS(a: integer[, 1]),
    cpo.trafo = { }), "CPOConstructor")

  expect_class(makeCPOExtendedTrafo("testCPO", par.set = pSS(a: integer[, 1]),
    cpo.trafo = { }, cpo.retrafo = NULL), "CPOConstructor")

  expect_class(makeCPOExtendedTrafo("testCPO", par.set = pSS(a: integer[, 1]),
    cpo.trafo = { }, cpo.retrafo = { }), "CPOConstructor")

  expect_class(makeCPOExtendedTargetOp("testCPO", par.set = pSS(a: integer[, 1]),
    cpo.trafo = { }, cpo.retrafo = { }, cpo.invert = NULL), "CPOConstructor")

  expect_class(makeCPOExtendedTargetOp("testCPO", par.set = pSS(a: integer[, 1]),
    cpo.trafo = { }, cpo.retrafo = NULL, cpo.invert = { }), "CPOConstructor")

  expect_class(makeCPOExtendedTargetOp("testCPO", par.set = pSS(a: integer[, 1]),
    cpo.trafo = { }, cpo.retrafo = NULL, cpo.invert = NULL), "CPOConstructor")

  expect_class(makeCPOExtendedTargetOp("testCPO", par.set = pSS(a: integer[, 1]),
    cpo.trafo = { }, cpo.retrafo = { }, cpo.invert = { }), "CPOConstructor")

  expect_class(makeCPOExtendedTargetOp("testCPO", par.set = pSS(a: integer[, 1]), constant.invert = TRUE,
    cpo.trafo = { }, cpo.retrafo = { }, cpo.invert = NULL), "CPOConstructor")

  expect_class(makeCPOExtendedTargetOp("testCPO", par.set = pSS(a: integer[, 1]), constant.invert = TRUE,
    cpo.trafo = { }, cpo.retrafo = NULL, cpo.invert = { }), "CPOConstructor")

  expect_class(makeCPOExtendedTargetOp("testCPO", par.set = pSS(a: integer[, 1]), constant.invert = TRUE,
    cpo.trafo = { }, cpo.retrafo = NULL, cpo.invert = NULL), "CPOConstructor")

  expect_class(makeCPOExtendedTargetOp("testCPO", par.set = pSS(a: integer[, 1]), constant.invert = TRUE,
    cpo.trafo = { }, cpo.retrafo = { }, cpo.invert = { }), "CPOConstructor")


})

test_that("Parameter Values are checked", {

  creators = list(
      function(...) makeCPO("testCPO", ..., cpo.train = { }, cpo.retrafo = { }),
      function(...) makeCPOExtendedTrafo("testCPO", ..., cpo.trafo = { }, cpo.retrafo = { }),
      function(...) makeCPOTargetOp("testCPO", ..., cpo.train = { }, cpo.retrafo = { }, cpo.train.invert = { }, cpo.invert = { }),
      function(...) makeCPOExtendedTargetOp("testCPO", ..., cpo.trafo = { }, cpo.retrafo = { }, cpo.invert = { }),
      function(...) makeCPORetrafoless("testCPO", ..., cpo.trafo = { }))

  for (cr in creators) {
    expect_class(cr(par.set = pSS(a: integer[, 1])), "CPOConstructor")
    expect_class(cr(par.set = pSS(a: integer[, 1]), list(a = 1)), "CPOConstructor")
    expect_error(cr(par.set = pSS(a: integer[, 1]), list(b = 1)), "'b'.*par.vals.*not parameters")
    expect_error(cr(par.set = pSS(a: integer[, 1]), list(a = 2)), "2 is not feasible.*'a'")
    expect_error(cr(par.set = pSS(export: integer[, 1])), "reserved")
  }

})


test_that("CPO with no parameters don't crash", {

  empty.cpos = list(
      makeCPOFunctional("testCPOEmptyF", cpo.trafo = {
        cpo.retrafo = function(data) data
        data
      }),

      makeCPOObject("testCPOEmptyF", cpo.trafo = {
        control = 0
        data
      }, cpo.retrafo = {
        data
      }),

      makeCPO("emptyCPO", cpo.train = { }, cpo.retrafo = { data }),

      makeCPO("emptyCPO.sl", cpo.train = NULL, cpo.retrafo = { data }),

      makeCPO("emptyCPO.fr", cpo.train = { identity }, cpo.retrafo = NULL),

      makeCPOTargetOp("emptyCPO.target",
        properties.target = c("classif", "twoclass"),
        cpo.train = { }, cpo.retrafo = { target }, cpo.train.invert = { }, cpo.invert = { target }),

      makeCPOTargetOp("emptyCPO.target.fni",
        properties.target = c("classif", "twoclass"),
        cpo.train = { }, cpo.retrafo = { target }, cpo.train.invert = { function(target, ...) target }, cpo.invert = NULL),

      makeCPOTargetOp("emptyCPO.target.fnr.fni",
        properties.target = c("classif", "twoclass"),
        cpo.train = {
          cpo.retrafo = function(target, ...) target
          cpo.train.invert = function(target, ...) function(target, ...) target
        }, cpo.retrafo = NULL, cpo.train.invert = NULL, cpo.invert = NULL),

      makeCPOTargetOp("emptyCPO.target.fnr",
        properties.target = c("classif", "twoclass"),
        cpo.train = {
          cpo.retrafo = function(target, ...) target
          cpo.train.invert = function(...) NULL
        }, cpo.retrafo = NULL, cpo.train.invert = NULL, cpo.invert = { target }),

      makeCPOTargetOp("emptyCPO.target.ci.sl", constant.invert = TRUE,
        properties.target = c("classif", "twoclass"),
        cpo.train = NULL, cpo.retrafo = { target }, cpo.train.invert = NULL, cpo.invert = { target }),

      makeCPOTargetOp("emptyCPO.target.ci", constant.invert = TRUE,
        properties.target = c("classif", "twoclass"),
        cpo.train = { }, cpo.retrafo = { target }, cpo.train.invert = NULL, cpo.invert = { target }),

      makeCPOTargetOp("emptyCPO.target.ci.fr", constant.invert = TRUE,
        properties.target = c("classif", "twoclass"),
        cpo.train = {
          cpo.retrafo = function(target, ...) target
          cpo.invert = function(target, ...) target
        }, cpo.retrafo = NULL, cpo.train.invert = NULL, cpo.invert = NULL),

      makeCPORetrafoless("emptyCPO.retrafoless",
        cpo.trafo = { data }),

      makeCPOExtendedTrafo("emptyCPO.extended",
        cpo.trafo = { control = NULL ; data }, cpo.retrafo = { data }),

      makeCPOExtendedTrafo("emptyCPO.extended.fr",
        cpo.trafo = { cpo.retrafo = identity ; data }, cpo.retrafo = NULL),

      makeCPOExtendedTargetOp("emptyCPO.extended.target.fi",
        properties.target = c("classif", "twoclass"),
        cpo.trafo = {
          control = NULL
          cpo.invert = function(target, ...) target
          target
        }, cpo.retrafo = {
          cpo.invert = function(target, ...) target
          target
        }, cpo.invert = NULL),

      makeCPOExtendedTargetOp("emptyCPO.extended.target.fr",
        properties.target = c("classif", "twoclass"),
        cpo.trafo = {
          cpo.retrafo = function(target, ...) {
            control.invert = NULL
            target
          }
          control.invert = NULL
          target
        }, cpo.retrafo = NULL, cpo.invert = { target }),

      makeCPOExtendedTargetOp("emptyCPO.extended.target.fr.fi",
        properties.target = c("classif", "twoclass"),
        cpo.trafo = {
          cpo.retrafo = function(target, ...) {
            cpo.invert = function(target, ...) target
            target
          }
          cpo.invert = function(target, ...) target
          target
        }, cpo.retrafo = NULL, cpo.invert = NULL),

      makeCPOExtendedTargetOp("emptyCPO.extended.target",
        properties.target = c("classif", "twoclass"),
        cpo.trafo = {
          control.invert = NULL
          control = NULL
          target
        }, cpo.retrafo = {
          control.invert = NULL
          target
        }, cpo.invert = { target }),

      makeCPOExtendedTargetOp("emptyCPO.extended.target.ci.fi", constant.invert = TRUE,
        properties.target = c("classif", "twoclass"),
        cpo.trafo = {
          control = NULL
          cpo.invert = function(target, ...) target
          target
        }, cpo.retrafo = {
          target
        }, cpo.invert = NULL),

      makeCPOExtendedTargetOp("emptyCPO.extended.target.ci.fr", constant.invert = TRUE,
        properties.target = c("classif", "twoclass"),
        cpo.trafo = {
          cpo.retrafo = function(target, ...) {
            target
          }
          control.invert = NULL
          target
        }, cpo.retrafo = NULL, cpo.invert = { target }),

      makeCPOExtendedTargetOp("emptyCPO.extended.target.ci.fr.fi", constant.invert = TRUE,
        properties.target = c("classif", "twoclass"),
        cpo.trafo = {
          cpo.retrafo = function(target, ...) {
            target
          }
          cpo.invert = function(target, ...) target
          target
        }, cpo.retrafo = NULL, cpo.invert = NULL),

      makeCPOExtendedTargetOp("emptyCPO.extended.target.ci", constant.invert = TRUE,
        properties.target = c("classif", "twoclass"),
        cpo.trafo = {
          control.invert = NULL
          control = NULL
          target
        }, cpo.retrafo = {
          target
        }, cpo.invert = { target }))

  names(empty.cpos) = BBmisc::vcapply(empty.cpos, getCPOName)

  testCPO = function(ecpo) {
    assert_class(ecpo(), "CPO")
    assert_class(ecpo(id = "test"), "CPO")
    pt = pid.task %>>% ecpo()
    expect_identical(getTaskData(pid.task), getTaskData(pt))
    pt2 = pid.task %>>% retrafo(pt)
    expect_identical(getTaskData(pid.task), getTaskData(pt2))
    targ = getTaskData(pid.task, target.extra = TRUE)$target
    tx1 = invert(inverter(pt), targ)
    expect_identical(targ, tx1)
    tx2 = invert(inverter(pt2), targ)
    expect_identical(targ, tx2)
    train(ecpo() %>>% makeLearner("classif.logreg"), pid.task)
    train(setCPOId(ecpo(), "test") %>>% makeLearner("classif.logreg"), pid.task)
    train(setCPOId(ecpo("test"), "test2") %>>% makeLearner("classif.logreg"), pid.task)

    expect_equal(length(getHyperPars(ecpo())), 0)
    expect_equal(length(getHyperPars(ecpo("test"))), 0)

    expect_equal(length(getParamSet(ecpo())$pars), 0)
    expect_equal(length(getParamSet(ecpo("test"))$pars), 0)
  }

  for (ecpo in empty.cpos) {
    testCPO(ecpo)
  }

})

test_that("CPO parameters behave as expected", {

  testglobalenv$cpotest.parvals = list()
  testglobalenv$cpotest.parvals2 = list()
  testglobalenv$cpotest.parvals3 = list()

  cpof = makeCPOFunctional("testCPOF",
    a: integer[, ], b = 1: integer[, ], c = 1: integer[, ], d: integer[, ], e: integer[, ],
    .par.vals = list(a = 1, b = 2, c = 1, d = 1),
    cpo.trafo = {
      testglobalenv$cpotest.parvals = list(a = a, b = b, c = c, d = d, e = e)  # nolint
      cpo.retrafo = function(data) data
      data
    })

  cpo2f = makeCPOFunctional("testCPO2F",
    a: numeric[, ], z: integer[, ], model = TRUE: logical,
    cpo.trafo = {
      testglobalenv$cpotest.parvals2 = list(a = a, z = z)  # nolint
      cpo.retrafo = function(data) data
      data
    })

  cpo3f = makeCPOFunctional("testCPO3F",
    f: integer[, ],
    cpo.trafo = {
      testglobalenv$cpotest.parvals3 = c(testglobalenv$cpotest.parvals3, f)  # nolint
      cpo.retrafo = function(data) data
      data
    })

  cpoo = makeCPOObject("testCPOO",
    a: integer[, ], b = 1: integer[, ], c = 1: integer[, ], d: integer[, ], e: integer[, ],
    .par.vals = list(a = 1, b = 2, c = 1, d = 1),
    cpo.trafo = {
      testglobalenv$cpotest.parvals = list(a = a, b = b, c = c, d = d, e = e)  # nolint
      control = 0
      data
    },
    cpo.retrafo = {
      data
    })

  cpo2o = makeCPOObject("testCPO2O",
    a: numeric[, ], z: integer[, ], model = TRUE: logical,
    cpo.trafo = {
      testglobalenv$cpotest.parvals2 = list(a = a, z = z)  # nolint
      control = 0
        data
    },
    cpo.retrafo = {
      data
    })

  cpo3o = makeCPOObject("testCPO3O",
    f: integer[, ],
    cpo.trafo = {
      testglobalenv$cpotest.parvals3 = c(testglobalenv$cpotest.parvals3, f)  # nolint
      control = 0
      data
    },
    cpo.retrafo = {
      data
    })

  # add id prefix to list names
  addid = function(id, l) {
    names(l) = paste(id, names(l), sep = ".")
    l
  }

  testCPO = function(cpo, cpo2, cpo3) {

    id1 = getCPOId(cpo())
    id2 = getCPOId(cpo2())
    id3 = getCPOId(cpo3())

    # normal parameters
    expect_class(cpo, "CPOConstructor")

    expect_identical(getHyperPars(cpo()), addid(id1, list(a = 1, b = 2, c = 1, d = 1)))

    expect_identical(getHyperPars(cpo(b = 3)), addid(id1, list(a = 1, b = 3, c = 1, d = 1)))

    expect_identical(getHyperPars(cpo(3)), addid(id1, list(a = 3, b = 2, c = 1, d = 1)))

    cpo.obj = setHyperPars(cpo(3, 4), par.vals = addid(id1, list(b = 0, c = -1)))

    expect_identical(getHyperPars(cpo.obj), addid(id1, list(a = 3, b = 0, c = -1, d = 1)))

    cpo.learner = cpo.obj %>>% makeLearner("classif.logreg", model = FALSE)

    expect_identical(getHyperPars(cpo.learner), c(list(model = FALSE), addid(id1, list(a = 3, b = 0, c = -1, d = 1))))

    expect_error(train(cpo.learner, pid.task), "Parameter .*e.*missing")

    testglobalenv$cpotest.parvals = list()  # nolint
    train(setHyperPars(cpo.learner, par.vals = addid(id1, list(e = 900))), pid.task)
    expect_identical(testglobalenv$cpotest.parvals, list(a = 3, b = 0, c = -1, d = 1, e = 900))


    # ID = NULL parameters
    expect_identical(getHyperPars(cpo(id = NULL)), list(a = 1, b = 2, c = 1, d = 1))

    expect_identical(getHyperPars(cpo(id = NULL, b = 3)), list(a = 1, b = 3, c = 1, d = 1))

    expect_identical(getHyperPars(cpo(id = NULL, 3)), list(a = 3, b = 2, c = 1, d = 1))

    cpo.obj = setHyperPars(cpo(id = NULL, 3, 4), par.vals = list(b = 0, c = -1))

    expect_identical(getHyperPars(cpo.obj), list(a = 3, b = 0, c = -1, d = 1))

    cpo.learner = cpo.obj %>>% makeLearner("classif.logreg", model = FALSE)

    expect_identical(getHyperPars(cpo.learner), c(list(model = FALSE), list(a = 3, b = 0, c = -1, d = 1)))

    expect_error(train(cpo.learner, pid.task), "Parameter .*e.*missing")

    testglobalenv$cpotest.parvals = list()  # nolint
    train(setHyperPars(cpo.learner, par.vals = list(e = 900)), pid.task)
    expect_identical(testglobalenv$cpotest.parvals, list(a = 3, b = 0, c = -1, d = 1, e = 900))


    # parameters of cpo with id
    expect_identical(getHyperPars(cpo(id = "x")), list(x.a = 1, x.b = 2, x.c = 1, x.d = 1))

    expect_identical(getHyperPars(cpo(b = 3, id = "x")), list(x.a = 1, x.b = 3, x.c = 1, x.d = 1))

    cpo.obj = setHyperPars(cpo(3, 4, id = "x"), x.b = 0, x.c = -1)

    expect_identical(getHyperPars(cpo.obj), list(x.a = 3, x.b = 0, x.c = -1, x.d = 1))

    cpo.learner = cpo.obj %>>% makeLearner("classif.logreg", model = FALSE)

    expect_identical(getHyperPars(cpo.learner), list(model = FALSE, x.a = 3, x.b = 0, x.c = -1, x.d = 1))

    expect_error(train(cpo.learner, pid.task), "Parameter (x\\.)?e .*missing")

    cpo.learner = setCPOId(cpo.obj, "y") %>>% makeLearner("classif.logreg", model = FALSE)

    expect_error(train(cpo.learner, pid.task), "Parameter (y\\.)?e .*missing")

    testglobalenv$cpotest.parvals = list()  # nolint
    train(setHyperPars(cpo.learner, y.e = 901), pid.task)
    expect_identical(testglobalenv$cpotest.parvals, list(a = 3, b = 0, c = -1, d = 1, e = 901))

    expect_error(setCPOId(cpo.obj %>>% cpo3(), "testx"), "Cannot set ID of compound CPO")

    # parameters of coupled CPOs
    expect_error(cpo(3, id = "x") %>>% cpo2(4, id = "x") %>>% cpo3(id = "x"), 'Parameter "x\\.a" occurs in both')

    expect_class(cpo(3) %>>% cpo2(4, id = "2nd") %>>% cpo3(), "CPO")

    expect_error(cpo2(4, id = "dummy") %>>% cpo3() %>>% dummylearnercpo, 'Parameter "dummy.model" occurs in both')

    lrn = cpo(3, id = "fst") %>>% cpo2(4, id = "2nd") %>>% cpo3(id = "thrd") %>>% makeLearner("classif.logreg", model = TRUE)

    expect_identical(getHyperPars(lrn), list(model = TRUE, fst.a = 3, fst.b = 2, fst.c = 1, fst.d = 1, `2nd.a` = 4, `2nd.model` = TRUE))

    expect_error(train(lrn, pid.task), "Parameters? fst\\.e.*missing")

    expect_error(train(setHyperPars(lrn, fst.e = 90), pid.task), "Parameters? (2nd\\.)?z.*missing")

    testglobalenv$cpotest.parvals = list()  # nolint
    testglobalenv$cpotest.parvals2 = list()  # nolint
    testglobalenv$cpotest.parvals3 = list()  # nolint
    train(setHyperPars(lrn, fst.e = 90, `2nd.a` = 9000, `2nd.z` = -10, thrd.f = 222), pid.task)
    expect_identical(testglobalenv$cpotest.parvals, list(a = 3, b = 2, c = 1, d = 1, e = 90))
    expect_identical(testglobalenv$cpotest.parvals2, list(a = 9000, z = -10))
    expect_identical(testglobalenv$cpotest.parvals3, list(222))

    # multiple instances of the same CPO
    testglobalenv$cpotest.parvals3 = list()  # nolint
    train(cpo3(id = "a", 100) %>>% cpo3(id = "b", 10) %>>% cpo3(20) %>>% makeLearner("classif.logreg"), pid.task)
    expect_identical(testglobalenv$cpotest.parvals3, list(100, 10, 20))

    testglobalenv$cpotest.parvals3 = list()  # nolint
    lrn = cpo3(id = "a") %>>% cpo3(id = "b", 10) %>>% cpo3(id = "x") %>>% makeLearner("classif.logreg")
    train(setHyperPars(lrn, a.f = 1000, x.f = 99), pid.task)
    expect_identical(testglobalenv$cpotest.parvals3, list(1000, 10, 99))
  }

  testCPO(cpof, cpo2f, cpo3f)

  testCPO(cpoo, cpo2o, cpo3o)

})

test_that("CPO Parameters of new makeCPO interface", {

  par.set = pSS(a: integer[, ],
    b = 1: integer[, ],
    c: integer[, ],
    d = 1: integer[, ])

  # obsolete:  par.vals = list(c = 2, d = 3)
  par.vals = list(b = 1, c = 2, d = 3)

  param.cpos = list(
      makeCPO("paramCPO", par.set, par.vals, cpo.train = {
        testglobalenv$cpotest.parvals %c=% list(a = a, b = b, c = c, d = d)
      }, cpo.retrafo = { testglobalenv$cpotest.parvals %c=% list(a = a, b = b, c = c, d = d)
        data
      }),

      makeCPO("paramCPO.sl", par.set, par.vals, cpo.train = NULL, cpo.retrafo = {
        testglobalenv$cpotest.parvals %c=% list(a = a, b = b, c = c, d = d)
        data
      }),

      makeCPO("paramCPO.fr", par.set, par.vals, cpo.train = {
        testglobalenv$cpotest.parvals %c=% list(a = a, b = b, c = c, d = d)
        function(x) {
          testglobalenv$cpotest.parvals %c=% list(a = a, b = b, c = c, d = d)
          x
        }
      }, cpo.retrafo = NULL),

      makeCPOTargetOp("paramCPO.target", par.set, par.vals,
        properties.target = c("classif", "twoclass"),
        cpo.train = {
          testglobalenv$cpotest.parvals %c=% list(a = a, b = b, c = c, d = d)
        }, cpo.retrafo = {
          testglobalenv$cpotest.parvals %c=% list(a = a, b = b, c = c, d = d)
          target
        }, cpo.train.invert = {
          testglobalenv$cpotest.parvals %c=% list(a = a, b = b, c = c, d = d)
        }, cpo.invert = {
          testglobalenv$cpotest.parvals %c=% list(a = a, b = b, c = c, d = d)
          target
        }),

      makeCPOTargetOp("paramCPO.target.fni", par.set, par.vals,
        properties.target = c("classif", "twoclass"),
        cpo.train = {
          testglobalenv$cpotest.parvals %c=% list(a = a, b = b, c = c, d = d)
        }, cpo.retrafo = {
          testglobalenv$cpotest.parvals %c=% list(a = a, b = b, c = c, d = d)
          target
        }, cpo.train.invert = {
          testglobalenv$cpotest.parvals %c=% list(a = a, b = b, c = c, d = d)
          function(target, ...) {
            testglobalenv$cpotest.parvals %c=% list(a = a, b = b, c = c, d = d)
            target
          }
        }, cpo.invert = NULL),

      makeCPOTargetOp("paramCPO.target.fnr.fni", par.set, par.vals,
        properties.target = c("classif", "twoclass"),
        cpo.train = {
          cpo.retrafo = function(target, ...) {
            testglobalenv$cpotest.parvals %c=% list(a = a, b = b, c = c, d = d)
            target
          }
          cpo.train.invert = function(target, ...) {
            testglobalenv$cpotest.parvals %c=% list(a = a, b = b, c = c, d = d)
            function(target, ...) {
              testglobalenv$cpotest.parvals %c=% list(a = a, b = b, c = c, d = d)
              target
            }
          }
          testglobalenv$cpotest.parvals %c=% list(a = a, b = b, c = c, d = d)
        }, cpo.retrafo = NULL, cpo.train.invert = NULL, cpo.invert = NULL),

      makeCPOTargetOp("paramCPO.target.fnr", par.set, par.vals,
        properties.target = c("classif", "twoclass"),
        cpo.train = {
          testglobalenv$cpotest.parvals %c=% list(a = a, b = b, c = c, d = d)
          cpo.retrafo = function(target, ...) {
            testglobalenv$cpotest.parvals %c=% list(a = a, b = b, c = c, d = d)
            target
          }
          cpo.train.invert = function(...) {
            testglobalenv$cpotest.parvals %c=% list(a = a, b = b, c = c, d = d)
            NULL
          }
        }, cpo.retrafo = NULL, cpo.train.invert = NULL, cpo.invert = {
          testglobalenv$cpotest.parvals %c=% list(a = a, b = b, c = c, d = d)
          target
        }),

      makeCPOTargetOp("paramCPO.target.ci.sl", par.set, par.vals, constant.invert = TRUE,
        properties.target = c("classif", "twoclass"),
        cpo.train = NULL, cpo.retrafo = {
          testglobalenv$cpotest.parvals %c=% list(a = a, b = b, c = c, d = d)
          target
        }, cpo.train.invert = NULL, cpo.invert = {
          testglobalenv$cpotest.parvals %c=% list(a = a, b = b, c = c, d = d)
          target
        }),

      makeCPOTargetOp("paramCPO.target.ci", par.set, par.vals, constant.invert = TRUE,
        properties.target = c("classif", "twoclass"),
        cpo.train = {
          testglobalenv$cpotest.parvals %c=% list(a = a, b = b, c = c, d = d)
        }, cpo.retrafo = {
          testglobalenv$cpotest.parvals %c=% list(a = a, b = b, c = c, d = d)
          target
        }, cpo.train.invert = NULL, cpo.invert = {
          testglobalenv$cpotest.parvals %c=% list(a = a, b = b, c = c, d = d)
          target
        }),

      makeCPOTargetOp("paramCPO.target.ci.fr", par.set, par.vals, constant.invert = TRUE,
        properties.target = c("classif", "twoclass"),
        cpo.train = {
          testglobalenv$cpotest.parvals %c=% list(a = a, b = b, c = c, d = d)
          cpo.retrafo = function(target, ...) {
            testglobalenv$cpotest.parvals %c=% list(a = a, b = b, c = c, d = d)
            target
          }
          cpo.invert = function(target, ...) {
            testglobalenv$cpotest.parvals %c=% list(a = a, b = b, c = c, d = d)
            target
          }
        }, cpo.retrafo = NULL, cpo.train.invert = NULL, cpo.invert = NULL),

      makeCPORetrafoless("paramCPO.retrafoless", par.set, par.vals,
        cpo.trafo = {
          testglobalenv$cpotest.parvals %c=% list(a = a, b = b, c = c, d = d)
          data
        }),

      makeCPOExtendedTrafo("paramCPO.extended", par.set, par.vals,
        cpo.trafo = {
          testglobalenv$cpotest.parvals %c=% list(a = a, b = b, c = c, d = d)
          control = NULL
          data
        }, cpo.retrafo = {
          testglobalenv$cpotest.parvals %c=% list(a = a, b = b, c = c, d = d)
          data
        }),

      makeCPOExtendedTrafo("paramCPO.extended.fr", par.set, par.vals,
        cpo.trafo = {
          testglobalenv$cpotest.parvals %c=% list(a = a, b = b, c = c, d = d)
          cpo.retrafo = function(x) {
            testglobalenv$cpotest.parvals %c=% list(a = a, b = b, c = c, d = d)
            x
          }
          data
        }, cpo.retrafo = NULL),

      makeCPOExtendedTargetOp("paramCPO.extended.target.fi", par.set, par.vals,
        properties.target = c("classif", "twoclass"),
        cpo.trafo = {
          testglobalenv$cpotest.parvals %c=% list(a = a, b = b, c = c, d = d)
          control = NULL
          cpo.invert = function(target, ...) {
            testglobalenv$cpotest.parvals %c=% list(a = a, b = b, c = c, d = d)
            target
          }
          target
        }, cpo.retrafo = {
          testglobalenv$cpotest.parvals %c=% list(a = a, b = b, c = c, d = d)
          cpo.invert = function(target, ...) {
            testglobalenv$cpotest.parvals %c=% list(a = a, b = b, c = c, d = d)
            target
          }
          target
        }, cpo.invert = NULL),

      makeCPOExtendedTargetOp("paramCPO.extended.target.fr", par.set, par.vals,
        properties.target = c("classif", "twoclass"),
        cpo.trafo = {
          testglobalenv$cpotest.parvals %c=% list(a = a, b = b, c = c, d = d)
          cpo.retrafo = function(target, ...) {
            testglobalenv$cpotest.parvals %c=% list(a = a, b = b, c = c, d = d)
            control.invert = NULL
            target
          }
          control.invert = NULL
          target
        }, cpo.retrafo = NULL, cpo.invert = {
          testglobalenv$cpotest.parvals %c=% list(a = a, b = b, c = c, d = d)
          target
        }),

      makeCPOExtendedTargetOp("paramCPO.extended.target.fr.fi", par.set, par.vals,
        properties.target = c("classif", "twoclass"),
        cpo.trafo = {
          testglobalenv$cpotest.parvals %c=% list(a = a, b = b, c = c, d = d)
          cpo.retrafo = function(target, ...) {
            testglobalenv$cpotest.parvals %c=% list(a = a, b = b, c = c, d = d)
            cpo.invert = function(target, ...) {
              testglobalenv$cpotest.parvals %c=% list(a = a, b = b, c = c, d = d)
              target
            }
            target
          }
          cpo.invert = function(target, ...) {
            testglobalenv$cpotest.parvals %c=% list(a = a, b = b, c = c, d = d)
            target
          }
          target
        }, cpo.retrafo = NULL, cpo.invert = NULL),

      makeCPOExtendedTargetOp("paramCPO.extended.target", par.set, par.vals,
        properties.target = c("classif", "twoclass"),
        cpo.trafo = {
          testglobalenv$cpotest.parvals %c=% list(a = a, b = b, c = c, d = d)
          control.invert = NULL
          control = NULL
          target
        }, cpo.retrafo = {
          testglobalenv$cpotest.parvals %c=% list(a = a, b = b, c = c, d = d)
          control.invert = NULL
          target
        }, cpo.invert = {
          testglobalenv$cpotest.parvals %c=% list(a = a, b = b, c = c, d = d)
          target
        }),

      makeCPOExtendedTargetOp("paramCPO.extended.target.ci.fi", par.set, par.vals, constant.invert = TRUE,
        properties.target = c("classif", "twoclass"),
        cpo.trafo = {
          testglobalenv$cpotest.parvals %c=% list(a = a, b = b, c = c, d = d)
          control = NULL
          cpo.invert = function(target, ...) {
            testglobalenv$cpotest.parvals %c=% list(a = a, b = b, c = c, d = d)
            target
          }
          target
        }, cpo.retrafo = {
          testglobalenv$cpotest.parvals %c=% list(a = a, b = b, c = c, d = d)
          target
        }, cpo.invert = NULL),

      makeCPOExtendedTargetOp("paramCPO.extended.target.ci.fr", par.set, par.vals, constant.invert = TRUE,
        properties.target = c("classif", "twoclass"),
        cpo.trafo = {
          testglobalenv$cpotest.parvals %c=% list(a = a, b = b, c = c, d = d)
          cpo.retrafo = function(target, ...) {
            testglobalenv$cpotest.parvals %c=% list(a = a, b = b, c = c, d = d)
            target
          }
          control.invert = NULL
          target
        }, cpo.retrafo = NULL, cpo.invert = {
          testglobalenv$cpotest.parvals %c=% list(a = a, b = b, c = c, d = d)
          target
        }),

      makeCPOExtendedTargetOp("paramCPO.extended.target.ci.fr.fi", par.set, par.vals, constant.invert = TRUE,
        properties.target = c("classif", "twoclass"),
        cpo.trafo = {
          testglobalenv$cpotest.parvals %c=% list(a = a, b = b, c = c, d = d)
          cpo.retrafo = function(target, ...) {
            testglobalenv$cpotest.parvals %c=% list(a = a, b = b, c = c, d = d)
            target
          }
          cpo.invert = function(target, ...) {
            testglobalenv$cpotest.parvals %c=% list(a = a, b = b, c = c, d = d)
            target
          }
          target
        }, cpo.retrafo = NULL, cpo.invert = NULL),

      makeCPOExtendedTargetOp("paramCPO.extended.target.ci", par.set, par.vals, constant.invert = TRUE,
        properties.target = c("classif", "twoclass"),
        cpo.trafo = {
          testglobalenv$cpotest.parvals %c=% list(a = a, b = b, c = c, d = d)
          control.invert = NULL
          control = NULL
          target
        }, cpo.retrafo = {
          testglobalenv$cpotest.parvals %c=% list(a = a, b = b, c = c, d = d)
          target
        }, cpo.invert = {
          testglobalenv$cpotest.parvals %c=% list(a = a, b = b, c = c, d = d)
          target
        }))

  expvals = list(a = -1, b = 1, c = 2, d = 3)

  names(param.cpos) = BBmisc::vcapply(param.cpos, getCPOName)

  testCPO = function(pcpo, rep.train, rep.retrafo, rep.invert,
                     rep.tl = rep.train,
                     rep.rl = rep.retrafo + rep.invert) {
    expect_error(pid.task %>>% pcpo(), ".a of CPO.*missing")
    testglobalenv$cpotest.parvals = namedList()
    pt = pid.task %>>% setHyperPars(pcpo(id = NULL, a = 0), a = -1)
    expect_identical(testglobalenv$cpotest.parvals, rep(expvals, rep.train))

    testglobalenv$cpotest.parvals = namedList()
    pt2 = pid.task %>>% retrafo(pt)
    expect_identical(testglobalenv$cpotest.parvals, rep(expvals, rep.retrafo))

    targ = getTaskData(pid.task, target.extra = TRUE)$target
    testglobalenv$cpotest.parvals = namedList()
    tx1 = invert(inverter(pt), targ)
    expect_identical(testglobalenv$cpotest.parvals, rep(expvals, rep.invert))

    testglobalenv$cpotest.parvals = namedList()
    tx2 = invert(inverter(pt2), targ)
    expect_identical(testglobalenv$cpotest.parvals, rep(expvals, rep.invert))

    testglobalenv$cpotest.parvals = namedList()
    trn = train(pcpo(a = -1) %>>% makeLearner("classif.logreg"), pid.task)
    expect_identical(testglobalenv$cpotest.parvals, rep(expvals, rep.tl))

    testglobalenv$cpotest.parvals = namedList()
    predict(trn, pid.task)
    expect_identical(testglobalenv$cpotest.parvals, rep(expvals, rep.rl))
  }

  testCPO(param.cpos$paramCPO, 2, 1, 0)  # simple cpo
  testCPO(param.cpos$paramCPO.sl, 1, 1, 0)  # simple CPO, stateless
  testCPO(param.cpos$paramCPO.fr, 2, 1, 0)  # simple CPO, functional
  testCPO(param.cpos$paramCPO.target, 3, 2, 1, rep.tl = 2, rep.rl = 2)  # target CPO
  testCPO(param.cpos$paramCPO.target.fni, 3, 2, 1, rep.tl = 2, rep.rl = 2)  # target CPO, invert is functional
  testCPO(param.cpos$paramCPO.target.fnr.fni, 3, 2, 1, rep.tl = 2, rep.rl = 2)  # target CPO, retrafo and invert are functional
  testCPO(param.cpos$paramCPO.target.fnr, 3, 2, 1, rep.tl = 2, rep.rl = 2)  # target CPO, retrafo is functional
  testCPO(param.cpos$paramCPO.target.ci.sl, 1, 1, 1, rep.tl = 1, rep.rl = 1)  # target CPO, constant invert, stateless
  testCPO(param.cpos$paramCPO.target.ci, 2, 1, 1, rep.tl = 2, rep.rl = 1)  # target CPO, constant invert
  testCPO(param.cpos$paramCPO.target.ci.fr, 2, 1, 1, rep.tl = 2, rep.rl = 1)  # target CPO, constant invert, functional
  testCPO(param.cpos$paramCPO.retrafoless, 1, 0, 0)
  testCPO(param.cpos$paramCPO.extended, 1, 1, 0)  # extended CPO
  testCPO(param.cpos$paramCPO.extended.fr, 1, 1, 0)  # extended CPO, functional
  testCPO(param.cpos$paramCPO.extended.target.fi, 1, 1, 1)  # extended target CPO, functional invert
  testCPO(param.cpos$paramCPO.extended.target.fr, 1, 1, 1)  # extended target CPO, functional retrafo
  testCPO(param.cpos$paramCPO.extended.target.fr.fi, 1, 1, 1)  # extended target CPO, functional retrafo, functional invert
  testCPO(param.cpos$paramCPO.extended.target, 1, 1, 1)   # extended target CPO
  testCPO(param.cpos$paramCPO.extended.target.ci.fi, 1, 1, 1, rep.rl = 1)  # extended target CPO, constant invert, functional invert
  testCPO(param.cpos$paramCPO.extended.target.ci.fr, 1, 1, 1, rep.rl = 1)  # extended target CPO, constant invert, functional retrafo
  testCPO(param.cpos$paramCPO.extended.target.ci.fr.fi, 1, 1, 1, rep.rl = 1)  # extended target CPO, constant invert, functional retrafo, functional invert
  testCPO(param.cpos$paramCPO.extended.target.ci, 1, 1, 1, rep.rl = 1)  # extended target CPO

})


test_that("Functional CPO Parameter feasibility is checked", {

  expect_error(makeCPOFunctional("testCPOF",
    a: integer[, ],
    .par.vals = list(a = 1, b = 2),
    cpo.trafo = { }))

  cpo = makeCPOFunctional("testCPOF",
    a: integer[, ], b: integer[, ],
    .par.vals = list(a = 1, b = 2),
    cpo.trafo = { })

  expect_error(cpo(1.5, 2), "is not feasible for parameter 'a'")

  cpoo = cpo(1, 2)
  expect_class(cpoo, "CPO")
  expect_class(setHyperPars(cpoo, testCPOF.a = 10), "CPO")
  expect_error(setHyperPars(cpoo, testCPOF.a = 0.4), "is not feasible for parameter 'testCPOF.a'")

  expect_error(makeCPOFunctional("testCPOF",
    a: integer[, ], b: integer[0, 1],
    .par.vals = list(a = 1, b = 2),
    cpo.trafo = { }), "2 is not feasible for parameter 'b'")

  cpo = makeCPOFunctional("testCPOF",
    a: integer[, ], b: integer[0, 1],
    cpo.trafo = { })

  expect_error(cpo(1, 2), "2 is not feasible for parameter 'b'")
  cpoo = cpo(0, 0)
  expect_class(setHyperPars(cpoo, testCPOF.b = 1), "CPO")
  expect_error(setHyperPars(cpoo, testCPOF.b = 3), "is not feasible for parameter 'testCPOF.b'")

  expect_error(makeCPOFunctional("testCPOF",
    a: integer[, ], b = 2: integer[0, 1],
    cpo.trafo = { }), "'default' must be a feasible parameter setting")

  makeCPOFunctional("testCPOF",
    a = (function() 1): discrete[a = function() 1, b = function() 2],
    cpo.trafo = { })

  expect_error(makeCPOFunctional("testCPOF",
    a = (function() 3): discrete[a = function() 1, b = function() 2],
    cpo.trafo = { }),  "'default' must be a feasible parameter setting")

  cpo = makeCPOFunctional("testCPOF",
    a: discrete[a = function() 1, b = function() 2],
    .par.vals = list(a = function() 1),
    cpo.trafo = { })

  expect_error(cpo(function() 3), "<function> is not feasible for parameter 'a'")
  cpoo = cpo(function() 1)
  expect_class(cpoo, "CPO")
  expect_class(setHyperPars(cpoo, testCPOF.a = function() 1), "CPO")
  expect_error(setHyperPars(cpoo, testCPOF.a = function() 3), "not feasible for parameter 'testCPOF.a'")

  expect_error(makeCPOFunctional("testCPOF",
    a: discrete[a = function() 1, b = function() 2],
    .par.vals = list(a = function() 3),
    cpo.trafo = { }),  "<function> is not feasible for parameter 'a'")

})

test_that("Object based CPO Parameter feasibility is checked", {

  expect_error(makeCPOObject("testCPOO",
    a: integer[, ],
    .par.vals = list(a = 1, b = 2),
    cpo.trafo = { }, cpo.retrafo = { }))

  cpo = makeCPOObject("testCPOO",
    a: integer[, ], b: integer[, ],
    .par.vals = list(a = 1, b = 2),
    cpo.trafo = { }, cpo.retrafo = { })

  expect_error(cpo(1.5, 2), "is not feasible for parameter 'a'")

  cpoo = cpo(1, 2)
  expect_class(cpoo, "CPO")
  expect_class(setHyperPars(cpoo, testCPOO.a = 10), "CPO")
  expect_error(setHyperPars(cpoo, testCPOO.a = 0.4), "is not feasible for parameter 'testCPOO.a'")

  expect_error(makeCPOObject("testCPOO",
    a: integer[, ], b: integer[0, 1],
    .par.vals = list(a = 1, b = 2),
    cpo.trafo = { }, cpo.retrafo = { }), "2 is not feasible for parameter 'b'")

  cpo = makeCPOObject("testCPOO",
    a: integer[, ], b: integer[0, 1],
    cpo.trafo = { }, cpo.retrafo = { })

  expect_error(cpo(1, 2), "2 is not feasible for parameter 'b'")
  cpoo = cpo(0, 0)
  expect_class(setHyperPars(cpoo, testCPOO.b = 1), "CPO")
  expect_error(setHyperPars(cpoo, testCPOO.b = 3), "is not feasible for parameter 'testCPOO.b'")

  expect_error(makeCPOObject("testCPOO",
    a: integer[, ], b = 2: integer[0, 1],
    cpo.trafo = { }, cpo.retrafo = { }), "'default' must be a feasible parameter setting")

  makeCPOObject("testCPOO",
    a = (function() 1): discrete[a = function() 1, b = function() 2],
    cpo.trafo = { }, cpo.retrafo = { })

  expect_error(makeCPOObject("testCPOO",
    a = (function() 3): discrete[a = function() 1, b = function() 2],
    cpo.trafo = { }, cpo.retrafo = { }),  "'default' must be a feasible parameter setting")

  cpo = makeCPOObject("testCPOO",
    a: discrete[a = function() 1, b = function() 2],
    .par.vals = list(a = function() 1),
    cpo.trafo = { }, cpo.retrafo = { })

  expect_error(cpo(function() 3), "<function> is not feasible for parameter 'a'")
  cpoo = cpo(function() 1)
  expect_class(cpoo, "CPO")
  expect_class(setHyperPars(cpoo, testCPOO.a = function() 1), "CPO")
  expect_error(setHyperPars(cpoo, testCPOO.a = function() 3), "not feasible for parameter 'testCPOO.a'")

  expect_error(makeCPOObject("testCPOO",
    a: discrete[a = function() 1, b = function() 2],
    .par.vals = list(a = function() 3),
    cpo.trafo = { }, cpo.retrafo = { }),  "<function> is not feasible for parameter 'a'")

})

test_that("discrete parameters work well", {


  X = 1
  Y = 2

  cpof = makeCPOFunctional("testCPOF",
    a: logical, b: discrete[a, b, 1], c = 1: discrete[a, b, 1], d = c(TRUE, TRUE): logical^2, e: discrete[a = function() 1, b = function() Y]^2,
    cpo.trafo = {
      testglobalenv$cpotest.parvals = list(a = a, b = b, c = c, d = d, e = c(e[[1]](), e[[2]]()))  # nolint
      cpo.retrafo = function(data) data
      data
    })

  cpoo = makeCPOObject("testCPOO",
    a: logical, b: discrete[a, b, 1], c = 1: discrete[a, b, 1], d = c(TRUE, TRUE): logical^2, e: discrete[a = function() 1, b = function() Y]^2,
    cpo.trafo = {
      testglobalenv$cpotest.parvals = list(a = a, b = b, c = c, d = d, e = c(e[[1]](), e[[2]]()))  # nolint
      control = 0
      data
    },
    cpo.retrafo = {
      data
    })

  testCPO = function(cpo) {
    testglobalenv$cpotest.parvals = list()  # nolint
    train(cpo(TRUE, "a", e = list(function() 1, function() 1)) %>>% makeLearner("classif.logreg"), pid.task)
    expect_identical(testglobalenv$cpotest.parvals, list(a = TRUE, b = "a", c = 1, d = c(TRUE, TRUE), e = c(1, 1)))

    testglobalenv$cpotest.parvals = list()  # nolint
    train(cpo(TRUE, 1, e = list(function() Y, function() 1)) %>>% makeLearner("classif.logreg"), pid.task)
    expect_identical(testglobalenv$cpotest.parvals, list(a = TRUE, b = 1, c = 1, d = c(TRUE, TRUE), e = c(2, 1)))
  }

  testCPO(cpof)
  testCPO(cpoo)
})

test_that("preprocessing actually changes data", {

  testglobalenv$cpotest.parvals = list()  # nolint
  t = train(testlearnercpo, testtaskcpo)
  predict(t, testtaskcpo2)
  expect_equal(testglobalenv$cpotest.parvals, list(1, 3))

  testCPO = function(cpoMultiplier, cpoAdder) {
    testglobalenv$cpotest.parvals = list()  # nolint
    t = train(testlearnercpo, testtaskcpo)
    predict(t, testtaskcpo2)
    expect_identical(testglobalenv$cpotest.parvals, list(1, 3))

    testglobalenv$cpotest.parvals = list()  # nolint
    predict(train(cpoMultiplier(10) %>>% testlearnercpo, testtaskcpo), testtaskcpo2)
    expect_identical(testglobalenv$cpotest.parvals, list(10, 0.3))

    testglobalenv$cpotest.parvals = list()  # nolint
    predict(train(cpoAdder(3) %>>% testlearnercpo, testtaskcpo), testtaskcpo2)
    expect_identical(testglobalenv$cpotest.parvals, list(4, -1.5))


    testglobalenv$cpotest.parvals = list()  # nolint
    predict(train(cpoAdder(3) %>>%
                  cpoMultiplier(3) %>>%
                  cpoAdder(2, id = "second") %>>%
                  cpoMultiplier(10, id = "second") %>>%
                  testlearnercpo, testtaskcpo), testtaskcpo2)
    # Calculation happening:
    # Training:
    #   c(1, 2), +3, *3, +2, *10 -> c(140, 170)
    #   first adder gets a meandata of 1.5, second adder gets meandata of 13.5
    # Prediction:
    #   c(3, 4) - 3 - 1.5, / 3, - 2 - 13.5, / 10 -> c(-1.6, -1.57)
    expect_identical(testglobalenv$cpotest.parvals, list(140, -1.6))
  }

  testCPO(cpomultiplier.task.f, cpoadder.task.f)
  testCPO(cpomultiplier.task.o, cpoadder.task.o)

})


test_that("preprocessing with inverter changes data", {

  trivialtask = makeRegrTask("1-2-task", data.frame(A = c(1, 2), B = c(1, 2)), "B")
  trivialtask2 = makeRegrTask("3-4-task", data.frame(A = c(3, 4), B = c(3, 4)), "B")


  testglobalenv$cpotest.parvals = list()  # nolint
  t = train(testregressorcpo, trivialtask)
  predict(t, trivialtask2)
  expect_identical(testglobalenv$cpotest.parvals, list(1, 3))

  testglobalenv$cpotest.parvals = list()  # nolint
  expect_identical(predict(train(cpoinvertmultiplier(10) %>>% testregressorcpo, trivialtask),
    trivialtask2)$data$response, c(1000, 10))
  expect_identical(testglobalenv$cpotest.parvals, list(10, 3))

  testglobalenv$cpotest.parvals = list()  # nolint
  expect_identical(predict(train(cpoinvertadder(3) %>>% testregressorcpo, trivialtask),
    trivialtask2)$data$response, c(10, 4))
  expect_identical(testglobalenv$cpotest.parvals, list(4, 3))


  testglobalenv$cpotest.parvals = list()  # nolint
  pr = predict(train(cpoinvertadder(3) %>>%
                cpoinvertmultiplier(3) %>>%
                cpoinvertadder(2, id = "second") %>>%
                cpoinvertmultiplier(10, id = "second") %>>%
                testregressorcpo, trivialtask), trivialtask2)
  # Calculation happening:
  # Training:
  #   c(1, 2), +3, *3, +2, *10 -> c(140, 170)
  #   first adder gets a meandata of 1.5, second adder gets meandata of 13.5
  # Prediction:
  #   140 * 100 + 4 * 9 + 6
  expect_identical(testglobalenv$cpotest.parvals, list(140, 3))
  expect_identical(pr$data$response, c(((140 * 100) + 4) * 9 + 6, 140))

})

test_that("CPO trafo functions work", {

  expect_error(makeCPOObject("testCPO", a: integer[, ], b: integer[, ],
    cpo.trafo = function(a) { }, cpo.retrafo = function(b, ...) { }), "Must have formal arguments")

  expect_error(makeCPOObject("testCPO", a: integer[, ], b: integer[, ],
    cpo.trafo = function(a, ...) { }, cpo.retrafo = function(b) { }), "Must have formal arguments")

  expect_class(makeCPOObject("testCPO", a: integer[, ], b: integer[, ],
    cpo.trafo = function(a, ...) { }, cpo.retrafo = function(b, ...) { }), "CPOConstructor")

  expect_error(makeCPOFunctional("testCPO", a: integer[, ], b: integer[, ],
    cpo.trafo = function(a) { }), "Must have formal arguments")

  expect_class(makeCPOFunctional("testCPO", a: integer[, ], b: integer[, ],
    cpo.trafo = function(a, ...) { }), "CPOConstructor")

  testglobalenv$cpotest.parvals = list()
  t = train(makeCPOObject("testCPO", a: integer[, ], b: integer[, ],
    cpo.trafo = function(data, target, b, ...) {
      testglobalenv$cpotest.parvals = list(b, slist(...))  # nolint
      control = 0
      data
    }, cpo.retrafo = function(data, b, ...) {
      testglobalenv$cpotest.parvals = list(b, slist(...))  # nolint
      data
    })(1, 2) %>>% makeLearner("classif.logreg"), pid.task)

  expect_identical(testglobalenv$cpotest.parvals, list(2, list(a = 1)))

  testglobalenv$cpotest.parvals = list()
  predict(t, pid.task)
  expect_identical(testglobalenv$cpotest.parvals, list(2, list(a = 1, control = 0)))

  testglobalenv$cpotest.parvals = list()
  t = train(makeCPOFunctional("testCPO", a: integer[, ], b: integer[, ],
    cpo.trafo = function(data, target, b, ...) {
      testglobalenv$cpotest.parvals = list(b, slist(...))  # nolint
      cpo.retrafo = function(data, ...) {
        testglobalenv$cpotest.parvals = list(b, slist(...))  # nolint
        data
      }
      data
    })(1, 2) %>>% makeLearner("classif.logreg"), pid.task)

  expect_identical(testglobalenv$cpotest.parvals, list(2, list(a = 1)))

  testglobalenv$cpotest.parvals = list()
  predict(t, pid.task)
  expect_identical(testglobalenv$cpotest.parvals, list(2, list()))

})

test_that("CPO arguments may be missing if requirements allow", {

  cpoc = makeCPOObject("testCPO", a = FALSE: logical, b: integer[, ] [[requires = quote(!(!a))]],
    cpo.trafo = {
      if (!a) {
        expect_true(missing(b))
      } else {
        expect_class(b, "integer")
      }
      control = 0
      data
    }, cpo.retrafo = {
      if (!a) {
        expect_true(missing(b))
      } else {
        expect_class(b, "integer")
      }
      data
    })

  testdfcpo %>>% cpoc()

  t = train(cpoc() %>>% makeLearner("classif.logreg"), pid.task)
  predict(t, pid.task)
  expect_error(train(cpoc(a = TRUE) %>>% makeLearner("classif.logreg"), pid.task), "Parameter .*b .*missing")
  t = train(cpoc(a = TRUE, b = 1L) %>>% makeLearner("classif.logreg"), pid.task)
  predict(t, pid.task)

  t = train(cpoc(id = "test") %>>% makeLearner("classif.logreg"), pid.task)
  predict(t, pid.task)
  expect_error(train(cpoc(a = TRUE, id = "test") %>>% makeLearner("classif.logreg"), pid.task), "Parameter test\\.b .*missing")
  t = train(cpoc(a = TRUE, b = 1L, id = "test") %>>% makeLearner("classif.logreg"), pid.task)
  predict(t, pid.task)

  expect_identical(getParamSet(cpoc(a = TRUE, id = "test"))$pars$test.b$requires, quote(!(!test.a)))


  cpoc = makeCPOFunctional("testCPO", a = FALSE: logical, b: integer[, ] [[requires = quote(!(!a))]],
    cpo.trafo = {
      if (!a) {
        expect_true(missing(b))
      } else {
        expect_class(b, "integer")
      }
      control = 0
      cpo.retrafo = function(data) data
      data
    })

  train(cpoc() %>>% makeLearner("classif.logreg"), pid.task)
  expect_error(train(cpoc(a = TRUE) %>>% makeLearner("classif.logreg"), pid.task), "Parameter .*b .*missing")
  train(cpoc(a = TRUE, b = 1L) %>>% makeLearner("classif.logreg"), pid.task)

  train(cpoc(id = "test") %>>% makeLearner("classif.logreg"), pid.task)
  expect_error(train(cpoc(a = TRUE, id = "test") %>>% makeLearner("classif.logreg"), pid.task), "Parameter test\\.b .*missing")
  train(cpoc(a = TRUE, b = 1L, id = "test") %>>% makeLearner("classif.logreg"), pid.task)

  expect_identical(getParamSet(cpoc(a = TRUE, id = "test"))$pars$test.b$requires, quote(!(!test.a)))
})

test_that("CPOs can be applied to data", {

  expect_identical(getTaskData(testtaskcpo %>>% cpomultiplier.f(10))$A, c(10, 20))
  expect_identical(getTaskData(testtaskcpo %>>% cpoadder.f(10))$A, c(11, 12))
  expect_identical(getTaskData(testtaskcpo %>>% cpoadder.f(10) %>>% cpomultiplier.f(10))$A, c(110, 120))
  expect_identical(getTaskData(testtaskcpo %>>% (cpoadder.f(10) %>>% cpomultiplier.f(10)))$A, c(110, 120))


  expect_identical(getTaskData(testtaskcpo %>>% cpomultiplier.o(10))$A, c(10, 20))
  expect_identical(getTaskData(testtaskcpo %>>% cpoadder.o(10))$A, c(11, 12))
  expect_identical(getTaskData(testtaskcpo %>>% cpoadder.o(10) %>>% cpomultiplier.o(10))$A, c(110, 120))
  expect_identical(getTaskData(testtaskcpo %>>% (cpoadder.o(10) %>>% cpomultiplier.o(10)))$A, c(110, 120))

  testdata = data.frame(A = c(1, 2))

  testdata %>>% cpomultiplier.f(10)

  expect_identical((testdata %>>% cpomultiplier.f(10))$A, c(10, 20))
  expect_identical((testdata %>>% cpoadder.f(10))$A, c(11, 12))
  expect_identical((testdata %>>% cpoadder.f(10) %>>% cpomultiplier.f(10))$A, c(110, 120))
  expect_identical((testdata %>>% (cpoadder.f(10) %>>% cpomultiplier.f(10)))$A, c(110, 120))


  expect_identical((testdata %>>% cpomultiplier.o(10))$A, c(10, 20))
  expect_identical((testdata %>>% cpoadder.o(10))$A, c(11, 12))
  expect_identical((testdata %>>% cpoadder.o(10) %>>% cpomultiplier.o(10))$A, c(110, 120))
  expect_identical((testdata %>>% (cpoadder.o(10) %>>% cpomultiplier.o(10)))$A, c(110, 120))

})

test_that("retrafo accessor does what it is supposed to do", {

  expect_identical(retrafo(pid.task), NULLCPO)

  expect_warning(expect_identical(retrafo(10), NULLCPO), "not a Task or data.frame")

  x = 10
  expect_warning(expect_identical(retrafo(x), NULLCPO), "not a Task or data.frame")

  tmpret = retrafo(pid.task %>>% cpoScale())
  expect_warning({retrafo(x) = tmpret}, "Task nor data.frame")

  expect_warning(expect_identical(retrafo(x), tmpret), "not a Task or data.frame")

  transformed = pid.task %>>% cpoScale()

  expect_class(retrafo(transformed), "CPORetrafo")

  expect_equal(getTaskData(pid.task %>>% retrafo(transformed)), getTaskData(transformed))

  testglobalenv$cpotest.parvals = list()  # nolint
  t = train(testlearnercpo, testtaskcpo)
  predict(t, testtaskcpo2)
  expect_identical(testglobalenv$cpotest.parvals, list(1, 3))

  f1 = function(data, target, args) {
    data[[1]] = data[[1]] * 10
    return(list(data = data, control = list()))
  }

  f2 = function(data, target, args, control) {
    data[[1]] = data[[1]] / 10
    return(data)
  }
  wrappedlearner = makePreprocWrapper(testlearnercpo, train = f1, predict = f2, par.set = makeParamSet(), par.vals = list())


  testCPO = function(cpoadder, cpomultiplier) {
    # short chain, task
    result = testtaskcpo %>>% cpoadder(10)
    expect_identical(getTaskData(result)$A, c(11, 12))
    expect_equal(getTaskData(testtaskcpo2 %>>% retrafo(result))$A, c(-8.5, -7.5))


    # short chain, data.frame
    result = testdfcpo %>>% cpoadder(10)
    expect_identical(result$A, c(11, 12))
    expect_equal(getTaskData(testtaskcpo2 %>>% retrafo(result))$A, c(-8.5, -7.5))
    expect_equal((testdfcpo2 %>>% retrafo(result))$A, c(-8.5, -7.5))

    # long chain, task
    result = (testtaskcpo %>>% cpoadder(10) %>>% cpomultiplier(2)) %>>% (cpoadder(-10) %>>% cpomultiplier(2))
    expect_equal(getTaskData(result)$A, c(24, 28))
    expect_equal(getTaskData(testtaskcpo2 %>>% retrafo(result))$A, ((c(3, 4) - 10 - 1.5) / 2 + 10 - 23) / 2)

    # long chain, data.frame
    result = (testdfcpo %>>% cpoadder(10) %>>% cpomultiplier(2)) %>>% (cpoadder(-10) %>>% cpomultiplier(2))
    expect_equal(result$A, c(24, 28))
    expect_equal(getTaskData(testtaskcpo2 %>>% retrafo(result))$A, ((c(3, 4) - 10 - 1.5) / 2 + 10 - 23) / 2)
    expect_equal((testdfcpo2 %>>% retrafo(result))$A, ((c(3, 4) - 10 - 1.5) / 2 + 10 - 23) / 2)

    # short chain, learner model
    testglobalenv$cpotest.parvals = list()  # nolint
    m = train(cpoadder(10) %>>% testlearnercpo, testtaskcpo)

    expect_equal(testglobalenv$cpotest.parvals, list(11))
    expect_equal(getTaskData(testtaskcpo2 %>>% retrafo(m))$A, c(-8.5, -7.5))
    expect_equal((testdfcpo2 %>>% retrafo(m))$A, c(-8.5, -7.5))
    predict(m, testtaskcpo2)
    expect_equal(testglobalenv$cpotest.parvals, list(11, -8.5))


    # long chain, learner model
    testglobalenv$cpotest.parvals = list()  # nolint
    m = train((cpoadder(10, id = "fst") %>>% cpomultiplier(2, id = "snd")) %>>%
              ((cpoadder(-10, id = "thd") %>>% cpomultiplier(2, id = "frth")) %>>% testlearnercpo), testtaskcpo)

    expect_equal(testglobalenv$cpotest.parvals,  list(24))
    expect_equal(getTaskData(testtaskcpo2 %>>% retrafo(m))$A, ((c(3, 4) - 10 - 1.5) / 2 + 10 - 23) / 2)
    expect_equal((testdfcpo2 %>>% retrafo(result))$A, ((c(3, 4) - 10 - 1.5) / 2 + 10 - 23) / 2)
    predict(m, testtaskcpo2)
    expect_equal(testglobalenv$cpotest.parvals, list(24, ((3 - 10 - 1.5) / 2 + 10 - 23) / 2))

    # message when learner contains something else
    # THIS WILL NOT WORK WHEN PREPROC WRAPPERS ARE GONE!
    testglobalenv$cpotest.parvals = list()  # nolint
    m = train((cpoadder(10, id = "fst") %>>% cpomultiplier(2, id = "snd")) %>>%
              ((cpoadder(-10, id = "thd") %>>% cpomultiplier(2, id = "frth")) %>>% wrappedlearner), testtaskcpo)
    expect_equal(testglobalenv$cpotest.parvals, list(240))

    expect_message({ retr = retrafo(m) }, "has some wrappers besides CPOs", all = TRUE)
    expect_equal(getTaskData(predict(retr, testtaskcpo2))$A, ((c(3, 4) - 10 - 1.5) / 2 + 10 - 23) / 2)
    expect_equal(predict(retr, testdfcpo2)$A, ((c(3, 4) - 10 - 1.5) / 2 + 10 - 23) / 2)
    predict(m, testtaskcpo2)
    expect_equal(testglobalenv$cpotest.parvals, list(240, (((3 - 10 - 1.5) / 2 + 10 - 23) / 2) / 10))

    # warning when learner contains buried CPOs
    # THIS WILL NOT HAPPEN WHEN PREPROC WRAPPERS ARE GONE!
    buriedlearner = makePreprocWrapper(cpoadder(-10, id = "thd") %>>% (cpomultiplier(2, id = "frth") %>>% testlearnercpo),
      train = f1, predict = f2, par.set = makeParamSet(), par.vals = list())

    testglobalenv$cpotest.parvals = list()  # nolint
    m = train((cpoadder(10, id = "fst") %>>% (cpomultiplier(2, id = "snd")) %>>% buriedlearner), testtaskcpo)
    expect_equal(testglobalenv$cpotest.parvals, list((11 * 2 * 10 - 10) * 2))

    expect_warning({ retr = retrafo(m) }, "has some CPOs wrapped by other wrappers", all = TRUE)
    expect_equal(getTaskData(predict(retr, testtaskcpo2))$A, ((c(3, 4) - 10 - 1.5) / 2))
    expect_equal(predict(retr, testdfcpo2)$A, ((c(3, 4) - 10 - 1.5) / 2))
    predict(m, testtaskcpo2)
    expect_equal(testglobalenv$cpotest.parvals, list((11 * 2 * 10 - 10) * 2, (((3 - 10 - 1.5) / 2 / 10 + 10 - 230) / 2)))
  }

  testCPO(cpoadder.f, cpomultiplier.f)

  testCPO(cpoadder.o, cpomultiplier.o)

})

test_that("functional trafo and retrafo return values are checked", {

  cpoone.f = makeCPOFunctional("one", a: logical, cpo.trafo = {
    cpo.retrafo = identity
    data
  })

  cpotwo.f = makeCPOFunctional("two", b: logical, cpo.trafo = {
    cpo.retrafo = identity
    data
  })

  cpobad.trafo.f = makeCPOFunctional("badtrafo", c: logical, cpo.trafo = {
    cpo.retrafo = identity
    data[[1]]
  })

  cpobad.retrafo.f = makeCPOFunctional("badretrafo", d: logical, cpo.trafo = {
    cpo.retrafo = function(data) data[[1]]
    data
  })

  expect_class(pid.task %>>% cpoone.f(TRUE) %>>% cpotwo.f(TRUE), "Task")
  expect_error(pid.task %>>% cpoone.f(TRUE) %>>% cpotwo.f(TRUE) %>>% cpobad.trafo.f(TRUE), "CPO badtrafo cpo\\.trafo .*cpo.trafo must return a data.frame")
  expect_error(pid.task %>>% cpoone.f(TRUE) %>>% cpobad.trafo.f(TRUE) %>>% cpotwo.f(TRUE), "CPO badtrafo cpo\\.trafo .*cpo.trafo must return a data.frame")
  expect_error(pid.task %>>% cpobad.trafo.f(TRUE) %>>% cpoone.f(TRUE) %>>% cpotwo.f(TRUE), "CPO badtrafo cpo\\.trafo .*cpo.trafo must return a data.frame")

  expect_class({res = pid.task %>>% cpoone.f(TRUE) %>>% cpotwo.f(TRUE) %>>% cpobad.retrafo.f(TRUE)}, "Task")
  expect_error(pid.task %>>% retrafo(res), "badretrafo .*must return a data.frame")
  expect_class({res = pid.task %>>% cpoone.f(TRUE) %>>% cpobad.retrafo.f(TRUE) %>>% cpotwo.f(TRUE)}, "Task")
  expect_error(pid.task %>>% retrafo(res), "badretrafo .*must return a data.frame")
  expect_class({res = pid.task %>>% cpobad.retrafo.f(TRUE) %>>% cpoone.f(TRUE) %>>% cpotwo.f(TRUE)}, "Task")
  expect_error(pid.task %>>% retrafo(res), "badretrafo .*must return a data.frame")
  expect_class({res = pid.task %>>% (cpobad.retrafo.f(TRUE) %>>% cpoone.f(TRUE) %>>% cpotwo.f(TRUE))}, "Task")
  expect_error(pid.task %>>% retrafo(res), "badretrafo .*must return a data.frame")

  expect_class(train(cpoone.f(TRUE) %>>% cpotwo.f(TRUE) %>>% makeLearner("classif.logreg"), pid.task), "WrappedModel")
  expect_error(train(cpoone.f(TRUE) %>>% cpotwo.f(TRUE) %>>%
                     cpobad.trafo.f(TRUE) %>>% makeLearner("classif.logreg"), pid.task),
    "CPO badtrafo cpo\\.trafo .*cpo.trafo must return a data.frame")
  expect_error(train(cpoone.f(TRUE) %>>% cpobad.trafo.f(TRUE) %>>%
                     cpotwo.f(TRUE) %>>% makeLearner("classif.logreg"), pid.task),
    "CPO badtrafo cpo\\.trafo .*cpo.trafo must return a data.frame")
  expect_error(train(cpobad.trafo.f(TRUE) %>>% cpoone.f(TRUE) %>>%
                     cpotwo.f(TRUE) %>>% makeLearner("classif.logreg"), pid.task),
    "CPO badtrafo cpo\\.trafo .*cpo.trafo must return a data.frame")


  expect_class({res = train(cpoone.f(TRUE) %>>% cpotwo.f(TRUE) %>>% cpobad.retrafo.f(TRUE) %>>%
                            makeLearner("classif.logreg"), pid.task)}, "WrappedModel")
  expect_error(predict(res, pid.task), "badretrafo .*must return a data.frame")
  expect_class({res = train(cpoone.f(TRUE) %>>% cpobad.retrafo.f(TRUE) %>>% cpotwo.f(TRUE) %>>%
                            makeLearner("classif.logreg"), pid.task)}, "WrappedModel")
  expect_error(predict(res, pid.task), "badretrafo .*must return a data.frame")
  expect_class({res = train(cpobad.retrafo.f(TRUE) %>>% cpoone.f(TRUE) %>>% cpotwo.f(TRUE) %>>%
                            makeLearner("classif.logreg"), pid.task)}, "WrappedModel")
  expect_error(predict(res, pid.task), "badretrafo .*must return a data.frame")
})

test_that("object based trafo and retrafo return values are checked", {

  cpoone.o = makeCPOObject("one", a: logical, cpo.trafo = {
    control = 0
    data
  }, cpo.retrafo = {
    data
  })

  cpotwo.o = makeCPOObject("two", b: logical, cpo.trafo = {
    control = 0
    data
  }, cpo.retrafo = {
    data
  })

  cpobad.trafo.o = makeCPOObject("badtrafo", c: logical, cpo.trafo = {
    data
  }, cpo.retrafo = {
    data
  })

  cpobad.retrafo.o = makeCPOObject("badretrafo", d: logical, cpo.trafo = {
    control = 0
    data
  }, cpo.retrafo = {
    data[[1]]
  })

  expect_class(pid.task %>>% cpoone.o(TRUE) %>>% cpotwo.o(TRUE), "Task")
  expect_error(pid.task %>>% cpoone.o(TRUE) %>>% cpotwo.o(TRUE) %>>% cpobad.trafo.o(TRUE), "CPO's cpo\\.trafo did not create a 'control'")
  expect_error(pid.task %>>% cpoone.o(TRUE) %>>% cpobad.trafo.o(TRUE) %>>% cpotwo.o(TRUE), "CPO's cpo\\.trafo did not create a 'control'")
  expect_error(pid.task %>>% cpobad.trafo.o(TRUE) %>>% cpoone.o(TRUE) %>>% cpotwo.o(TRUE), "CPO's cpo\\.trafo did not create a 'control'")

  expect_class({res = pid.task %>>% cpoone.o(TRUE) %>>% cpotwo.o(TRUE) %>>% cpobad.retrafo.o(TRUE)}, "Task")
  expect_error(pid.task %>>% retrafo(res), "badretrafo .*must return a data.frame")
  expect_class({res = pid.task %>>% cpoone.o(TRUE) %>>% cpobad.retrafo.o(TRUE) %>>% cpotwo.o(TRUE)}, "Task")
  expect_error(pid.task %>>% retrafo(res), "badretrafo .*must return a data.frame")
  expect_class({res = pid.task %>>% cpobad.retrafo.o(TRUE) %>>% cpoone.o(TRUE) %>>% cpotwo.o(TRUE)}, "Task")
  expect_error(pid.task %>>% retrafo(res), "badretrafo .*must return a data.frame")
  expect_class({res = pid.task %>>% (cpobad.retrafo.o(TRUE) %>>% cpoone.o(TRUE) %>>% cpotwo.o(TRUE))}, "Task")
  expect_error(pid.task %>>% retrafo(res), "badretrafo .*must return a data.frame")

  expect_class(train(cpoone.o(TRUE) %>>% cpotwo.o(TRUE) %>>% makeLearner("classif.logreg"), pid.task), "WrappedModel")
  expect_error(train(cpoone.o(TRUE) %>>% cpotwo.o(TRUE) %>>%
                     cpobad.trafo.o(TRUE) %>>% makeLearner("classif.logreg"), pid.task),
    "CPO's cpo\\.trafo did not create a 'control'")
  expect_error(train(cpoone.o(TRUE) %>>% cpobad.trafo.o(TRUE) %>>%
                     cpotwo.o(TRUE) %>>% makeLearner("classif.logreg"), pid.task),
    "CPO's cpo\\.trafo did not create a 'control'")
  expect_error(train(cpobad.trafo.o(TRUE) %>>% cpoone.o(TRUE) %>>%
                     cpotwo.o(TRUE) %>>% makeLearner("classif.logreg"), pid.task),
    "CPO's cpo\\.trafo did not create a 'control'")

  expect_class({res = train(cpoone.o(TRUE) %>>% cpotwo.o(TRUE) %>>% cpobad.retrafo.o(TRUE) %>>%
                            makeLearner("classif.logreg"), pid.task)}, "WrappedModel")
  expect_error(predict(res, pid.task), "badretrafo .*must return a data.frame")
  expect_class({res = train(cpoone.o(TRUE) %>>% cpobad.retrafo.o(TRUE) %>>% cpotwo.o(TRUE) %>>%
                            makeLearner("classif.logreg"), pid.task)}, "WrappedModel")
  expect_error(predict(res, pid.task), "badretrafo .*must return a data.frame")
  expect_class({res = train(cpobad.retrafo.o(TRUE) %>>% cpoone.o(TRUE) %>>% cpotwo.o(TRUE) %>>%
                            makeLearner("classif.logreg"), pid.task)}, "WrappedModel")
  expect_error(predict(res, pid.task), "badretrafo .*must return a data.frame")
})

test_that("to.list and pipeCPO work", {

  testCPO = function(cpoadder, cpomultiplier) {

    cpochain = ((cpoadder(10, id = "fst") %>>% cpomultiplier(2, id = "snd")) %>>%
                (cpoadder(-10, id = "thd") %>>% cpomultiplier(2, id = "frth")))


    result = testdfcpo %>>% cpochain
    expect_equal(result$A, c(24, 28))
    expect_equal((testdfcpo2 %>>% retrafo(result))$A, ((c(3, 4) - 10 - 1.5) / 2 + 10 - 23) / 2)

    cpolist = as.list(cpochain)
    expect_list(cpolist, len = 4)
    expect_equal(cpolist[[1]], cpoadder(10, id = "fst"))
    expect_equal(cpolist[[2]], cpomultiplier(2, id = "snd"))
    expect_equal(cpolist[[3]], cpoadder(-10, id = "thd"))
    expect_equal(cpolist[[4]], cpomultiplier(2, id = "frth"))

    result = testdfcpo %>>% pipeCPO(cpolist)
    expect_equal(result$A, c(24, 28))
    expect_equal((testdfcpo2 %>>% retrafo(result))$A, ((c(3, 4) - 10 - 1.5) / 2 + 10 - 23) / 2)

    expect_equal(cpolist, as.list(pipeCPO(cpolist)))

    cpolist.chg = as.list(setHyperPars(cpochain, fst.summand = 20))
    expect_equal(cpolist.chg[[1]], cpoadder(20, id = "fst"))

    cpolist.chg[[2]] = setHyperPars(cpolist.chg[[2]], snd.factor = 10)
    result = testdfcpo %>>% pipeCPO(cpolist.chg)
    expect_equal(result$A, ((c(1, 2) + 20) * 10 - 10) * 2)
    expect_equal((testdfcpo2 %>>% retrafo(result))$A, ((c(3, 4) - 20 - 1.5) / 10 + 10 - 215) / 2)

  }

  testCPO(cpoadder.f, cpomultiplier.f)
  testCPO(cpoadder.o, cpomultiplier.o)
})


test_that("retrafo catabolization and anabolization work", {

  cpoadder = cpoadder.o
  cpomultiplier = cpomultiplier.o

  testCPO = function(cpoadder, cpomultiplier) {

    cpochain = ((cpoadder(20, id = "fst") %>>% cpomultiplier(2, id = "snd")) %>>%
                (cpoadder(-10, id = "thd") %>>% cpomultiplier(2, id = "frth")) %>>%
                cpoadder(10) %>>% cpomultiplier(2))

    cpochain = setHyperPars(cpochain, frth.factor = -2)

    res = testdfcpo %>>% cpochain

    expect_equal(res[[1]], (((c(1, 2) + 20) * 2 - 10) * -2 + 10) * 2)


    expect_equal((testdfcpo2 %>>% retrafo(res))[[1]], (((c(3, 4) - 20 - 1.5) / 2 + 10 - 43) / -2 - 10 + 66) / 2)

    lrn = cpomultiplier(2, id = "a") %>>% cpomultiplier(0.5, id = "b") %>>% cpochain %>>% testlearnercpo

    retrafochain = retrafo(train(setHyperPars(lrn, fst.summand = 10), testtaskcpo))

    rfclist = as.list(retrafochain)

    expect_equal(getCPOTrainedState(rfclist[[1]])$factor, 2)
    expect_equal(getCPOTrainedState(rfclist[[2]])$factor, 0.5)
    expect_equal(getCPOTrainedState(rfclist[[3]])$summand, 10)
    expect_equal(getCPOTrainedState(rfclist[[6]])$factor, -2)

    rfclist.states = lapply(rfclist, getCPOTrainedState)

    constructors = list(cpomultiplier, cpomultiplier, cpoadder, cpomultiplier, cpoadder, cpomultiplier, cpoadder, cpomultiplier)
    rfclist2 = lapply(seq_along(constructors), function(idx) makeCPOTrainedFromState(constructors[[idx]], rfclist.states[[idx]]))

    expect_equal(predict(pipeCPO(rfclist), testdfcpo2)[[1]], (((c(3, 4) - 10 - 1.5) / 2 + 10 - 23) / -2 - 10 + 26) / 2)
    expect_equal(predict(pipeCPO(rfclist2), testdfcpo2)[[1]], (((c(3, 4) - 10 - 1.5) / 2 + 10 - 23) / -2 - 10 + 26) / 2)

    expect_equal(predict(pipeCPO(rfclist[c(1:5)]) %>>% pipeCPO(rfclist[c(6:8)]), testdfcpo2)[[1]], (((c(3, 4) - 10 - 1.5) / 2 + 10 - 23) / -2 - 10 + 26) / 2)
    expect_equal(predict(pipeCPO(rfclist2[c(1:5)]) %>>% pipeCPO(rfclist2[c(6:8)]), testdfcpo2)[[1]], (((c(3, 4) - 10 - 1.5) / 2 + 10 - 23) / -2 - 10 + 26) / 2)

    rfclist.states2 = rfclist.states
    rfclist.states2[[1]]$factor = 4
    rfclist2 = lapply(seq_along(constructors), function(idx) makeCPOTrainedFromState(constructors[[idx]], rfclist.states2[[idx]]))
    expect_equal(predict(pipeCPO(rfclist2), testdfcpo2)[[1]], (((c(3, 4) / 2 - 10 - 1.5) / 2 + 10 - 23) / -2 - 10 + 26) / 2)

    chain2 = pipeCPO(rfclist[c(1, 2, 3)]) %>>% pipeCPO(rfclist[c(4, 5, 6, 7, 8)])

    expect_equal(predict(chain2, testdfcpo2)[[1]], (((c(3, 4) - 10 - 1.5) / 2 + 10 - 23) / -2 - 10 + 26) / 2)

    chain3 = pipeCPO(rfclist[3:4]) %>>% pipeCPO(rfclist[c(5:6, 1:2)]) %>>% pipeCPO(rfclist[7:8])

    expect_equal(predict(chain3, testdfcpo2)[[1]], (((c(3, 4) - 10 - 1.5) / 2 + 10 - 23) / -2 - 10 + 26) / 2)

    expect_equal((testdfcpo2 %>>% pipeCPO(rfclist[7:8]) %>>% pipeCPO(rfclist[5:6]) %>>% pipeCPO(rfclist[1:2]) %>>% pipeCPO(rfclist[3:4]))[[1]],
      (((c(3, 4) - 10 + 26) / 2 + 10 - 23) / -2 - 10 - 1.5) / 2)
    expect_equal((testdfcpo2 %>>% pipeCPO(rfclist[c(7, 8, 5, 6, 3, 4, 1, 2)]))[[1]], (((c(3, 4) - 10 + 26) / 2 + 10 - 23) / -2 - 10 - 1.5) / 2)
    chained.again = pipeCPO(rfclist[7:8]) %>>% (pipeCPO(rfclist[5:6]) %>>% pipeCPO(rfclist[1:4]))
    expect_equal((testdfcpo2 %>>% chained.again)[[1]], (((c(3, 4) - 10 + 26) / 2 + 10 - 23) / -2 - 10 - 1.5) / 2)

    expect_error(setHyperPars(as.list(chained.again)[[1]], summand = 10), "Cannot change parameter values")
    expect_error(removeHyperPars(as.list(chained.again)[[1]], "summand"))

    expect_error(getHyperPars(chained.again), "Cannot get parameters of compound retrafo")
    expect_error(getParamSet(chained.again), "Cannot get param set of compound retrafo")

    expect_identical(getHyperPars(as.list(chained.again)[[1]]), list(summand = 10))
    expect_identical(getHyperPars(as.list(chained.again)[[7]]), list(summand = 10))
    expect_identical(getHyperPars(as.list(chained.again)[[4]]), list(factor = -2))

    expect_identical(getParamSet(as.list(chained.again)[[1]]), pSSLrn(summand = 1: integer[, ]))
    expect_identical(getParamSet(as.list(chained.again)[[7]]), pSSLrn(summand = 1: integer[, ]))
    expect_identical(getParamSet(as.list(chained.again)[[4]]), pSSLrn(factor = 1: numeric[~., ~.]))

    testdfcpo %>>% pipeCPO(as.list(cpochain)[1:3])
    firsthalf = retrafo(testdfcpo %>>% pipeCPO(as.list(cpochain)[1:3]))
    secondhalf = retrafo(testdfcpo %>>% pipeCPO(as.list(cpochain)[4:6]))

    expect_equal((testdfcpo2 %>>% firsthalf)[[1]], (c(3, 4) - 20 - 1.5) / 2 + 10 - 43)
    expect_equal((testdfcpo2 %>>% secondhalf)[[1]], (c(3, 4) / -2 - 10 + 3) / 2)

    expect_equal((testdfcpo2 %>>% (firsthalf %>>% secondhalf))[[1]], (((c(3, 4) - 20 - 1.5) / 2 + 10 - 43) / -2 - 10 + 3) / 2)

    testglobalenv$cpotest.parvals = list()  # nolint
    retrafos = list(
        (testdfcpo %>>% cpoadder(10, id = "fst")) %>>% cpomultiplier(2, id = "snd"),  # apply CPO twice
        testdfcpo %>>% (cpoadder(10, id = "fst") %>>% cpomultiplier(2, id = "snd")),  # apply compound CPO
        train(setHyperPars(cpoadder(1, id = "fst") %>>% (cpomultiplier(1, id = "snd") %>>%
                                                         testlearnercpo), fst.summand = 10, snd.factor = 2), testtaskcpo), # wrap with CPO twice
        train(setHyperPars((cpoadder(1, id = "fst") %>>% cpomultiplier(1, id = "snd")), fst.summand = 10, snd.factor = 2) %>>%
              testlearnercpo, testtaskcpo))  # wrap with compound CPO

    expect_equal(testglobalenv$cpotest.parvals, list(22, 22))


    for (retgen in retrafos) {
      ret = retrafo(retgen)
      expect_equal((testdfcpo2 %>>% ret)[[1]], (c(3, 4) - 10 - 1.5) / 2)
      expect_equal((testdfcpo2 %>>% as.list(ret)[[1]])[[1]], c(3, 4) - 10 - 1.5)
      expect_equal((testdfcpo2 %>>% as.list(ret)[[2]])[[1]], c(3, 4) / 2)
      expect_equal((testdfcpo2 %>>% pipeCPO(as.list(ret)[c(2, 1)]))[[1]], c(3, 4) / 2 - 10 - 1.5)
    }
  }

  testCPO(cpoadder.o, cpomultiplier.o)
  testCPO(cpoadder.f, cpomultiplier.f)


})



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

  at(lung.task)  # surv
  at(yeast.task)  # multilabel
  at(bh.task)  # regr
  at(pid.task)  # classif, binary
  at(iris.task)  # classif, multiclass
  at(agri.task)  # cluster

})

