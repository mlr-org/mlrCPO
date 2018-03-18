
context("ParamSetSugar")

test_that("ParamSetSugar generates the expected ParamSets", {

  param.list = list(a = "b", b = "a")

  x = 10

  # list of pairs that whould have identical elements
  param.sets = list(

      # empty defaults
      list(makeParamSet(makeIntegerLearnerParam("a"), makeIntegerLearnerParam("b"), makeIntegerLearnerParam("c")),
        pSSLrn(a: integer[, ], b = NA: integer[, ], c: integer)),

      # discrete parameters
      list(makeParamSet(makeDiscreteLearnerParam("b", c("a", "b", "c")),
        makeDiscreteLearnerParam("c", list(a = 1, b = 2, c = 3), default = 1),
        makeDiscreteVectorLearnerParam("d", 2, list(a = "a", b = "b", `1` = 1), default = list("a", 1)),
        makeDiscreteLearnerParam("e", param.list, when = "both", requires = expression(b == "a"), default = "b")),
        pSSLrn(b: discrete[a, b, c],
          c = 1: discrete[a = 1, b = 2, c = 3],
          d = list("a", 1): discrete[a, b, 1]^2,
          e = "b": discrete[param.list] [[when = "both", requires = expression(b == "a")]])),

      # ranges, extra parameters
      list(makeParamSet(makeNumericParam("a", allow.inf = TRUE, default = 1, tunable = FALSE),
        makeNumericParam("b", lower = 0, allow.inf = TRUE, default = 2),
        makeNumericVectorParam("c", 3, upper = 0, allow.inf = FALSE, special.vals = list(-1)),
        makeNumericVectorParam("d", 2, lower = 0, upper = 1, default = c(0.5, 0.5), requires = expression(a == 0))),
        pSSLrn(a = 1: numeric [[tunable = FALSE]],
          b = 2: numeric[0, ],
          c: numeric[~., x - 10] ^ (2 + 1) [[special.vals = list(1 - 2)]],
          d = c(1 / 2, 0.5): numeric[0, 1]^2 [[requires = expression(a == 0), allow.inf = FALSE]], .pss.learner.params = FALSE)),

      # empty parameter set
      list(makeParamSet(),
        pSSLrn())

  )

  for (ps.pair in param.sets) {
    expect_identical(ps.pair[[1]], ps.pair[[2]])
  }

})

test_that("ParamSetSugar works with params that have no bounds", {

  ps.sugar = pSS(a = TRUE: logical, b = identity: funct, c = "test": character, d = list(1, 2, 3): untyped, e = 1: numeric, f = 0: integer)

  ps.normal = makeParamSet(
      makeLogicalParam("a", TRUE),
      makeFunctionParam("b", identity),
      makeCharacterParam("c", "test"),
      makeUntypedParam("d", list(1, 2, 3)),
      makeNumericParam("e", default = 1, allow.inf = TRUE),
      makeIntegerParam("f", default = 0))

  expect_identical(ps.sugar, ps.normal)

})

test_that("ParamSetSugar handles exotic bounds well", {

  # list of triplets: (param set, feasible example points, infeasible example points).

  rangetests = list(
      list(pSSLrn(a: integer[, ]),
        list(list(a = -1), list(a = 0), list(a = 2^30)),
        list(list(a = -Inf), list(a = Inf), list(a = 0.5))),
      list(pSSLrn(a: numeric[, ]),
        list(list(a = 0), list(a = .Machine$double.xmax), list(a = -Inf)),
        list()),
      list(pSSLrn(a: integer[0, ]),
        list(list(a = 0), list(a = 1), list(a = 100)),
        list(list(a = -1), list(a = 0.5))),
      list(pSSLrn(a: numeric[0, ~.]),
        list(list(a = 0), list(a = .Machine$double.xmax)),
        list(list(a = -1), list(a = Inf))),
      list(pSSLrn(a: numeric[~0, ]),
        list(list(a = Inf), list(a = 1), list(a = .Machine$double.eps)),
        list(list(a = -1), list(a = 0), list(a = -Inf))),
      list(pSSLrn(a: numeric[~., ]),
        list(list(a = Inf), list(a = -.Machine$double.xmax)),
        list(list(a = -Inf))),
      list(pSSLrn(a: numeric[~., ~Inf]),
        list(list(a = 0), list(a = .Machine$double.xmax)),
        list(list(a = Inf), list(a = -Inf))),
      list(pSSLrn(a: numeric[~0, 1]),
        list(list(a = .Machine$double.eps), list(a = 1), list(a = 0.5)),
        list(list(a = 0), list(a = -Inf), list(a = Inf), list(a = 1 + .Machine$double.eps))),
      list(pSSLrn(a: discrete[1, 2, 3]),
        list(list(a = 1), list(a = 2), list(a = 3)),
        list(list(a = "1"), list(a = 4))),
      list(pSSLrn(a: discrete[a = "b", b = "c"]),
        list(list(a = "b"), list(a = "c")),
        list(list(a = "a"))),
      list(pSSLrn(a: numeric[~1, ~2^30]),
        list(list(a = 2), list(a = 2^30 - 1)),
        list(list(a = 1), list(a = 2^30)))
  )

  for (rt in rangetests) {
    for (feas in rt[[2]]) {
      expect_true(isFeasible(rt[[1]], feas))
    }
    for (infeas in rt[[3]]) {
      expect_false(isFeasible(rt[[1]], infeas))
    }
  }

})

test_that("Finite exclusive bounds work", {

  param = pSS(test: integer[~0, ~4])$pars[[1]]
  expect_identical(param$lower, 1)
  expect_identical(param$upper, 3)

  param = pSS(test: numeric[~0, ~4])
  expect_false(isFeasible(param, list(test = 0)))
  expect_true(isFeasible(param, list(test = 0.0001)))
  expect_true(isFeasible(param, list(test = 1e-100)))

  expect_false(isFeasible(param, list(test = 4)))
  expect_true(isFeasible(param, list(test = 4 - .Machine$double.eps * 2)))
  expect_false(isFeasible(param, list(test = 4 - .Machine$double.eps)))  # 4 - machine eps == 4 in double arithmetic


})

test_that("ParamSetSugar works when called indirectly", {

  pss2 = function(..., env) {
    x = 3
    pSSLrn(..., .pss.env = env)
  }

  pss1 = function(...) {
    x = 2
    pss2(..., env = parent.frame())
  }

  x = 1
  expect_identical(pss1(.pss.learner.params = FALSE, a = x: integer[x, x * 2]^x [[tunable = (x == 2)]]),
    makeParamSet(makeIntegerVectorParam("a", 1, 1, 2, default = 1, tunable = FALSE)))
})

test_that("ParamSetSugar gives error messages", {

  expect_error(pSS(test), ":` was missing")
  expect_error(pSS(1 + 1), ":` was missing")
  expect_error(pSS("a: integer"), ":` was missing")

  expect_error(pSS(x: function(){}), "Unknown parameter type function")

  expect_error(pSSLrn(x: character), "'character' not allowed for Learner params.")
  expect_identical(pSS(x: character), makeParamSet(makeCharacterParam("x")))

  expect_error(pSS(x: nonexistent), "Unknown parameter type nonexistent")

  expect_error(pSS(x: funct[1, 2]), "'funct' parameter may not have range parameter")

  expect_error(pSS(x: discrete), "discrete parameter must be of the form.*Range suffix.*is missing")

  expect_error(pSS(x: funct ^ list), "`\\^` found, but exponent list did not eval to numeric")

  expect_error(pSS(x: discrete[1, 2, 1 + 1]), "value list .*1 \\+ 1) invalid")

  expect_error(pSS(x: numeric[1, 2, 3]), "invalid numeric / integer range")

  expect_error(pSS(x: integer[~., ]), '"~."-bounds \\(unbounded but excluding "Inf"\\) are only allowed for "numeric" variables.')

})

