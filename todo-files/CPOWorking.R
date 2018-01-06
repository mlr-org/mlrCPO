
library("roxygen2")


roxygenise("..")


debugger()

devtools::load_all("../../ParamHelpers")
devtools::load_all("../../mlr")


devtools::load_all("..", export_all = FALSE)

devtools::load_all("..", export_all = FALSE)

devtools::build_vignettes("..")


devtools::load_all("..")

library("checkmate")




options(error = dump.frames)


options(max.print = 2000)


configureMlr(show.info = TRUE, on.learner.error = "stop", show.learner.output = TRUE)

devtools::load_all("..")



devtools::test(pkg = "..", filter = "^_meta_")



# all tests:
devtools::test(pkg = "..", filter = "affect")       # [ 00 ]
devtools::test(pkg = "..", filter = "basic")        # [ 00 ]
devtools::test(pkg = "..", filter = "cbind")        # [ 00 ]
devtools::test(pkg = "..", filter = "concrete")     # [ 00 ]
devtools::test(pkg = "..", filter = "datasplit")    # [ 00 ]
devtools::test(pkg = "..", filter = "filter")       # [ 00 ]
devtools::test(pkg = "..", filter = "impute")       # [ 00 ]
devtools::test(pkg = "..", filter = "meta")         # [ 00 ]
devtools::test(pkg = "..", filter = "properties")   # [ 00 ]
devtools::test(pkg = "..", filter = "quick")        # [ 00 ]
devtools::test(pkg = "..", filter = "tuning")       # [ 00 ]


devtools::test(pkg = "..", filter = "dataflow")


devtools::test(pkg = "..", filter = "lint")         # [ ?? ]

devtools::test(pkg = "..")


system.time(devtools::test(pkg = "..", filter = "cpo_dataformat"), gcFirst = FALSE)

###################################
devtools::test(pkg = "..", filter = "cpo$", reporter = c("summary", "stop"))

system.time(devtools::test(pkg = "..", filter = "cpo_dataformat", reporter = c("summary", "stop")), gcFirst = FALSE)
devtools::test(pkg = "..", filter = "ParamSetSugar")

rm(list = ls())

tst = makeCPOTargetOp("test", .type = "regr",
                      cpo.trafo = {
                        control = 0
                        data.frame(target = target + 1000)
                      }, cpo.retrafo = { data })

df = data.frame(a = c(1, 2, 1), b = c(1, , 1))

getTaskData(df %>>% tst(), target.extra = TRUE)

x = train(tst() %>>% makeLearner("regr.lm"), makeRegrTask(data = df, target = "a"))

x$learner.model$next.model$learner.model

getLearnerType(makeLearner("classif.logreg"))



#####
# try to find out what return values different learners use

ll = listLearners(create = TRUE)

tasks = list(classif = pid.task, regr = bh.task, surv = lung.task, cluster = mtcars.task, multilabel = agri.task)

configureMlr(on.learner.error = "warn", on.learner.warning = "warn", show.learner.output = TRUE, show.info = TRUE)

cll = Filter(function(x) getLearnerType(x) == "cluster" && "prob" %in% getLearnerProperties(x), ll)

getLearnerParams
(cll[[2]])

names(predict(train(setPredictType(setHyperPars(cll[[2]], centers = 1), "prob"), tasks$cluster), tasks$cluster)$data)
xx = predict(train(setPredictType(cll[[2]], "prob"), tasks$cluster), tasks$cluster)
str(xx)

dropNamed(xx$data, c("id", "response"))

debugger()

rbind(matrix(c(0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0), nrow=5), 0)




hermiteRoots = (function() {
  ev = environment()
  function(n) {
    nname = as.character(n)
    ret = get0(nname, ev, inherits = FALSE, ifnotfound = NULL)
    if (!is.null(ret)) {
      return(ret)
    }
    ltrig = cbind(rbind(0, diag(sqrt(seq_len(n - 1) / 2))), 0)
    ret = eigen(ltrig, symmetric = TRUE, only.values = TRUE)$values * sqrt(2)
    assign(nname, ret, ev)
    ret
  }
})()

hermitePoly = function(n) {
  p0 = 1
  p1 = 1
  l = 2
  index = n + 1
  while (l < index) {
    p2 = c(0, p1) - c((l - 1) * p0, 0)
    p3 = p2 - c(l * p1, 0)
    l = l + 2
    p0 = p2
    p1 = p3
  }
  if (l == index) p3 else p2
}

evalHermitePoly = function(n, x) {
  hp = hermitePoly(n)
  offset = 2 - n %% 2
  sapply(x, function(x) sum(hp * x^(seq_along(hp) * 2 - offset)))
}

hermiteWeights = (function() {
  ev = environment()
  function(n) {
    nname = as.character(n)
    ret = get0(nname, ev, inherits = FALSE, ifnotfound = NULL)
    if (!is.null(ret)) {
      return(ret)
    }
    roots = hermiteRoots(n)
    hvals = evalHermitePoly(n - 1, roots)
    ret = factorial(n - 1) / (n * hvals^2)
    assign(nname, ret, ev)
    ret
  }
})()

normExpVal = function(fun, n, mu = 0, sigma = 1) {
  roots = hermiteRoots(n)
  fvals = vnapply(roots, function(x) fun(x * sigma + mu))

  sum(fvals * hermiteWeights(n))
}

invertNormalMuSigma = function(fun, mu, sigma, n = 23, as.df = FALSE) {
  if (as.df) {
    muinv = mapply(normExpVal, mu = mu, sigma = sigma, MoreArgs = list(fun = fun, n = n))
    seinv = sqrt(mapply(function(muinv, mu, sigma) normExpVal(function(x) (fun(x) - muinv)^2, n, mu, sigma),
      muinv = muinv, mu = mu, sigma = sigma))
    cbind(response = muinv, se = seinv)
  } else {
    muinv = normExpVal(fun, n, mu, sigma)
    seinv = sqrt(normExpVal(function(x) (fun(x) - muinv)^2, n, mu, sigma))
    c(response = muinv, se = seinv)
  }
}
