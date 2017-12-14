
context("cpoIca test")


test_that("cpoIca works as expected", {

  set.seed(123)
  true = list(
      fastICA::fastICA(iris[1:4], n.comp = 4, method = "C"),
      fastICA::fastICA(iris[1:4], n.comp = 2, alg.typ = "deflation", fun = "exp", maxit = 1000, tol = 1e-7, method = "C"),
      fastICA::fastICA(getTaskData(pid.task, target.extra = TRUE)$data, n.comp = 3, alg.typ = "deflation", fun = "logcosh", alpha = 1.5, method = "R"))
  set.seed(123)
  trials = list(
      iris.task %>>% cpoIca(),
      iris.task %>>% cpoIca(n.comp = 2, alg.typ = "deflation", fun = "exp", maxit = 1000, tol = 1e-7),
      pid.task %>>% cpoIca(n.comp = 3, alg.typ = "deflation", fun = "logcosh", alpha = 1.5, method = "R"))

  expect_equal(length(true), length(trials))

  for (idx in seq_along(true)) {
    ctrl = getCPOTrainedState(retrafo(trials[[idx]]))$control
    expect_identical(names(ctrl), c("K", "W", "A", "center"))
    expect_identical(ctrl[c("K", "W", "A")], true[[idx]][c("K", "W", "A")])
    resmat = as.matrix(getTaskData(trials[[idx]], target.extra = TRUE)$data)
    dimnames(resmat) = NULL
    dimnames(true[[idx]]$S) = NULL
    expect_equal(resmat, true[[idx]]$S)
  }

  trafd = iris.task %>>% cpoIca()

  retd = iris.task %>>% retrafo(trafd)

  expect_equal(clearRI(trafd), clearRI(retd))
  expect_equal(head(getTaskData(trafd)), clearRI(head(getTaskData(iris.task)) %>>% retrafo(trafd)))

  trafd = factors.classif %>>% cpoIca()

  expect_equal(clearRI(trafd), factors.classif)
  expect_equal(clearRI(factors.classif %>>% retrafo(trafd)), factors.classif)

})
