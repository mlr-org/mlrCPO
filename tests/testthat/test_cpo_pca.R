
context("cpoPca test")

test_that("cpoPca works as expected", {

  ip = iris %>>% cpoPca()

  ret = retrafo(ip)

  expect_equal(clearRI(iris %>>% ret), clearRI(ip))

  hip = head(ip)
  expect_equal(clearRI(head(iris) %>>% ret), clearRI(hip))

  prc = prcomp(iris[1:4])

  expect_equal(getCPOTrainedState(ret)$control$rotation, prc$rotation)

  expect_equal(getTaskData(iris.task %>>% cpoPca(center = FALSE), target.extra = TRUE)$data,
               as.data.frame(prcomp(iris[1:4], center = FALSE, scale. = FALSE)$x))

  true = list(
      prcomp(iris[1:4], center = FALSE, scale = TRUE, rank = 3),
      prcomp(iris[1:4], tol = 0.5))
  trials = list(
      iris.task %>>% cpoPca(center = FALSE, scale = TRUE, rank = 3),
      iris.task %>>% cpoPca(tol = 0.5))

  for (idx in seq_along(true)) {
    ctrl = getCPOTrainedState(retrafo(trials[[idx]]))$control
    expect_identical(names(ctrl), c("rotation", "scale", "center"))
    expect_identical(ctrl[c("rotation", "scale", "center")], true[[idx]][c("rotation", "scale", "center")])
    resmat = as.matrix(getTaskData(trials[[idx]], target.extra = TRUE)$data)
    dimnames(resmat) = NULL
    dimnames(true[[idx]]$x) = NULL
    expect_equal(resmat, true[[idx]]$x)
  }

  trafd = factors.classif %>>% cpoPca()

  expect_equal(clearRI(trafd), factors.classif)
  expect_equal(clearRI(factors.classif %>>% retrafo(trafd)), factors.classif)


})
