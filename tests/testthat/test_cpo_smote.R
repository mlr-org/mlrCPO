
context("cpoSmote test")

test_that("cpoSmote works as expected", {

  set.seed(123)
  t1 = pid.task %>>% cpoSmote()

  ratio = do.call(`/`, as.list(getTaskDesc(pid.task)$class.distribution))
  if (ratio < 1) {
    ratio = 1 / ratio
  }
  set.seed(123)
  t2 = pid.task %>>% cpoSmote(ratio)

  expect_equal(t1, t2)

  set.seed(123)
  t1 = pid.task %>>% cpoSmote(alt.logic = TRUE)

  ratio = do.call(`/`, as.list(getTaskDesc(pid.task)$class.distribution))
  if (ratio < 1) {
    ratio = 1 / ratio
  }
  set.seed(123)
  t2 = pid.task %>>% cpoSmote(ratio, alt.logic = TRUE)

  expect_equal(t1, t2)

  set.seed(123)
  x1 = cpo.df1c %>>% cpoSmote(nn = 1)
  set.seed(123)
  x2 = cpo.df1c %>>% cpoSmote(2, nn = 1)
  expect_equal(x1, x2)

  set.seed(123)
  smitten = smote(pid.task, ratio, 6, FALSE, TRUE)

  set.seed(123)
  t3 = pid.task %>>% cpoSmote(nn = 6, standardize = FALSE, alt.logic = TRUE)
  expect_equal(smitten, t3)


})
