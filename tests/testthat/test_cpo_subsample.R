
context("subsampling test")

test_that("cpoOversample works", {

  set.seed(123)
  os1 = oversample(pid.task, 2.4)

  set.seed(123)
  os2 = pid.task %>>% cpoOversample(2.4)

  expect_equal(os1, os2)

  set.seed(123)
  os1 = oversample(pid.task, 2.4, "neg")

  set.seed(123)
  os2 = pid.task %>>% cpoOversample(2.4, "neg")

  expect_equal(os1, os2)

  set.seed(123)
  os1 = oversample(pid.task, 2.4, "pos")

  set.seed(123)
  os2 = pid.task %>>% cpoOversample(2.4, "pos")

  expect_equal(os1, os2)

  ratio = do.call(`/`, as.list(getTaskDesc(pid.task)$class.distribution))
  if (ratio < 1) {
    ratio = 1 / ratio
  }

  set.seed(123)
  os0 = oversample(pid.task, ratio)

  set.seed(123)
  os1 = pid.task %>>% cpoOversample()

  set.seed(123)
  os2 = pid.task %>>% cpoOversample(ratio)

  set.seed(123)
  os3 = pid.task %>>% cpoOversample(cl = "pos")

  expect_error(pid.task %>>% cpoOversample(cl = "neg"), "'rate' may not be NULL when 'cl' is neither NULL nor the minor class")

  expect_equal(os0, os1)
  expect_equal(os0, os2)
  expect_equal(os0, os3)

})


test_that("cpoUndersample works", {

  set.seed(123)
  os1 = undersample(pid.task, .24)

  set.seed(123)
  os2 = pid.task %>>% cpoUndersample(.24)

  expect_equal(os1, os2)

  set.seed(123)
  os1 = undersample(pid.task, .24, "neg")

  set.seed(123)
  os2 = pid.task %>>% cpoUndersample(.24, "neg")

  expect_equal(os1, os2)

  set.seed(123)
  os1 = undersample(pid.task, .24, "pos")

  set.seed(123)
  os2 = pid.task %>>% cpoUndersample(.24, "pos")

  expect_equal(os1, os2)

  ratio = do.call(`/`, as.list(getTaskDesc(pid.task)$class.distribution))
  if (ratio < 1) {
    ratio = 1 / ratio
  }

  set.seed(123)
  os0 = undersample(pid.task, 1 / ratio)

  set.seed(123)
  os1 = pid.task %>>% cpoUndersample()

  set.seed(123)
  os2 = pid.task %>>% cpoUndersample(1 / ratio)

  set.seed(123)
  os3 = pid.task %>>% cpoUndersample(cl = "neg")

  expect_error(pid.task %>>% cpoUndersample(cl = "pos"), "'rate' may not be NULL when 'cl' is neither NULL nor the major class")

  expect_equal(os0, os1)
  expect_equal(os0, os2)
  expect_equal(os0, os3)

})


test_that("cpoSample works", {

  set.seed(123)
  it = iris.task %>>% cpoSample(0.5)

  set.seed(123)
  it2 = iris.task %>>% cpoSample(size = round(nrow(iris) / 2))

  set.seed(123)
  id = iris %>>% cpoSample(0.5)

  set.seed(123)
  id2 = iris %>>% cpoSample(size = round(nrow(iris) / 2))

  set.seed(123)
  im = iris[sample(nrow(iris), round(nrow(iris) / 2)), ]

  expect_equal(id, im)
  expect_equal(id2, im)
  expect_equal(getTaskData(it), im)
  expect_equal(getTaskData(it2), im)

  set.seed(123)
  ot1 = iris.task %>>% cpoSample(size = 1)

  set.seed(123)
  ot2 = iris.task %>>% cpoSample(1 / nrow(iris))

  expect_equal(ot1, ot2)

  expect_error(iris.task %>>% cpoSample(2), "cannot take a sample larger than")

  set.seed(123)
  it = iris.task %>>% cpoSample(2, replace = TRUE)

  set.seed(123)
  im = iris[sample(nrow(iris), nrow(iris) * 2, TRUE), ]

  expect_equal(getTaskData(it), im)


  expect_error(iris.task %>>% cpoSample(rate = 1, size = 1), "Exactly one of 'rate' or 'size' must be NULL")

  expect_error(iris.task %>>% cpoSample(), "Exactly one of 'rate' or 'size' must be NULL")

})
