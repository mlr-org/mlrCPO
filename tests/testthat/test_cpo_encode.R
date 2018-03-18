
context("encoding CPO test")


test_that("cpoProbEncode works as expected", {

  testdf = data.frame(
      a = factor(c("a", "b", "a", "b", "a", "b")),
      b = factor(c("a", "b", "b", "a", "a", "a")),
      t = factor(c("x", "x", "x", "y", "y", "y")))

  testtask = makeClassifTask(data = testdf, target = "t")

  expectdf = data.frame(
      t = testdf$t,
      a.x = c(2, 1, 2, 1, 2, 1) / 3,
      a.y = c(1, 2, 1, 2, 1, 2) / 3,
      b.x = c(1, 4, 4, 1, 1, 1) / 4,
      b.y = c(3, 0, 0, 3, 3, 3) / 4)

  expect_equal(getTaskData(testtask %>>% cpoProbEncode()),
    expectdf)

  expect_equal(clearRI(iris.task %>>% cpoProbEncode()), iris.task)

  expect_equal(getCPOTrainedState(
      testtask %>|% cpoProbEncode())$control$a,
      matrix(c(4, 2, 3, 2, 4, 3) / 6, nrow = 3,
        dimnames = list(c("a", "b", NA), c("x", "y"))))

  testdf2 = data.frame(
      a = factor(c("a", "b", "a", "b", NA, "b")),
      b = factor(c("a", "b", "b", "a", "a", NA)),
      t = factor(c("x", "x", "x", "y", "y", "y")))

  expectdf2 = expectdf
  expectdf2$a.x[5] = .5
  expectdf2$a.y[5] = .5
  expectdf2$b.x[6] = .5
  expectdf2$b.y[6] = .5

  expect_equal(testdf2 %>>% (testtask %>|% cpoProbEncode()),
    expectdf2)

  testtask3 = makeClassifTask(data = testdf2, target = "t")


  expectdf3 = data.frame(
      t = testdf$t,
      a.x = c(3, 1, 3, 1, 1.5, 1) / 3,
      a.y = c(0, 2, 0, 2, 1.5, 2) / 3,
      b.x = c(1, 3, 3, 1, 1, 1.5) / 3,
      b.y = c(2, 0, 0, 2, 2, 1.5) / 3)


  expect_equal(getTaskData(testtask3 %>>% cpoProbEncode()),
    expectdf3)

})

test_that("cpoImpactEncodeClassif works as expected", {

  testdf = data.frame(
      a = factor(c("a", "b", "a", "b", "a", "b")),
      b = factor(c("a", "b", "b", "a", "a", "a")),
      t = factor(c("x", "x", "x", "y", "y", "y")))

  testtask = makeClassifTask(data = testdf, target = "t")

  getTaskData(testtask %>>% cpoImpactEncodeClassif())


  expect_equal(clearRI(iris.task %>>% cpoImpactEncodeClassif()),
    iris.task)

  expm = sapply(c("x", "y"), function(x) {
    glm(I(t == x) ~ 0 + a, data = testdf, family = "binomial")$coef
  })

  expm = rbind(expm, c(0, 0))
  rownames(expm) = c("a", "b", NA)

  expect_equal(
      getCPOTrainedState(testtask %>|% cpoImpactEncodeClassif(1e-10))$control$a,
      expm)

  expm = sapply(c("x", "y"), function(x) {
    glm(I(t == x) ~ 0 + b, data = testdf, family = "binomial")$coef
  })

  expm = rbind(expm, c(0, 0))
  rownames(expm) = c("a", "b", NA)

  expect_equal(
      getCPOTrainedState(testtask %>|% cpoImpactEncodeClassif(6.362e-9))$control$b,
      expm, tolerance = 1e-3)


  testdf2 = data.frame(
      a = factor(c("a", "b", "a", "b", NA, "b")),
      b = factor(c("a", "b", "b", "a", "a", NA)),
      t = factor(c("x", "x", "x", "y", "y", "y")))

  testtask3 = makeClassifTask(data = testdf2, target = "t")

  expect_false(any(is.na(getTaskData(testtask3 %>>% cpoImpactEncodeClassif()))))

  # TODO: more thorough testing
})



test_that("cpoImpactEncodeRegr works as expected", {

  testdf = data.frame(
      a = factor(c("a", "b", "a", "b", "a", "b")),
      b = factor(c("a", "b", "b", "a", "a", "a")),
      t = c(1, 2, 3, 1, 2, 3))

  testtask = makeRegrTask(data = testdf, target = "t")

  expect = data.frame(
      a = c(0, 0, 0, 0, 0, 0),
      b = c(-1, 2, 2, -1, -1, -1) / 4,
      t = c(1, 2, 3, 1, 2, 3))
  rownames(expect) = as.character(rownames(expect)) # remove this when internal is finally fixed.

  expect_equal(getTaskData(testtask %>>% cpoImpactEncodeRegr(1e-10)), expect)

  expect_equal(clearRI(bh.task %>>% cpoSelect("numeric") %>>% cpoImpactEncodeRegr()),
    clearRI(bh.task %>>% cpoSelect("numeric")))

  expect_equal(getCPOTrainedState(testtask %>|% cpoImpactEncodeRegr())$control$a, c(a = 0, b = 0, .TEMP.MISSING = 0))
  expect_equal(getCPOTrainedState(testtask %>|% cpoImpactEncodeRegr(1e-10))$control$b, c(a = -1 / 4, b = 0.5, .TEMP.MISSING = 0))

  testdf2 = data.frame(
      a = factor(c("a", "b", "a", "b", NA, "b")),
      b = factor(c("a", "b", "b", "a", "a", NA)),
      t = c(1, 2, 3, 1, 2, 3))

  expectdf2 = expect
  expectdf2$b[6] = 0
  expect_equal(testdf2 %>>% (testtask %>|% cpoImpactEncodeRegr(1e-10)), expectdf2)

  testtask3 = makeRegrTask(data = testdf2, target = "t")


  expect_false(any(is.na(getTaskData(testtask3 %>>% cpoImpactEncodeRegr()))))

  expectdf3 = expect
  expectdf3$b = c(-2 / 3, 0.5, 0.5, -2 / 3, -2 / 3, 0)

  expect_equal(getTaskData(testtask3 %>>% cpoImpactEncodeRegr(1e-10)), expectdf3)

})
