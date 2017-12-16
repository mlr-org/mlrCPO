

context("cpoMakeCols")


test_that("cpoMakeCols behaves as expected", {

  pd = getTaskData(pid.task)
  trg = getTaskTargetNames(pid.task)

  expected = pd[trg]
  expected$gpi = pd$glucose * pd$pressure
  expected$ppt = pd$pressure * pd$pregnant + pd$triceps
  expect_identical(getTaskData(pid.task %>>% cpoMakeCols(gpi = glucose * pressure)), expected[c("gpi", trg)])

  expect_identical(getTaskData(pid.task %>>% cpoAddCols(gpi = glucose * pressure, ppt = pressure * pregnant + triceps)),
    cbind(dropNamed(pd, trg), expected[c("gpi", "ppt", trg)]))

  expect_error(pid.task %>>% cpoMakeCols(diabetes = FALSE), "diabetes.*clash.*target names")
  expect_error(pid.task %>>% cpoAddCols(diabetes = FALSE), "diabetes.*clash.*target names")

  expect_identical(getTaskData(pid.task %>>% cpoMakeCols(triceps = FALSE)), cbind(triceps = factor(FALSE), pd[trg]))

  expect_error(pid.task %>>% cpoAddCols(triceps = FALSE), "triceps.*clash.*feature names")

  indat = data.frame(a = 1:6, b = 6:1)

  onetwothree = 1:3

  expect_identical(clearRI(indat %>>% cpoAddCols(c = a + b, d = onetwothree, e = 1:2)), cbind(indat, c = 7L, d = c(1:3, 1:3), e = rep(1:2, 3)))

  expect_identical(clearRI(indat %>>% cpoMakeCols(x = TRUE, y = c("a", "b"))), data.frame(x = factor(rep(TRUE, 6)), y = factor(rep(c("a", "b"), 3))))

  expect_identical(clearRI(indat %>>% cpoMakeCols()), unname(indat[character(0)]))
  expect_identical(clearRI(indat %>>% cpoAddCols()), indat)

  expect_identical(getTaskData(pid.task %>>% cpoMakeCols()), pd[trg])
  expect_equal(clearRI(pid.task %>>% cpoAddCols()), pid.task)

  expect_identical(getTaskData(factors.classif %>>% cpoMakeCols(F1 = as.character(F1), F2 = as.character(F2), F3 = as.character(F3))),
    getTaskData(factors.classif))

  expect_identical(getTaskData(factors.classif %>>% cpoMakeCols(F1 = as.character(F1), F2 = as.character(F2), F3 = as.character(F3), .make.factors = FALSE),
    target.extra = TRUE)$data,
    as.data.frame(as.matrix(getTaskData(factors.classif, target.extra = TRUE)$data), stringsAsFactors = FALSE))

})

