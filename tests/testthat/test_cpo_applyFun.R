
context("cpoApplyFun test")

test_that("cpoApplyFun works as expected", {

  trans = iris.task %>>% cpoApplyFun(function(x) x^2)
  expect_equal(getTaskData(trans, target.extra = TRUE)$data,
    as.data.frame(iris[1:4]^2))
  expect_equal(clearRI(iris %>>% retrafo(trans))[1:4],
    as.data.frame(iris[1:4]^2))

  trans = iris.task %>>% cpoApplyFun(as.character)
  expect_equal(getTaskData(trans, target.extra = TRUE)$data,
    data.frame(lapply(iris[1:4], as.factor)))
  expect_equal(iris[1:4] %>>% retrafo(trans), data.frame(lapply(iris[1:4], as.factor)))

  trans = iris.task %>>% cpoApplyFun(as.character, make.factors = FALSE)
  expect_equal(getTaskData(trans, target.extra = TRUE)$data,
    data.frame(lapply(iris[1:4], as.character), stringsAsFactors = FALSE))
  expect_equal(iris[1:4] %>>% retrafo(trans),
    data.frame(lapply(iris[1:4], as.character), stringsAsFactors = FALSE))

  trans = data.frame(a = 1:10)[integer(0)] %>>% cpoApplyFun(stop)
  expect_identical(clearRI(trans), data.frame(a = 1:10)[integer(0)])
  expect_identical(clearRI(data.frame(a = 1:20)[integer(0)] %>>% retrafo(trans)), data.frame(a = 1:20)[integer(0)])

})
