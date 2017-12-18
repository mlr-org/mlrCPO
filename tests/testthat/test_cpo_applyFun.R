
context("cpoApplyFun test")

test_that("cpoApplyFun works as expected", {

  trans = iris.task %>>% cpoApplyFun(function(x) x^2, vectorize = TRUE)
  expect_equal(getTaskData(trans, target.extra = TRUE)$data,
    as.data.frame(iris[1:4]^2))
  expect_equal(clearRI(iris %>>% retrafo(trans))[1:4],
    as.data.frame(iris[1:4]^2))

  trans = iris.task %>>% cpoApplyFun(function(x) x^2, vectorize = FALSE)
  expect_equal(getTaskData(trans, target.extra = TRUE)$data,
    as.data.frame(iris[1:4]^2))
  expect_equal(clearRI(iris %>>% retrafo(trans))[1:4],
    as.data.frame(iris[1:4]^2))


  trans = iris.task %>>% cpoApplyFun(as.character, vectorize = TRUE)
  expect_equal(getTaskData(trans, target.extra = TRUE)$data,
    data.frame(lapply(iris[1:4], as.factor)))
  expect_equal(iris[1:4] %>>% retrafo(trans), data.frame(lapply(iris[1:4], as.factor)))

  trans = iris.task %>>% cpoApplyFun(as.character, vectorize = FALSE)
  expect_equal(getTaskData(trans, target.extra = TRUE)$data,
    data.frame(lapply(iris[1:4], as.factor)))
  expect_equal(iris[1:4] %>>% retrafo(trans), data.frame(lapply(iris[1:4], as.factor)))


  trans = iris.task %>>% cpoApplyFun(as.character, make.factors = FALSE, vectorize = TRUE)
  expect_equal(getTaskData(trans, target.extra = TRUE)$data,
    data.frame(lapply(iris[1:4], as.character), stringsAsFactors = FALSE))
  expect_equal(iris[1:4] %>>% retrafo(trans),
    data.frame(lapply(iris[1:4], as.character), stringsAsFactors = FALSE))

  trans = iris.task %>>% cpoApplyFun(as.character, make.factors = FALSE, vectorize = FALSE)
  expect_equal(getTaskData(trans, target.extra = TRUE)$data,
    data.frame(lapply(iris[1:4], as.character), stringsAsFactors = FALSE))
  expect_equal(iris[1:4] %>>% retrafo(trans),
    data.frame(lapply(iris[1:4], as.character), stringsAsFactors = FALSE))


  trans = data.frame(a = 1:10)[integer(0)] %>>% cpoApplyFun(stop, vectorize = FALSE)
  expect_identical(clearRI(trans), data.frame(a = 1:10)[integer(0)])
  expect_identical(clearRI(data.frame(a = 1:20)[integer(0)] %>>% retrafo(trans)), data.frame(a = 1:20)[integer(0)])

  trans = data.frame(a = 1:10)[integer(0)] %>>% cpoApplyFun(stop, vectorize = TRUE)
  expect_identical(clearRI(trans), data.frame(a = 1:10)[integer(0)])
  expect_identical(clearRI(data.frame(a = 1:20)[integer(0)] %>>% retrafo(trans)), data.frame(a = 1:20)[integer(0)])


  expect_identical(clearRI(iris %>>% cpoApplyFun(identity, vectorize = FALSE)), iris)
  expect_identical(clearRI(iris %>>% cpoApplyFun(identity, vectorize = TRUE)), iris)

  expect_error(iris %>>% cpoApplyFun(function(x) 1:2, vectorize = TRUE), "cpoApplyFun 'fun' .*wrong length")
  expect_error(iris %>>% cpoApplyFun(function(x) list(x), vectorize = TRUE), "cpoApplyFun 'fun' .*wrong length")
  expect_error(iris %>>% cpoApplyFun(function(x) as.list(x), vectorize = TRUE), "cpoApplyFun 'fun' .*not return values that simplified")

  expect_error(iris %>>% cpoApplyFun(function(x) 1:2, vectorize = FALSE), "did not return a result with length 1")
  expect_error(iris %>>% cpoApplyFun(function(x) list(x), vectorize = FALSE), "cpoApplyFun 'fun' did not return values that simplified to an atomic vector")

  expect_error(iris %>>% cpoApplyFun(function() 1, vectorize = TRUE), "must take at least 1 arguments")
  expect_error(iris %>>% cpoApplyFun(function() 1, vectorize = FALSE), "must take at least 1 arguments")

  iriscounter = data.frame(rep(list(seq_len(nrow(iris))), ncol(iris)))
  colnames(iriscounter) = colnames(iris)
  expect_identical(clearRI(iris %>>% cpoApplyFun(function(x) seq_len(nrow(iris)))), iriscounter)

})


test_that("cpoApplyFun param comes through", {

  expect_true(all(as.matrix(iris %>>% cpoApplyFun(function(a, b) b, vectorize = FALSE, param = 10)) == 10))
  expect_true(all(as.matrix(iris %>>% cpoApplyFun(function(a, b) b, vectorize = TRUE, param = rep(11, nrow(iris)))) == 11))

  expect_true(all(as.matrix(iris %>>% cpoApplyFun(function(a, ...) list(...)[[1]], vectorize = FALSE, param = 12)) == 12))

  expect_identical(clearRI(iris %>>% cpoApplyFun(function(a, ...) list(...)[[1]], vectorize = FALSE, param = 13)),
    clearRI(iris %>>% cpoApplyFun(function(x) rep(13, length(x)), vectorize = TRUE)))

})
