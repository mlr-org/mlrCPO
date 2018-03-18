
context("cpoApplyFunRegrTarget test")


test_that("cpoApplyFunRegrTarget trafo / retrafo works as expected", {

  bhtarget = getTaskData(bh.task, target.extra = TRUE)$target

  trans = bh.task %>>% cpoApplyFunRegrTarget(function(x) x^2, vectorize = TRUE)
  expect_equal(getTaskData(trans, target.extra = TRUE)$target, bhtarget ^ 2)
  expect_equal(clearRI(getTaskData(bh.task) %>>% retrafo(trans)), getTaskData(trans))
  expect_error(invert(inverter(trans), 1:10, predict.type = "response"), "cannot predict 'response', since invert.response was NULL")
  expect_error(invert(inverter(trans), matrix(1:10, ncol = 2), predict.type = "se"), "cannot predict 'se', since invert.response .* invert.se must be non-NULL")

  trans = bh.task %>>% cpoApplyFunRegrTarget(function(x) x^2, vectorize = FALSE)
  expect_equal(getTaskData(trans, target.extra = TRUE)$target, bhtarget ^ 2)
  expect_equal(clearRI(getTaskData(bh.task) %>>% retrafo(trans)), getTaskData(trans))

  expect_error(bh.task %>>% cpoApplyFunRegrTarget(as.character, vectorize = TRUE), "did not return a numeric")
  expect_error(bh.task %>>% cpoApplyFunRegrTarget(as.character, vectorize = FALSE), "did not return a numeric")

  trans = bh.task %>>% cpoApplyFunRegrTarget(as.numeric, vectorize = FALSE)
  expect_equal(getTaskData(trans, target.extra = TRUE)$target, bhtarget)
  expect_equal(clearRI(getTaskData(bh.task) %>>% retrafo(trans)), getTaskData(trans))

  trans = bh.task %>>% cpoApplyFunRegrTarget(as.numeric, vectorize = TRUE)
  expect_equal(getTaskData(trans, target.extra = TRUE)$target, bhtarget)
  expect_equal(clearRI(getTaskData(bh.task) %>>% retrafo(trans)), getTaskData(trans))

  expect_error(bh.task %>>% cpoApplyFunRegrTarget(function(x) x[-1], vectorize = TRUE), "trafo may not change length")

  expect_error(bh.task %>>% cpoApplyFunRegrTarget(function() 1, vectorize = TRUE), "trafo must take at least 1 argument")

  expect_error(bh.task %>>% cpoApplyFunRegrTarget(function(x) list(x), vectorize = FALSE), "trafo' did not return values that simplified to an atomic vector.")
  expect_error(bh.task %>>% cpoApplyFunRegrTarget(function(x) list(x), vectorize = TRUE), "trafo did not return a numeric")

  expect_error(bh.task %>>% cpoApplyFunRegrTarget(function(x) 1:2, vectorize = FALSE), "trafo' did not return a result with length 1")

  trans = bh.task %>>% cpoApplyFunRegrTarget(function(x) seq_len(getTaskSize(bh.task)), vectorize = TRUE)
  expect_equal(getTaskData(trans, target.extra = TRUE)$target, seq_along(bhtarget))
  expect_equal(clearRI(getTaskData(bh.task) %>>% retrafo(trans)), getTaskData(trans))

})


test_that("cpoApplyFunRegrTarget invert works as expected", {

  trans = bh.task %>>% cpoApplyFunRegrTarget(function(x) x^2, vectorize = TRUE)
  expect_error(invert(inverter(trans), 1:10, predict.type = "response"), "cannot predict 'response', since invert.response was NULL")
  expect_error(invert(inverter(trans), matrix(1:10, ncol = 2), predict.type = "se"), "cannot predict 'se', since invert.response .* invert.se .*NULL")

  trans = bh.task %>>% cpoApplyFunRegrTarget(identity, identity)
  expect_identical(invert(inverter(trans), 1:10, predict.type = "response"), 1:10)

  trans = bh.task %>>% cpoApplyFunRegrTarget(identity, invert.se = function(x, y) cbind(x, y), vectorize = TRUE)
  expect_error(invert(inverter(trans), 1:10, predict.type = "response"), "cannot predict 'response', since invert.response was NULL")
  expect_identical(unname(invert(inverter(trans), matrix(1:10, ncol = 2), predict.type = "se")), matrix(1:10, ncol = 2))


  expect_error(bh.task %>>% cpoApplyFunRegrTarget(identity, function() 1), "invert.response must take at least 1 argument")
  expect_error(bh.task %>>% cpoApplyFunRegrTarget(identity, invert.se = function(x) 1), "invert.se must take at least 2 argument")


  # response tests
  trans = bh.task %>>% cpoApplyFunRegrTarget(identity, function(x) x^2, vectorize = TRUE)
  expect_identical(invert(inverter(trans), 1:10), (1:10)^2)
  trans = bh.task %>>% cpoApplyFunRegrTarget(identity, function(x) x^2, vectorize = FALSE)
  expect_identical(invert(inverter(trans), 1:10), (1:10)^2)

  trans = bh.task %>>% cpoApplyFunRegrTarget(identity, as.character, vectorize = TRUE)
  expect_error(invert(inverter(trans), 1:10), "did not return a numeric")
  trans = bh.task %>>% cpoApplyFunRegrTarget(identity, as.character, vectorize = FALSE)
  expect_error(invert(inverter(trans), 1:10), "did not return a numeric")


  trans = bh.task %>>% cpoApplyFunRegrTarget(identity, as.list, vectorize = TRUE)
  expect_error(invert(inverter(trans), 1:10), "did not return a numeric")
  trans = bh.task %>>% cpoApplyFunRegrTarget(identity, as.list, vectorize = FALSE)
  expect_error(invert(inverter(trans), 1:10), "did not return values that simplified to an atomic vector")

  trans = bh.task %>>% cpoApplyFunRegrTarget(identity, function(x) x[-1], vectorize = TRUE)
  expect_error(invert(inverter(trans), 1:10), "invert.response output length different from input length")
  trans = bh.task %>>% cpoApplyFunRegrTarget(identity, function(x) 1:2, vectorize = FALSE)
  expect_error(invert(inverter(trans), 1:10), "cpoApplyFunRegrTarget 'invert.response' did not return a result with length 1")

  # se tests
  trans = bh.task %>>% cpoApplyFunRegrTarget(identity, invert.se = function(x, y) cbind(x^2, sqrt(y)), vectorize = TRUE)
  expect_identical(unname(invert(inverter(trans), matrix(1:10, ncol = 2), predict.type = "se")),
    unname(as.matrix(data.frame((1:5)^2, sqrt(6:10)))))

  trans = bh.task %>>% cpoApplyFunRegrTarget(identity, invert.se = function(x, y) c(x^2, sqrt(y)), vectorize = FALSE)
  expect_identical(unname(invert(inverter(trans), matrix(1:10, ncol = 2), predict.type = "se")),
    unname(as.matrix(data.frame((1:5)^2, sqrt(6:10)))))

  trans = bh.task %>>% cpoApplyFunRegrTarget(identity, invert.se = function(x, y) cbind(x^2, as.character(y)), vectorize = TRUE)
  expect_error(invert(inverter(trans), matrix(1:10, ncol = 2), predict.type = "se"), "invert.se did not return a numeric")
  trans = bh.task %>>% cpoApplyFunRegrTarget(identity, invert.se = function(x, y) as.character(c(x, y)), vectorize = FALSE)
  expect_error(invert(inverter(trans), matrix(1:10, ncol = 2), predict.type = "se"), "invert.se did not return a numeric")
  trans = bh.task %>>% cpoApplyFunRegrTarget(identity, invert.se = function(x, y) cbind(x^2, as.list(y)), vectorize = TRUE)
  expect_error(invert(inverter(trans), matrix(1:10, ncol = 2), predict.type = "se"), "invert.se did not return a numeric")
  trans = bh.task %>>% cpoApplyFunRegrTarget(identity, invert.se = function(x, y) as.list(c(x, y)), vectorize = FALSE)
  expect_error(invert(inverter(trans), matrix(1:10, ncol = 2), predict.type = "se"), "'invert.se' did not return values that simplified to an atomic vector")
  trans = bh.task %>>% cpoApplyFunRegrTarget(identity, invert.se = function(x, y) x, vectorize = TRUE)
  expect_error(invert(inverter(trans), matrix(1:10, ncol = 2), predict.type = "se"), "invert.se returned data needs to have two columns")
  trans = bh.task %>>% cpoApplyFunRegrTarget(identity, invert.se = function(x, y) x, vectorize = FALSE)
  expect_error(invert(inverter(trans), matrix(1:10, ncol = 2), predict.type = "se"), "'invert.se' did not return a result with length 2")

  trans = bh.task %>>% cpoApplyFunRegrTarget(identity, invert.se = function(x, y) cbind(x, y)[-1, ], vectorize = TRUE)
  expect_error(invert(inverter(trans), matrix(1:10, ncol = 2), predict.type = "se"), "invert.se output had length different from input length")

})

test_that("cpoLogTrafoRegr works", {

  bhtarget = getTaskData(bh.task, target.extra = TRUE)$target

  trans = bh.task %>>% cpoLogTrafoRegr()
  expect_identical(getTaskData(trans, target.extra = TRUE)$target, log(bhtarget))
  expect_identical((getTaskData(bh.task) %>>% retrafo(trans))[[getTaskTargetNames(bh.task)]], log(bhtarget))

  expect_equal(invert(inverter(trans), 1:10), exp(1:10))

  expect_equal(unname(invert(inverter(trans), matrix(1:10, ncol = 2), predict.type = "se")),
    t(apply(matrix(1:10, ncol = 2), 1, function(x) c(exp(x[1] + x[2]^2 / 2), sqrt((exp(x[2]^2) - 1) * exp(2 * x[1] + x[2]^2))))))


})

test_that("cpoApplyFunRegrTarget invert se with gauss quadrature works", {

  trans = bh.task %>>% cpoApplyFunRegrTarget(function(x) log(x), function(x) exp(x), vectorize = FALSE)
  ltrans = bh.task %>>% cpoLogTrafoRegr()

  expect_equal(unname(invert(inverter(trans), matrix(1:10 / 10, ncol = 2), predict.type = "se")),
    unname(invert(inverter(ltrans), matrix(1:10 / 10, ncol = 2), predict.type = "se")))

  expect_equal(unname(invert(inverter(trans), matrix(1:10 / 5, ncol = 2), predict.type = "se")),
    unname(invert(inverter(ltrans), matrix(1:10 / 5, ncol = 2), predict.type = "se")))

  expect_equal(unname(invert(inverter(trans), matrix(10:1 / 3, ncol = 2), predict.type = "se")),
    unname(invert(inverter(ltrans), matrix(10:1 / 3, ncol = 2), predict.type = "se")))


  trans = bh.task %>>% cpoApplyFunRegrTarget(function(x) log(x), function(x) exp(x), vectorize = TRUE)

  expect_equal(unname(invert(inverter(trans), matrix(1:10 / 10, ncol = 2), predict.type = "se")),
    unname(invert(inverter(ltrans), matrix(1:10 / 10, ncol = 2), predict.type = "se")))

  expect_equal(unname(invert(inverter(trans), matrix(1:10 / 5, ncol = 2), predict.type = "se")),
    unname(invert(inverter(ltrans), matrix(1:10 / 5, ncol = 2), predict.type = "se")))

  expect_equal(unname(invert(inverter(trans), matrix(10:1 / 3, ncol = 2), predict.type = "se")),
    unname(invert(inverter(ltrans), matrix(10:1 / 3, ncol = 2), predict.type = "se")))

})
