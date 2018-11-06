
context("cpo concrete implementations")

test_that("cpoScale test", {

  for (sets in list(list(FALSE, FALSE),
    list(TRUE, FALSE),
    list(FALSE, TRUE),
    list(TRUE, TRUE))) {
    ip = iris %>>% do.call(cpoScale, sets)
    ret = retrafo(ip)
    retrafo(ip) = NULL

    expect_equal(iris %>>% ret, ip)
    hip = head(ip)

    expect_equal(head(iris) %>>% ret, hip)
  }



  scld = scale(iris[1:4])
  # last iteration of the loop above has both 'center' and 'scale' set to TRUE
  # otherwise, the following wouldn't work
  expect_equal(getCPOTrainedState(ret)$control$center, attr(scld, "scaled:center"))
  expect_equal(getCPOTrainedState(ret)$control$scale, attr(scld, "scaled:scale"))

  expect_equal(getTaskData(iris.task %>>% cpoScale(center = FALSE), target.extra = TRUE)$data,
               as.data.frame(scale(iris[1:4], center = FALSE)))
})

test_that("cpo applicator", {


  ip = iris %>>% cpoWrap(cpoPca())

  ret = retrafo(ip)
  retrafo(ip) = NULL

  expect_equal(iris %>>% ret, ip)
  hip = head(ip)

  expect_equal(head(iris) %>>% ret, hip)

  expect_equal(getTaskData(iris.task %>>% cpoPca()), iris %>>% ret)


  ip = iris %>>% cpoWrap(cpoScale(center = FALSE))
  ret = retrafo(ip)
  retrafo(ip) = NULL

  expect_equal(iris %>>% ret, ip)
  hip = head(ip)

  expect_equal(head(iris) %>>% ret, hip)

  expect_equal(getTaskData(iris.task %>>% cpoScale(center = FALSE)), iris %>>% ret)

})

test_that("cpo selector", {

  ip = iris %>>% cpoSelect(type = "factor", index = c(2, 1), names = c("Petal.Length", "Sepal.Width"))
  ret = retrafo(ip)
  retrafo(ip) = NULL

  expect_equal(iris %>>% ret, ip)

  expect_equal(iris %>>% ret, iris[c(2, 1, 3, 5)])

  expect_equal(names(iris %>>% cpoSelect(pattern = "Width")), c("Sepal.Width", "Petal.Width"))

  expect_error(iris %>>% cpoSelect(names = "nosuchcol"), "not found.*nosuchcol")

  expect_error(iris %>>% cpoSelect(index = 1000), "undefined columns selected")

  ip = iris %>>% cpoSelect(type = "factor", index = c(2, 1), names = c("Petal.Length", "Sepal.Width"), invert = TRUE)
  retrafo(ip) = NULL

  expect_equal(ip, iris[4])

})

test_that("cpo dummyencoder", {

  hi = head(iris)

  expected = hi
  expected$Species = NULL
  expected[paste0("Species", levels(iris$Species))] = 0
  expected$Speciessetosa = 1

  hip = hi %>>% cpoDummyEncode()
  ret = retrafo(hip)
  retrafo(hip) = NULL

  expect_equal(head(iris %>>% ret), hip)

  expect_equal(hip, expected)

  hip2 = hi %>>% cpoDummyEncode(TRUE)
  expected$Speciessetosa = NULL
  retrafo(hip2) = NULL
  expect_equal(hip2, expected)

  hi2 = hi
  hi2$Species = factor(as.character(hi2$Species), levels = c("setosa", "versicolor"))
  hi3 = hi2
  hi3$Species = factor(as.character(hi3$Species), levels = c("versicolor", "setosa"))

  hi2p = hi2 %>>% cpoDummyEncode()
  ret2 = retrafo(hi2p)
  retrafo(hi2p) = NULL
  expect_equal(hi3 %>>% ret2, hi2p)

  expect_equal(hi %>>% ret2, hi2p)

  expect_equal(nrow(iris %>>% ret2), nrow(iris))

  it = makeRegrTask("iris2", iris, target = "Sepal.Length")

  nodumpred = predict(train("regr.lm", it), it)
  dumpred = predict(train(cpoDummyEncode(TRUE) %>>% makeLearner("regr.lm"), it), it)

  expect_equal(nodumpred$data, dumpred$data)

})

test_that("cpo dropconstants", {
  expect_equal(iris, clearRI(iris %>>% cpoDropConstants()))
  expect_equal(iris[c(1, 3, 5)], clearRI(iris %>>% cpoDropConstants(abs.tol = 1.5)))
  expect_equal(iris[3:5], clearRI(iris %>>% cpoDropConstants(rel.tol = 0.5)))
  expect_equal(iris[1:10, 1:4], clearRI(iris[1:10, ] %>>% cpoDropConstants()))
  iris.na = iris
  iris.na[[1]] = 1
  iris.na[1, 1] = NA
  iris.na[[2]] = NA_integer_
  expect_equal(iris.na[c(1, 3:5)], clearRI(iris.na %>>% cpoDropConstants()))
  expect_equal(iris.na[3:5], clearRI(iris.na %>>% cpoDropConstants(ignore.na = TRUE)))
  expect_equal(iris.na[3:5], clearRI(iris.na %>>% cpoDropConstants(ignore.na = TRUE, abs.tol = 0, rel.tol = 0)))
  expect_equal(iris[3:5], iris %>>% (iris.na %>|% cpoDropConstants(ignore.na = TRUE)))
  expect_equal(subsetTask(iris.task, features = c(1, 3)), clearRI(iris.task %>>% cpoDropConstants(abs.tol = 1.5)))
  minus.iris = clearRI(iris.task %>>% cpoApplyFun(function(x) x * -1))
  expect_equal(minus.iris, clearRI(minus.iris %>>% cpoDropConstants()))
})
