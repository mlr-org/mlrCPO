context("test cpoResponseFromSE")



test_that("cpoResponseFromSE works as expected", {

  tf = bh.task %>>% cpoResponseFromSE()
  expect_equal(clearRI(tf), bh.task)
  expect_equal(clearRI(bh.task %>>% retrafo(tf)), bh.task)

  expect_identical(getCPOPredictType(cpoResponseFromSE()), c(response = "se", se = "se"))

  lrn1 = makeLearner("regr.lm")
  lrn2 = cpoResponseFromSE() %>>% lrn1

  expect_equal(getLearnerPredictType(lrn2), "response")

  expect_identical(predict(train(lrn1, bh.task), bh.task)$data,
    predict(train(lrn2, bh.task), bh.task)$data)

  expect_identical(predict(train(setPredictType(lrn1, "se"), bh.task), bh.task)$data,
    predict(train(setPredictType(lrn2, "se"), bh.task), bh.task)$data)

  expect_error(invert(retrafo(train(lrn2, bh.task)), data.frame(a = 1:10)), "'se' prediction must be a numeric matrix with two col")

  expect_identical(invert(retrafo(tf), data.frame(a = 1:10, b = 10:1)), 1:10)

  expect_identical(invert(retrafo(tf), data.frame(a = 1:10, b = 10:1), predict.type = "se"), as.matrix(data.frame(a = 1:10, b = 10:1)))

})
