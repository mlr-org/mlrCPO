context("PipeOpFixFactors")

test_that("PipeOpFixFactors", {
  task = mlr_tasks$get("boston_housing")

  chaslevels = task$levels()$chas
  townlevels = task$levels()$town

  expect_datapreproc_pipeop_class(PipeOpCollapseFactors, task = task)

  expect_datapreproc_pipeop_class(PipeOpCollapseFactors, task = mlr_tasks$get("iris"))

  op = PipeOpCollapseFactors$new()
  expect_pipeop(op)

  nt = train_pipeop(op, inputs = list(task))[[1L]]
  fn = nt$feature_names

  # factor cols are removed
  expect_true(all(c("chas", "town") %in% fn))
  expect_set_equal(nt$levels()$chas, c("0", "1"))

  expect_set_equal(nt$levels()$town, names(sort(table(task$data()$town), decreasing = TRUE)[1:2]))
  expect_set_equal(levels(nt$data()$town), names(sort(table(task$data()$town), decreasing = TRUE)[1:2]))

  expect_equal(table(nt$data()$town)['Cambridge'], table(task$data()$town)['Cambridge'])
  expect_equal(unname(table(nt$data()$town)['Boston Savin Hill']), unname(task$nrow - table(task$data()$town)['Cambridge']))

  expect_true(all(nt$data()$town[task$data()$town != "Cambridge"] == "Boston Savin Hill"))
  expect_true(all(nt$data()$town[task$data()$town == "Cambridge"] == "Cambridge"))

  op$param_set$values$no_collapse_above_prevalence = .04

  nt = op$train(list(task))[[1L]]
  fn = nt$feature_names

  expect_set_equal(nt$levels()$town, names(sort(table(task$data()$town), decreasing = TRUE)[1:3]))
  expect_set_equal(levels(nt$data()$town), names(sort(table(task$data()$town), decreasing = TRUE)[1:3]))
  expect_equal(table(nt$data()$town)['Cambridge'], table(task$data()$town)['Cambridge'])
  expect_equal(table(nt$data()$town)['Boston Savin Hill'], table(task$data()$town)['Boston Savin Hill'])
  expect_equal(unname(table(nt$data()$town)['Lynn']), unname(task$nrow - sum(sort(table(task$data()$town), decreasing = TRUE)[1:2])))

  nt = op$predict(list(task$clone()$filter(1)))[[1]]
  expect_equal(nt$data()$town, factor("Lynn", levels = c("Boston Savin Hill", "Cambridge", "Lynn")))

  dattrain = data.table(
    a = factor(c("a", "a", "a", "b", "b", "c", NA), levels = letters[-26]),
    b = ordered(c("a", "b", "b", "c", "d", "d", "d")),
    target = 1:7)

  tasktrain = TaskRegr$new("train", dattrain, "target")

  dattest = data.table(
    a = factor(c("a", "b", "c", "d", "e", "z")),
    b = ordered(c("a", NA, "c", "d", "e", "z"), levels = letters[c(1, 10:3, 26)]),
    target = 1:6)

  tasktest = TaskRegr$new("train", dattest, "target")

  op$param_set$values$no_collapse_above_prevalence = 1
  op$param_set$values$target_level_count = 2

  opt = op$train(list(tasktrain))[[1]]

  expect_equal(opt$levels(), list(a = letters[1:2], b = c("b", "d")))
  expect_equal(levels(opt$data()$a), letters[1:2])
  expect_equal(levels(opt$data()$b), c("b", "d"))
  expect_true(is.ordered(opt$data()$b))
  expect_false(is.ordered(opt$data()$a))

  expect_equal(opt$data()$b, ordered(c("b", "b", "b", "b", "d", "d", "d")))
  expect_equal(opt$data()$a, factor(c("a", "a", "a", "b", "b", "b", NA)))

  opt = op$predict(list(tasktest))[[1]]

  expect_equal(opt$levels(), list(a = letters[c(1:2, 26)], b = letters[c(10:4, 26, 2)])) # 'b' gets ordered to the back.
  expect_equal(levels(opt$data()$a), letters[c(1:2, 26)])
  expect_equal(levels(opt$data()$b), letters[c(10:4, 26, 2)])
  expect_true(is.ordered(opt$data()$b))
  expect_false(is.ordered(opt$data()$a))
  expect_equal(opt$data()$a, factor(c("a", "b", "b", "b", "b", "z"), levels = letters[c(1:2, 26)]))
  expect_equal(opt$data()$b, ordered(c("b", NA, "b", "d", "e", "z"), levels = letters[c(10:4, 26, 2)]))

  expect_equal(op$state$collapse_map$a, list(a = "a", b = letters[2:25]))
  expect_equal(op$state$collapse_map$b$d, "d")
  expect_set_equal(op$state$collapse_map$b$b, c("b", "a", "c"))
  expect_equal(length(op$state$collapse_map), 2)
  expect_equal(length(op$state$collapse_map$b), 2)


})

