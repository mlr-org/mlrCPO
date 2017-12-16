context("listCPO")

test_that("listCPO works", {

  expect_data_frame(listCPO())

  expect_identical(colnames(listCPO()), c("name", "cponame", "category", "subcategory", "description"))

  expect_true(is.character(listCPO()$name))
  expect_true(is.character(listCPO()$cponame))
  expect_true(is.character(listCPO()$description))

  expect_true(is.factor(listCPO()$category))
  expect_true(is.factor(listCPO()$subcategory))


})

test_that("all CPOs are listed with the right name", {

  clist = listCPO()

  for (idx in seq_len(nrow(clist))) {
    cons = get(clist[idx, 1])
    expect_output(print(cons), sprintf("^<<CPO %s\\(", clist[idx, 2]))
  }

})


test_that("all exported CPOConstructors are listed", {

  nspath = dirname(system.file("NAMESPACE", package = "mlrCPO"))

  exports = parseNamespaceFile(basename(nspath), dirname(nspath))$exports

  pkgenv = asNamespace("mlrCPO")

  cpocons = Filter(function(x) "CPOConstructor" %in% class(get(x)),
    ls(pkgenv, all.names = TRUE))

  cpocons = intersect(cpocons, exports)

  expect_equal(sort(cpocons), sort(listCPO()$name))

})
