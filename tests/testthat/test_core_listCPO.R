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


test_that("registerCPO works", {

  testrcpo = function() {  # do this in a function so on.exit restores prior state even on error.

    origlist = asNamespace("mlrCPO")$CPOLIST

    on.exit({
      assign("CPOLIST", origlist, asNamespace("mlrCPO"))
      lockBinding("CPOLIST", asNamespace("mlrCPO"))
    })
    unlockBinding("CPOLIST", asNamespace("mlrCPO"))
    assign("CPOLIST", NULL, asNamespace("mlrCPO"))


    registerCPO(cpoPca, "cat", "subcat", "desc")
    registerCPO(cpoPca(), "cat", "subcat", "desc")
    registerCPO(list(name = "cpoPca", cponame = getCPOName(cpoPca)), "cat", "subcat", "desc")

    list(cpolist = asNamespace("mlrCPO")$CPOLIST, listcpo = listCPO())
  }

  res = testrcpo()

  expect_equal(res$cpolist, rep(list(list(name = "cpoPca", cponame = "pca", category = "cat", subcategory = "subcat", description = "desc")), 3))

  indf = data.frame(name = "cpoPca", cponame = "pca", category = factor("cat"), subcategory = factor("subcat"), description = "desc", stringsAsFactors = FALSE)

  expect_equal(res$listcpo, addClasses(rbind(indf, indf, indf), "ListCPO"))

})
