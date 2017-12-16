context("fauxcpo")


test_that("makeFauxCPOConstructor does what it is supposed to do", {


  scaleOrPCA = function(which) {
    wstr = as.character(substitute(which))
    expect_true(wstr %in% c("pca", "scale"))
    if (wstr == "pca") {
      cpoPca
    } else {
      cpoScale
    }
  }

  scaleOrPCANoExpect = function(which) {
    wstr = as.character(substitute(which))
    if (wstr == "pca") {
      cpoPca
    } else {
      cpoScale
    }
  }

  sopFauxNOEXP = makeFauxCPOConstructor(scaleOrPCANoExpect, "scale.or.pca", "other")  # nolint
  expect_output(print(sopFauxNOEXP()), "scale\\(center = TRUE, scale = TRUE\\)")

  sopFauxCPO = makeFauxCPOConstructor(scaleOrPCA, "scale.or.pca", "target",  # nolint
    trafo.funs = list(cpo.train.orig = NULL, cpo.retrafo.orig = identity), default.id.null = TRUE)

  sopFauxCPO2 = makeFauxCPOConstructor(scaleOrPCA, "scale.or.pca", "target",  # nolint
    trafo.funs = list(cpo.train.orig = NULL, cpo.retrafo.orig = identity), default.id.null = FALSE)



  expect_output(print(sopFauxCPO), "<<CPO scale.or.pca\\(which\\)>>")
  expect_output(print(sopFauxCPO, verbose = TRUE), "<<CPO scale.or.pca\\(which\\)>>.*cpo.retrafo:\nfunction \\(x\\) *\nx\n")

  expect_equal(scaleOrPCA(pca), cpoPca)
  expect_equal(scaleOrPCA(scale), cpoScale)

  expect_identical(getCPOConstructor(sopFauxCPO(pca)), sopFauxCPO)

  item = sopFauxCPO(pca)
  item$constructor = NULL
  item$old.constructor = NULL
  class(item) = setdiff(class(item), "FauxCPOConstructed")

  comparison = setCPOId(cpoPca(), NULL)
  comparison$constructor = NULL

  expect_equal(item, comparison)

  expect_false(identicalCPO(sopFauxCPO(pca), cpoPca()))
  expect_false(identicalCPO(sopFauxCPO(pca), sopFauxCPO(scale)))
  expect_true(identicalCPO(sopFauxCPO(pca), sopFauxCPO(pca)))
  expect_true(identicalCPO(sopFauxCPO(pca, affect.index = 2), sopFauxCPO(pca)))

  compar2 = sopFauxCPO(pca, id = "test", affect.index = 2)

  expect_identical(getCPOId(compar2), "test")
  expect_identical(getCPOAffect(compar2), list(index = 2))

  expect_identical(getCPOId(sopFauxCPO(scale)), NULL)
  expect_identical(getCPOId(sopFauxCPO2(scale)), "scale")

  mandatory = setdiff(names(formals(cpoPca)), getParamIds(getParamSet(setCPOId(cpoPca(export = "export.all"), NULL))))
  expect_set_equal(c(mandatory, "which"), names(formals(sopFauxCPO)))

})

test_that("wrapFauxCPOConstructor", {

  scaleOrPCA2 = function(which) {
    wstr = as.character(substitute(which))
    expect_true(wstr %in% c("pca", "scale", ""))
    if (wstr == "pca") {
      cpoPca()
    } else {
      cpoScale()
    }
  }

  sopFauxCo1 = wrapFauxCPOConstructor(scaleOrPCA2)  # nolint

  expect_output(print(sopFauxCo1), "<<CPO scale\\(which\\)>>")
  expect_output(print(sopFauxCo1, verbose = TRUE), "<<CPO scale\\(which\\)>>.*cpo.trafo:\nfunction .*cpo.retrafo:\nfunction \\(")

  sopFauxCo2 = wrapFauxCPOConstructor(scaleOrPCA2, cpo.name = "sop")  # nolint

  expect_output(print(sopFauxCo2), "<<CPO sop\\(which\\)>>")
  expect_output(print(sopFauxCo2, verbose = TRUE), "<<CPO sop\\(which\\)>>.*cpo.trafo:\nfunction .*cpo.retrafo:\nfunction \\(")

  expect_true(identicalCPO(sopFauxCo2(pca), sopFauxCo2(scale)))
  expect_false(identicalCPO(sopFauxCo2(scale), sopFauxCo1(scale)))

  item = sopFauxCo1(pca)
  item$constructor = NULL

  comparison = cpoPca()
  comparison$constructor = NULL

  expect_equal(item, comparison)

})
