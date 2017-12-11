
context("cpo meta")

test_that("cpo multiplexer", {




  expect_error(cpoMultiplex(list(cpoScale, cpoScale)), "duplicates found")

  expect_equal(getTaskData(iris.task %>>% cpoMultiplex(list(cpoScale, cpoPcaLegacy))),
    getTaskData(iris.task %>>% cpoScale()))
  expect_equal(getTaskData(iris.task %>>% cpoMultiplex(list(cpoScale(center = FALSE), cpoPcaLegacy))),
    getTaskData(iris.task %>>% cpoScale(center = FALSE)))

  expect_equal(getTaskData(iris.task %>>% setHyperPars(cpoMultiplex(list(cpoScale(center = FALSE), cpoPcaLegacy(center = FALSE, scale = FALSE, id = "pcaX"))),
    selected.cpo = "pcaX", pcaX.scale = TRUE)),
    getTaskData(iris.task %>>% cpoPcaLegacy(center = FALSE, scale = TRUE)))

  expect_equal(getParamSet(cpoMultiplex(list(a = cpoScale, b = cpoPcaLegacy)))$pars$selected.cpo$values, list(a = "a", b = "b"))

  expect_equal(getHyperPars(cpoMultiplex(list(a = cpoScale, b = cpoPcaLegacy)))$selected.cpo, "a")

  expect_error(setHyperPars(cpoMultiplex(list(a = cpoScale, b = cpoPcaLegacy)), selected.cpo = "c"), "c is not feasible for parameter 'selected.cpo'")

  expect_error(cpoMultiplex(list(cpoScale(id = "pca"), cpoPcaLegacy)), "duplicates found: pca")


  testa = makeCPOExtended("testa", .properties = c("numerics", "missings"),
    .properties.adding = "missings", .properties.needed = "ordered", cpo.trafo = { }, cpo.retrafo = NULL)
  testb = makeCPOExtended("testb", .properties = c("numerics", "factors"),
    .properties.adding = "factors", .properties.needed = c("missings", "ordered"), cpo.trafo = { }, cpo.retrafo = NULL)


  newprops = getCPOProperties(cpoMultiplex(list(testa, testb)))


  expect_set_equal(intersect(newprops$handling, cpo.dataproperties), c("numerics", "factors", "missings"))

  expect_set_equal(newprops$adding, c("missings", "factors"))

  expect_set_equal(newprops$needed, "ordered")


  ta = makeCPOExtended("testa", .properties.target = c("classif", "twoclass"), cpo.retrafo = { data }, cpo.trafo = { data })
  tb = makeCPOExtended("testb", .properties.target = c("classif", "oneclass"), cpo.retrafo = { data }, cpo.trafo = { data })

  expect_set_equal(getCPOProperties(cpoMultiplex(list(ta, tb)))$handling, c(cpo.dataproperties, cpo.predict.properties, "classif", "oneclass", "twoclass"))

})

test_that("cpoCase", {

  expect_set_equal(names(getParamSet(cpoCase(export.cpos = list(cpoScale(id = "a"), cpoPcaLegacy(id = "b")), cpo.build = { a }))$pars),
    c("a.center", "a.scale", "b.center", "b.scale"))
  expect_class(cpoCase(export.cpos = list(a = cpoScale, b = cpoScale), cpo.build = { a }), "CPO")
  expect_error(cpoCase(export.cpos = list(a = cpoScale(id = "a"), a = cpoScale(id = "b")), cpo.build = { a }), "must be unique, but duplicates found")


  expect_equal(getTaskData(iris.task %>>% cpoCase(export.cpos = list(cpoScale(id = "a"), cpoPcaLegacy(id = "b")), cpo.build = { a })),
    getTaskData(iris.task %>>% cpoScale()))
  expect_equal(getTaskData(iris.task %>>% cpoCase(export.cpos = list(cpoScale(id = "a", center = FALSE), cpoPcaLegacy(id = "b")), cpo.build = { a })),
    getTaskData(iris.task %>>% cpoScale(center = FALSE)))


  multiplex.emu = cpoCase(pSS(selected.cpo = "a": discrete[a, b]), export.cpos = list(a = cpoScale(center = FALSE), b = cpoPcaLegacy(center = FALSE, scale = FALSE, id = "pcaX")),
    cpo.build = { switch(selected.cpo, a = a, b = b) })

  expect_equal(getTaskData(iris.task %>>% setHyperPars(multiplex.emu, selected.cpo = "b", pcaX.scale = TRUE)),
    getTaskData(iris.task %>>% cpoPcaLegacy(center = FALSE, scale = TRUE)))


  # properties
  testa = makeCPOExtended("testa", .properties = c("numerics", "missings"),
    .properties.adding = "missings", .properties.needed = "ordered", cpo.trafo = { }, cpo.retrafo = NULL)
  testb = makeCPOExtended("testb", .properties = c("numerics", "factors"),
    .properties.adding = "factors", .properties.needed = c("missings", "ordered"), cpo.trafo = { }, cpo.retrafo = NULL)


  newprops = getCPOProperties(cpoCase(export.cpos = list(a = testa, b = testb), cpo.build = { a }))

  expect_set_equal(intersect(newprops$handling, cpo.dataproperties), c("numerics", "factors", "missings"))

  expect_set_equal(newprops$adding, c("missings", "factors"))

  expect_set_equal(newprops$needed, "ordered")


  ta = makeCPOExtended("testa", .properties.target = c("classif", "twoclass"), cpo.retrafo = { data }, cpo.trafo = { data })
  tb = makeCPOExtended("testb", .properties.target = c("classif", "oneclass"), cpo.retrafo = { data }, cpo.trafo = { data })

  expect_set_equal(getCPOProperties(cpoCase(export.cpos = list(a = ta, b = tb), cpo.build = { a }))$handling,
    c(cpo.dataproperties, cpo.predict.properties, "classif", "oneclass", "twoclass"))


  # data split
  for (split in c("task", "no", "target", "most", "all", "factor", "numeric", "ordered", "onlyfactor")) {
    strans = datasplitToDataformat(split)
    checking.cpo = cpoCase(pSS(hastarget: logical), dataformat = strans$dataformat,
    dataformat.factor.with.ordered = strans$dataformat.factor.with.ordered, cpo.build = function(data, target, hastarget) {
      switch(split,
        task = {
          if (hastarget) {
            expect_equal(data, cpo.df5c)
          } else {
            expect_equal(data, makeClusterTask(getTaskId(data), cpo.df5))
          }
        },
        no = {
          expect_equal(data, cpo.df5)
        },
        target = {
          if (hastarget) {
            expect_equal(data, dropNamed(cpo.df5, names(target)))
          } else {
            expect_equal(data, cpo.df5)
          }
        },
        most = {
          expect_equal(data$numeric, cpo.df5[c(1, 4, 7)])
        },
        all = {
          expect_equal(data$numeric, cpo.df5[c(1, 4, 7)])
        },
        factor = {
          exp = cpo.df5[c(2, 3, 5, 6, 8, 9)]
          if (hastarget) {
            exp = dropNamed(exp, names(target))
          }
          expect_equal(data, exp)
        },
        numeric = {
          expect_equal(data, cpo.df5[c(1, 4, 7)])
        },
        ordered = {
          exp = cpo.df5[c(3, 6, 9)]
          expect_equal(data, exp)
        },
        onlyfactor = {
          exp = cpo.df5[c(2, 5, 8)]
          if (hastarget) {
            exp = dropNamed(exp, names(target))
          }
          expect_equal(data, exp)
        },
        stop("Unexpected split"))
      NULLCPO
    })

    cpo.df5c %>>% setHyperPars(checking.cpo, hastarget = TRUE)
    cpo.df5 %>>% setHyperPars(checking.cpo, hastarget = FALSE)

  }

  # data dependent cpo
  cpo = cpoCase(pSS(logical.param: logical),
  export.cpos = list(a = cpoScale(id = "scale"), b = cpoPcaLegacy(id = "pca", scale = FALSE, center = FALSE)),
  cpo.build = function(data, target, logical.param, a, b) {
    assert(is.nullcpo(retrafo(data)))
    if (logical.param || mean(data[[1]]) > 10) {
      a %>>% b
    } else {
      b %>>% a
    }
  })

  iris.scale.pca = iris %>>% cpoScale() %>>% cpoPcaLegacy(scale = FALSE, center = FALSE)
  iris.pca.scale = iris %>>% cpoPcaLegacy(scale = FALSE, center = FALSE) %>>% cpoScale()
  retrafo(iris.scale.pca) = NULL
  retrafo(iris.pca.scale) = NULL

  bigiris = iris %>>% cpomultiplier.nt.o(factor = 1000)
  retrafo(bigiris) = NULL

  bigiris.scale.pca = bigiris %>>% cpoScale() %>>% cpoPcaLegacy(scale = FALSE, center = FALSE)
  bigiris.pca.scale = bigiris %>>% cpoPcaLegacy(scale = FALSE, center = FALSE) %>>% cpoScale()
  retrafo(bigiris.scale.pca) = NULL
  retrafo(bigiris.pca.scale) = NULL

  ip = iris %>>% setHyperPars(cpo, logical.param = TRUE)  # scale, then pca
  rip = iris %>>% retrafo(ip)
  retrafo(ip) = NULL
  expect_equal(ip, iris.scale.pca)
  expect_equal(rip, iris.scale.pca)

  ip = iris %>>% setHyperPars(cpo, logical.param = FALSE)  # pca, then scale
  rip = iris %>>% retrafo(ip)
  retrafo(ip) = NULL
  expect_equal(ip, iris.pca.scale)
  expect_equal(rip, iris.pca.scale)

  ip = bigiris %>>% setHyperPars(cpo, logical.param = FALSE)  # scale, then pca
  rip = bigiris %>>% retrafo(ip)
  retrafo(ip) = NULL
  expect_equal(ip, bigiris.scale.pca)
  expect_equal(rip, bigiris.scale.pca)

  ip = bigiris %>>% setHyperPars(cpo, logical.param = FALSE)  # scale, then pca, since mean(data[[1]]) is large
  rip = bigiris %>>% retrafo(ip)
  retrafo(ip) = NULL
  expect_equal(ip, bigiris.scale.pca)
  expect_equal(rip, bigiris.scale.pca)

})
