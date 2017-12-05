


test_that("cpo.trafo has expected effects", {

})

test_that("cpo.retrafo has information from cpo.trafo", {


})

test_that("cpo.invert has information from cpo.trafo when taken as trafo", {


})

test_that("cpo.invert has information from cpo.trafo when taken as invert", {


})

test_that("cpo.invert has information from cpo.retrafo when taken as invert", {


})

test_that("presence of control object is checked", {

})

test_that("return values are checked", {
  # changing number of rows only allowed in retrafoless
  # changing column layout not allowed in retrafoless
  # changing number of columns not allowed in retrafoless
  # changing target forbidden in feature op
  # changing data forbidden in target op
})

test_that("capabilities behave as expected when breaking and rebuilding CPOTrained", {


})

test_that("tocpo sees the correct rows when using affect args", {

})

test_that("cbind doesn't accept tocpos / rocpos", {

})

test_that("cbind accepts tocpo / rocpo when in acceptable parts of the graph", {


})

test_that("cpoSelectFreeProperties", {

})

test_that("cpoTransformParams", {


})

test_that("cpoWrapRetrafoless", {

})

test_that("getters and setters", {


})

test_that("operators", {


})

test_that("printers", {


})

test_that("helpers", {
  # is.*
  # nulltonullcpo etc


})

test_that("datasplit", {


})

test_that("changing target names to clash gives error", {

})

test_that("target stays in its position unless names changed", {

})

test_that("properties.target is respected", {


})

test_that("target conversion works as expected", {


})

test_that("multiplexer works for all types", {
  # even NULLCPO
  # property composition

})

test_that("multiplexer checks its arguments", {

})


test_that("cpoCase works for all types", {
  # even NULLCPO
  # property composition
})

test_that("cpoCase checks its arguments", {


})

test_that("cpoCase checks the generated CPO", {


})

test_that("composing CPO with conversion etc. behaves as expected", {

  # throws errors when not compatible
  # convertfrom, convertto are updated
  # predict.type map
})

test_that("composing CPOTrained with conversion etc. behaves as expected", {
  # capabilities updated
  # convertfrom, convertto updated
  # errors when incompatible
  # predict.type map
})

test_that("'sometimes'-properties work as expected", {


})


test_that("cpoCache", {

})


test_that("NULLCPO", {

})

test_that("targetbound: target changes", {

})

test_that("target bound: changing data causes fail", {

})

test_that("targetbound type conversion", {
# ("classif", "multilabel", "regr", "surv", "cluster", "costsens")

})

test_that("targetbound datasplit", {

})

test_that("targetbound retrafo data change works", {

})

test_that("targetbound changes data in a different way on 'retrafo' fails", {

})

test_that("'bound' well behaved: after splitting, uniting; for trafos and retrafos", {

})

test_that("change task names in targetbound datasplit 'no' fails", {

})

test_that("change task names in targetbound target", {


})

test_that("switching classif levels in targetbound switches positive", {

})

test_that("targetbound functional", {

})

test_that("invert() works", {

})

test_that("inverter is noop when no targetbounds", {

})

test_that("truth is kept", {

})

test_that("classif number of classes changes", {

})

test_that("classif class names change", {

})

test_that("convert data.frame as if it were 'cluster'", {

})

test_that("inferPredictionTypePossibilities, getResponseType", {

})

test_that("incompatibility of cpo prediction and predict type detected", {

})

test_that("chaining inverters with incompatible conversion gives error", {

})

test_that("no complaint about missing 'control' in stateless cpo", {

})

test_that("after attaching CPO, predict.type stays the same if possible", {

})

test_that("chaining retrafo to learner that doesn't support the predict.type it needs fails", {

})

test_that("cpo.trafo-less CPOs, must be stateless", {

})

test_that("predict.type map works as expected", {


})

test_that("dataformat.factor.with.ordered influences strictness of property presence check", {

})

test_that("index reference in affect and cpoSelect is relative to data cols", {

})

test_that("missings tolerated in retrafo in certain conditions", {

})

test_that("attached learner properties change with tocpo", {

})

test_that("new operators work", {

})

test_that("convertNamesToItems etc", {
  # convertNamesToItems
  # convertItemsToNames
})

test_that("on.par.out.of.bounds respected", {

  # also with convertItemsToNames etc.
})


