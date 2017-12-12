# takes a function that creates a CPOConstructor, and turns this function into
# a CPOConstructor. It thus makes it possible to handle the 'constructorconstructor'
# like a normal CPOConstructor.
#
#
# @param constructorconstructor [function] a function that returns a CPOConstructor
# @param cpo.name [character(1)] the cpo name
# @param cpo.type.extended [character(1)] one of "target", "feature", "target.extended", "feature.extended", "retrafoless", "other"
#   What should the CPO be printed as?
# @param trafo.funs [list] list with names cpo.trafo, cpo.retrafo, cpo.train.invert, cpo.invert, and the same suffixed
#   with '.orig': For printing.
# @param default.id.null [logical(1)] whether to set the ID of the created CPO to NULL.
# @return [CPOConstructor].
makeFauxCPOConstructor = function(constructorconstructor, cpo.name, cpo.type.extended, trafo.funs = list(), default.id.null = FALSE) {
  assertString(cpo.name)
  assertChoice(cpo.type.extended, c("target", "feature", "target.extended", "feature.extended", "retrafoless", "other"))
  constructor = function(id = NULL, export = "export.default",
           affect.type = NULL, affect.index = integer(0), affect.names = character(0), affect.pattern = NULL,
           affect.invert = FALSE, affect.pattern.ignore.case = FALSE, affect.pattern.perl = FALSE, affect.pattern.fixed = FALSE) {
    cc = constructorconstructor
    constconstcall = dropNamed(match.call(), const.params)
    constconstcall[[1]] = substitute(cc)
    cpoconst = eval.parent(constconstcall)

    constcall = dropNamed(match.call(), constconst.params)
    constcall[[1]] = substitute(cpoconst)
    newcpo = eval.parent(constcall)
    newcpo$old.constructor = newcpo$constructor
    newcpo$constructor = constructor
    if (default.id.null) {
      newcpo = setCPOId(newcpo, NULL)
    }
    addClasses(newcpo, "FauxCPOConstructed")
  }
  const.params = names(formals(constructor))

  constconst.params = names(formals(constructorconstructor))

  formals(constructor) = as.pairlist(c(formals(constructorconstructor), formals(constructor)))

  constructor = addClasses(constructor, c("FauxCPOConstructor", "CPOConstructor"))
  constructor
}

#' @export
identicalCPO.FauxCPOConstructed = function(cpo1, cpo2) {
  identical(cpo1$old.constructor, cpo2$old.constructor)
}
