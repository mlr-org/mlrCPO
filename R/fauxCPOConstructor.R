# takes a function that creates a CPOConstructor, and turns this function into
# a CPOConstructor. It thus makes it possible to handle the 'constructorconstructor'
# like a normal CPOConstructor.
#
#
# @param constructorconstructor [function] a function that returns a CPOConstructor
# @param cpo.name [character(1)] the cpo name
# @param cpo.type.extended [character(1)] one of "target", "feature", "target.extended", "feature.extended", "retrafoless", "other"
#   What should the CPO be printed as?
# @param trafo.funs [list] list with names cpo.trafo.orig, cpo.retrafo.orig, cpo.train.invert.orig, cpo.invert.orig
# @param default.id.null [logical(1)] whether to set the ID of the created CPO to NULL.
# @return [CPOConstructor].
makeFauxCPOConstructor = function(constructorconstructor, cpo.name, cpo.type.extended, trafo.funs = list(), default.id.null = FALSE) {
  assertString(cpo.name)
  assertChoice(cpo.type.extended, c("target", "feature", "target.extended", "feature.extended", "retrafoless", "other"))
  constructor = function(id, export = "export.default",
           affect.type = NULL, affect.index = integer(0), affect.names = character(0), affect.pattern = NULL,
           affect.invert = FALSE, affect.pattern.ignore.case = FALSE, affect.pattern.perl = FALSE, affect.pattern.fixed = FALSE) {
    cc = constructorconstructor
    constconstcall = match.call()
    if (!is.null(names(constconstcall))) {
      constconstcall = dropNamed(constconstcall, const.params)  # drop affect.* etc.
    }
    constconstcall[[1]] = cc
    cpoconst = eval.parent(constconstcall)

    constcall = match.call()
    if (!is.null(names(constcall))) {
      constcall = dropNamed(constcall, constconst.params)  # keep affect.* etc., drop the others
    }
    constcall[[1]] = cpoconst
    newcpo = eval.parent(constcall)
    newcpo$old.constructor = newcpo$constructor
    newcpo$constructor = constructor
    if (default.id.null && missing(id)) {
      newcpo = setCPOId(newcpo, NULL)
    }
    addClasses(newcpo, "FauxCPOConstructed")
  }
  const.params = names(formals(constructor))

  constconst.params = names(formals(constructorconstructor))

  formals(constructor) = as.pairlist(c(formals(constructorconstructor), formals(constructor)))

  constructor = addClasses(constructor, c("FauxCPOConstructor", "CPOConstructor"))
  # need the extra following line so that 'constructor' as seen by itself has the right classes.
  constructor
}

#' @export
identicalCPO.FauxCPOConstructed = function(cpo1, cpo2) {
  identical(class(cpo1), class(cpo2)) &&
    identical(cpo1$constructor, cpo2$constructor) &&
    identical(cpo1$old.constructor, cpo2$old.constructor)
}

# Take a function that creates a CPO and turn this function into
# a CPOConstructor. This is mostly useful for pretty printing, no
# actual functionality is added.
# @param constructor [function] function that creates a CPO
# @param cpo.name [character(1) | NULL] name of the CPO to be printed. If this is NULL, it is extracted from the constructor.
# @param cpo.type.extended [character(1) | NULL] one of "target", "feature", "target.extended", "feature.extended", "retrafoless", "other"
#   If this is NULL, it is extracted from the constructor.
# @param trafo.funs [list] list with names cpo.trafo.orig, cpo.retrafo.orig, cpo.train.invert.orig, cpo.invert.orig: For printing.
#   If `cpo.type.extended` is NULL, this is ignored.
# @return [CPOConstructor]
wrapFauxCPOConstructor = function(constructor, cpo.name = NULL, cpo.type.extended = NULL, trafo.funs = list()) {
  constructorconstructor = constructor  # for covrTraceCPOs
  if (is.null(cpo.name) || is.null(cpo.type.extended)) {
    protocpo = constructor()
    protoenv = environment(getCPOConstructor(protocpo))
    cpo.name = firstNonNull(cpo.name, protoenv$cpo.name)
    if (is.null(cpo.type.extended)) {
      cpo.type.extended = protoenv$cpo.type.extended
      trafo.funs = protoenv$trafo.funs
    }
    rm(protoenv)
    rm(protocpo)
  }
  cons = function() {
    constcall = match.call()
    constcall[[1]] = constructorconstructor
    newcpo = eval.parent(constcall)
    newcpo$constructor = cons
    newcpo
  }
  formals(cons) = formals(constructorconstructor)
  cons = addClasses(cons, c("FauxCPOConstructor", "CPOConstructor"))
  # need the extra following line so that 'constructor' as seen by itself has the right classes.
  cons
}

