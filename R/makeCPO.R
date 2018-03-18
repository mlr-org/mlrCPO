# makeCPO.R contains all CPO constructor functions, that are used both
# internally and are exported for user use.

#' @include listCPO.R parameters.R properties.R callInterface.R ParamSetSugar.R

cpo.dataproperties = c("numerics", "factors", "ordered", "missings")
cpo.tasktypes = c("cluster", "classif", "multilabel", "regr", "surv")  # these are the SUPPORTED tasks
cpo.targetproperties = c("oneclass", "twoclass", "multiclass")
cpo.predict.properties = c("prob", "se")
cpo.identity.predict.type.map = c(response = "response", prob = "prob", se = "se")

cpo.predict.types = c("response", cpo.predict.properties)
cpo.all.target.properties = c(cpo.tasktypes, cpo.targetproperties, cpo.predict.properties)
cpo.all.properties = c(cpo.dataproperties, cpo.all.target.properties)


# All "affect.*" parameters
affect.params = c("affect.type", "affect.index", "affect.names", "affect.pattern", "affect.invert", "affect.pattern.ignore.case",
  "affect.pattern.perl", "affect.pattern.fixed")
# The "export" parameter may either contain a list of names of parameters to export, or one of these special values:
export.possibilities = c("export.default", "export.set", "export.default.set", "export.unset", "export.default.unset",
  "export.all", "export.none", "export.all.plus")

# Reserved parameter names:
# these parameters are either special parameters given to the constructor function (id, affect.*),
# the possible special values of 'export' that should not clash with param names,
# special parameters given to the cpo.trafo function (data, target), special parameters given to the
# cpo.retrafo function (predict.type, control),
reserved.params = c("data", "target", "data.reduced", "target.reduced",
  "df.features", "predict.type", "control", "id", "export", affect.params, export.possibilities)


# Developer Notes: All exported CPO defining functions call makeCPOGeneral, with certain parameters already set.

#' @rdname makeCPO
#' @export
makeCPO = function(cpo.name, par.set = makeParamSet(), par.vals = NULL, dataformat = c("df.features", "split", "df.all", "task", "factor", "ordered", "numeric"),
                   dataformat.factor.with.ordered = TRUE, export.params = TRUE,  # FALSE, TRUE, names of parameters to export
                   fix.factors = FALSE, properties.data = c("numerics", "factors", "ordered", "missings"),
                   properties.adding = character(0), properties.needed = character(0),
                   properties.target = c("cluster", "classif", "multilabel", "regr", "surv",
                     "oneclass", "twoclass", "multiclass"),
                   packages = character(0), cpo.train, cpo.retrafo) {
  dataformat = match.arg(dataformat)

  makeCPOGeneral(cpo.type = "feature",
    cpo.name = cpo.name, par.set = par.set, par.vals = par.vals,
    dataformat = dataformat, dataformat.factor.with.ordered = dataformat.factor.with.ordered,
    fix.factors = fix.factors, export.params = export.params, properties.data = properties.data,
    properties.adding = properties.adding, properties.needed = properties.needed,
    properties.target = properties.target,
    packages = packages, cpo.trafo = substitute(cpo.train), cpo.retrafo = substitute(cpo.retrafo))
}

#' @rdname makeCPO
#' @export
makeCPOExtendedTrafo = function(cpo.name, par.set = makeParamSet(), par.vals = NULL,
                   dataformat = c("df.features", "split", "df.all", "task", "factor", "ordered", "numeric"),
                   dataformat.factor.with.ordered = TRUE,
                   export.params = TRUE,  # FALSE, TRUE, names of parameters to export
                   fix.factors = FALSE, properties.data = c("numerics", "factors", "ordered", "missings"),
                   properties.adding = character(0), properties.needed = character(0),
                   properties.target = c("cluster", "classif", "multilabel", "regr", "surv",
                     "oneclass", "twoclass", "multiclass"),
                   packages = character(0), cpo.trafo, cpo.retrafo) {

  dataformat = match.arg(dataformat)

  makeCPOGeneral(cpo.type = "feature.extended",
    cpo.name = cpo.name, par.set = par.set, par.vals = par.vals,
    dataformat = dataformat, dataformat.factor.with.ordered = dataformat.factor.with.ordered,
    fix.factors = fix.factors, export.params = export.params, properties.data = properties.data,
    properties.adding = properties.adding, properties.needed = properties.needed,
    properties.target = properties.target,
    packages = packages, cpo.trafo = substitute(cpo.trafo), cpo.retrafo = substitute(cpo.retrafo))
}

#' @rdname makeCPO
#' @export
makeCPORetrafoless = function(cpo.name, par.set = makeParamSet(), par.vals = NULL, dataformat = c("df.all", "task"),
                              dataformat.factor.with.ordered = TRUE,
                   export.params = TRUE,  # FALSE, TRUE, names of parameters to export
                   fix.factors = FALSE, properties.data = c("numerics", "factors", "ordered", "missings"),
                   properties.adding = character(0), properties.needed = character(0),
                   properties.target = c("cluster", "classif", "multilabel", "regr", "surv",
                     "oneclass", "twoclass", "multiclass"),
                   packages = character(0), cpo.trafo) {
  dataformat = match.arg(dataformat)

  makeCPOGeneral(cpo.type = "retrafoless",
    cpo.name = cpo.name, par.set = par.set, par.vals = par.vals,
    dataformat = dataformat, dataformat.factor.with.ordered = dataformat.factor.with.ordered,
    fix.factors = fix.factors,
    export.params = export.params, properties.data = properties.data,
    properties.adding = properties.adding, properties.needed = properties.needed,
    properties.target = properties.target,
    packages = packages, cpo.trafo = substitute(cpo.trafo))
}

#' @rdname makeCPO
#' @export
makeCPOTargetOp = function(cpo.name, par.set = makeParamSet(), par.vals = NULL,
                           dataformat = c("df.features", "split", "df.all", "task", "factor", "ordered", "numeric"),
                           dataformat.factor.with.ordered = TRUE,
                           export.params = TRUE, fix.factors = FALSE,
                           properties.data = c("numerics", "factors", "ordered", "missings"),
                           properties.adding = character(0), properties.needed = character(0),
                           properties.target = "cluster",
                           task.type.out = NULL,
                           predict.type.map = c(response = "response"),
                           packages = character(0),
                           constant.invert = FALSE,
                           cpo.train, cpo.retrafo, cpo.train.invert, cpo.invert) {
  dataformat = match.arg(dataformat)

  prep = prepareCPOTargetOp(properties.adding, properties.needed, properties.target,
    task.type.out, predict.type.map, constant.invert)

  makeCPOGeneral(cpo.type = "target",
    cpo.name = cpo.name, par.set = par.set, par.vals = par.vals,
    dataformat = dataformat, dataformat.factor.with.ordered = dataformat.factor.with.ordered,
    fix.factors = fix.factors, export.params = export.params, properties.data = properties.data,
    properties.adding = prep$properties.adding, properties.needed = prep$properties.needed,
    properties.target = prep$properties.target, type.from = prep$task.type.in, type.to = prep$task.type.out,
    predict.type.map = prep$predict.type.map, packages = packages, constant.invert = constant.invert,
    cpo.trafo = substitute(cpo.train), cpo.retrafo = substitute(cpo.retrafo),
    cpo.train.invert = substitute(cpo.train.invert), cpo.invert = substitute(cpo.invert))
}

#' @rdname makeCPO
#' @export
makeCPOExtendedTargetOp = function(cpo.name, par.set = makeParamSet(), par.vals = NULL,
                           dataformat = c("df.features", "split", "df.all", "task", "factor", "ordered", "numeric"),
                           dataformat.factor.with.ordered = TRUE,
                           export.params = TRUE, fix.factors = FALSE,
                           properties.data = c("numerics", "factors", "ordered", "missings"),
                           properties.adding = character(0), properties.needed = character(0),
                           properties.target = "cluster",
                           task.type.out = NULL,
                           predict.type.map = c(response = "response"),
                           packages = character(0),
                           constant.invert = FALSE,
                           cpo.trafo, cpo.retrafo, cpo.invert) {
  dataformat = match.arg(dataformat)

  prep = prepareCPOTargetOp(properties.adding, properties.needed, properties.target,
    task.type.out, predict.type.map, constant.invert)

  makeCPOGeneral(cpo.type = "target.extended",
    cpo.name = cpo.name, par.set = par.set, par.vals = par.vals,
    dataformat = dataformat, dataformat.factor.with.ordered = dataformat.factor.with.ordered,
    fix.factors = fix.factors, export.params = export.params, properties.data = properties.data,
    properties.adding = prep$properties.adding, properties.needed = prep$properties.needed,
    properties.target = prep$properties.target, type.from = prep$task.type.in, type.to = prep$task.type.out,
    predict.type.map = prep$predict.type.map, packages = packages, constant.invert = constant.invert,
    cpo.trafo = substitute(cpo.trafo), cpo.retrafo = substitute(cpo.retrafo), cpo.invert = substitute(cpo.invert))
}

# Make some checks and adjustmends for target operating CPOs
#
# These are common to makeCPOTargetOp and makeCPOExtendedTargetOp.
# All parameters coincide with the respecive parameters of the makeCPO* functions.
# @return [list] named list with the following, which should be given to makeCPOGeneral as is:
#   properties.adding, properties.needed properties.target,
#   predict.type.map, task.type.in, task.type.out
prepareCPOTargetOp = function(properties.adding, properties.needed, properties.target,
                              task.type.out, predict.type.map, constant.invert) {

  task.type.in = intersect(properties.target, cpo.tasktypes)

  if (length(task.type.in) > 1) {
    stopf("Only one task type may be given in properties.target, but the given ones are '%s'",
      collapse(task.type.in, "', '"))
  }

  if (is.null(task.type.out)) {
    task.type.out = task.type.in
  }

  assertSubset(properties.target, c(cpo.tasktypes, cpo.targetproperties))

  assertChoice(task.type.out, cpo.tasktypes)

  predtypes = list(classif = c("response", "prob"), regr = c("response", "se"),
    cluster = c("response", "prob"), multilabel = c("response", "prob"),
    surv = c("response", "prob"))

  # !! watch: if two different task types ever get overlapping properties, the error message
  # in the code block below needs to be adjusted.
  possible.properties = list(multilabel = character(0), regr = character(0), cluster = character(0),
      surv = character(0), classif = c("oneclass", "twoclass", "multiclass"))

  ###
  # check properties
  if (length(possible.properties[[task.type.in]])) {
    assertSubset(properties.target, c(task.type.in, possible.properties[[task.type.in]]))
    if (task.type.out != task.type.in && length(setdiff(properties.target, c(properties.adding, task.type.in)))) {
      stopf("For conversion away from %s, the elements of properties.adding that are *not* the input task type must equal properties.target.", task.type.in)
    }
    assertSubset(properties.adding, c(possible.properties[[task.type.in]],
      paste0(possible.properties[[task.type.in]], ".sometimes")))
  } else if (length(setdiff(properties.target, cpo.tasktypes))) {
    stopf("CPO handling type %s must have properties.target equal to the task type only.", task.type.in)
  } else if (length(properties.adding)) {
    stopf("Input type is %s, so properties.adding must be empty.", task.type.in)
  }

  if (length(possible.properties[[task.type.out]])) {
    assertSubset(properties.needed, c(possible.properties[[task.type.out]],
      paste0(possible.properties[[task.type.out]], ".sometimes")))
  } else if (length(properties.needed)) {
    stopf("Output type is %s, so properties.needed must be empty.", task.type.out)
  }

  ###
  # check predict.type.map
  if (is.list(predict.type.map)) {
    # turn named list into named character
    predict.type.map = vcapply(predict.type.map, identity)
  }
  if (!isTRUE(checkCharacter(predict.type.map, any.missing = FALSE, min.len = 1, names = "unique"))) {
    stop("predict.type.map argument is not, and could not be converted into, a uniquely named character vector.")
  }
  if (!isTRUE(checkSubset(names(predict.type.map), predtypes[[task.type.in]]))) {
    stopf("names of predict.type.map must be a subset of the possible prediction types %s of input Task type %s.", collapse(predtypes[[task.type.in]], sep = ", "), task.type.in)
  }
  if (!isTRUE(checkSubset(predict.type.map, predtypes[[task.type.out]]))) {
    stop("predict.type.map values must be a subset of the possible prediction types %s of output Task type %s.", predtypes[[task.type.out]], task.type.out)
  }

  if (!"response" %in% names(predict.type.map)) {
    stop("CPO must always support predict.type.map 'response', so predict.type.map must have one value named 'response'.")
  }

  list(properties.adding = properties.adding, properties.needed = properties.needed,
    properties.target = properties.target, predict.type.map = predict.type.map,
    task.type.in = task.type.in, task.type.out = task.type.out)

}

# This is the central CPO defining function, for Feature Operating CPOs and Target Operating CPOs.
# It checks that the given parameters are valid, creates functions and ParamSet from nonstandardevaluation
# arguments, and then returns the CPO creator function.
# For parameters, see docu of `makeCPO`, `makeCPOExtended`, `makeCPOTargetOpExtended`.
makeCPOGeneral = function(cpo.type = c("feature", "feature.extended", "target", "target.extended", "retrafoless"), cpo.name, par.set, par.vals,
                          dataformat, dataformat.factor.with.ordered, fix.factors, export.params,
                          properties.data, properties.adding, properties.needed,
                          properties.target, type.from, type.to, predict.type.map, packages,
                          constant.invert, cpo.trafo, cpo.retrafo = NULL, cpo.train.invert = NULL, cpo.invert = NULL) {

  cpo.type.extended = match.arg(cpo.type)

  # for most cases, *.extended  work the same as the simple equivalents
  cpo.type = gsub(".extended", "", cpo.type.extended, fixed = TRUE)

  assertString(cpo.name)

  if (!is.null(par.vals)) {
    assertList(par.vals, names = "unique")
  }
  assertFlag(dataformat.factor.with.ordered)

  if (dataformat == "ordered" && dataformat.factor.with.ordered) {
    stop('dataformat.factor.with.ordered must be FALSE when dataformat is "ordered".')
  }

  if (fix.factors && cpo.type == "target" && dataformat %in% c("df.all", "task")) {
    stop("fix.factors must be FALSE in target operation CPOs with dataformat df.all or task.")
  }


  params = prepareParams(par.set, par.vals, export.params)
  par.set = params$par.set
  par.vals = params$par.vals
  export.params = params$export.params

  if (cpo.type == "target") {
    assertFlag(constant.invert)
    assertCharacter(predict.type.map, any.missing = FALSE, names = "unique")
  } else {
    # for feature operating CPOs, this is the identity.
    predict.type.map = cpo.identity.predict.type.map
  }
  properties.list = assembleProperties(properties.data, properties.needed, properties.adding, properties.target, cpo.type, type.from, type.to)

  funargs = lapply(par.set$pars, function(dummy) substitute())

  funargs = insert(funargs, par.vals)

  trafo.funs = constructTrafoFunctions(funargs, cpo.trafo, cpo.retrafo, cpo.train.invert, cpo.invert, parent.frame(2),
    cpo.name, cpo.type.extended, dataformat, constant.invert)

  control.type = trafo.funs$control.type
  trafo.funs$control.type = NULL


  funargs = insert(funargs, list(id = substitute(), export = "export.default"))
  default.affect.args = list(affect.type = NULL, affect.index = integer(0),
    affect.names = character(0), affect.pattern = NULL, affect.invert = FALSE,
    affect.pattern.ignore.case = FALSE, affect.pattern.perl = FALSE, affect.pattern.fixed = FALSE)
  if (cpo.type != "retrafoless") {
    # "retrafoless" CPOs have no affect.* arguments
    funargs = insert(funargs, default.affect.args)
  }

  ####
  # The CPO creator function
  ####
  funbody = quote({
    # in the first two code lines, there are still arguments around that we don't know the names of.
    # therefore, we need to catch them into the 'args' list and delete them, always referencing the
    # functions we mean by '::'.
    args = base::match.call()
    base::rm(list = base::setdiff(base::ls(), "args"))  # delete all arguments to avoid name clashes
    args[[1]] = quote(list)
    args = eval(args, envir = parent.frame())
    nondefault.args = args
    args = insert(funargs, args)
    id = args$id
    if (missing(id)) {
      id = cpo.name
    }
    args$id = NULL
    export = args$export
    args$export = NULL
    if (!is.null(id)) {
      assertString(id)
    }
    if (cpo.type == "retrafoless") {
      # "retrafoless" CPOs must always take all data.
      affect.args = default.affect.args
      names(affect.args) = substring(names(affect.args), 8)
    } else {
      affect.args = args[affect.params]
      names(affect.args) = substring(names(affect.args), 8)
      if (!is.null(affect.args$type)) {
        assertSubset(affect.args$type, c("numeric", "factor", "ordered", "other"))
      }
      assertIntegerish(affect.args$index, any.missing = FALSE, unique = TRUE)
      assertCharacter(affect.args$names, any.missing = FALSE, unique = TRUE)
      if (!is.null(affect.args$pattern)) {
        assertString(affect.args$pattern)
      }
      assertFlag(affect.args$invert)
      assertFlag(affect.args$pattern.ignore.case)
      assertFlag(affect.args$pattern.perl)
      assertFlag(affect.args$pattern.fixed)
      args = dropNamed(args, affect.params)
    }

    present.pars = Filter(function(x) !identical(x, substitute()), args[names(par.set$pars)])
    checkParamsFeasible(par.set, present.pars)

    nondefault.args = nondefault.args[intersect(names2(nondefault.args), names2(present.pars))]

    if (length(export) == 1 && export %in% export.possibilities) {
      export = switch(export,
        export.default = export.params,
        export.set = names2(nondefault.args),
        export.default.set = intersect(export.params, names2(nondefault.args)),
        export.unset = setdiff(names2(par.set$pars), names2(nondefault.args)),
        export.default.unset = intersect(export.params, setdiff(names2(par.set$pars), names2(nondefault.args))),
        export.all = names2(par.set$pars),
        export.none = character(0),
        export.all.plus = stop('"export" setting "export.all.plus" not yet supported.'),
        stopf('Unknown "export" setting "%s".', export))
    }
    if (length(par.set$pars) == 0) {
      assert(identical(export, character(0)))
    } else {
      # assertChoice also with 'export.possibilities' for nicer output.
      if (length(export) == 1) {
        assertChoice(export, c(names2(par.set$pars), export.possibilities))
      } else {
        assertSubset(export, names2(par.set$pars))
      }
    }
    needed = setdiff(names2(Filter(function(x) is.null(x$requires), par.set$pars)), names2(present.pars))
    missing = setdiff(needed, export)
    if (length(missing)) {
      singular = length(missing) == 1
      are = ifelse(singular, "is", "are")
      stopf("Paramter%s '%s' %s no default, %s not exported, and %s not given on construction.",
        ifelse(singular, "", "s"), collapse(missing, sep = "', '"), ifelse(singular, "has", "have"), are, are)
    }

    unexported.pars = dropNamed(present.pars, export)
    unexportedpar.set = par.set
    unexportedpar.set$pars = dropNamed(par.set$pars, export)
    par.set$pars = par.set$pars[export]
    present.pars = present.pars[intersect(names2(present.pars), export)]

    cpo = makeS3Obj(c("CPOPrimitive", "CPO"),
      # --- CPO part
      name = cpo.name,                                       # [character(1)] the name of the operation performed by this CPO
      debug.name = cpo.name,                                 # [character(1)] Readable representation of of name and ID
      par.set = par.set,                                     # [ParamSet] exported parameters
      par.vals = present.pars,                               # [named list] values of exported parameters
      properties = properties.list,                          # properties$handling: [character] properties handled by this CPO
                                                             # properties$adding [character] capabilities that this CPO adds to the next processor
                                                             # properties$needed [character] capabilities needed by the next processor
      operating.type = cpo.type,                             # [character(1)] one of "feature", "target", "retrafoless": what the CPO operates on
                                                             #   for compound CPOs this can be a character with more than one of these.
      operating.type.extended = cpo.type.extended,           # [character(1)] similar to operating.type, but may include suffix '.extended"
      predict.type = predict.type.map,                       # [named character] translation of predict.type of underlying learner. Only for operating = "target"
      convertfrom = NULL,                                    # see TOCPO part below
      convertto = NULL,                                      # see TOCPO part below
      constant.invert = TRUE,                                # see TOCPO part below
      # --- CPOPrimitive part
      id = NULL,                                             # [character(1)] ID of the CPO -- prefix to parameters and possibly postfix to printed name
      trafo.funs = trafo.funs,                               # [list of function] cpo.trafo, cpo.retrafo, cpo.invert, and cpo.*.orig
      packages = packages,                                   # [character] package(s) to load when constructing the CPO
      affect.args = affect.args,                             # [named list] values of the "affect.*" arguments
      unexported.pars = unexported.pars,                     # [named list] values of parameters that are not exported
      unexportedpar.set = unexportedpar.set,                 # [ParamSet] unexported parameter set
      bare.par.set = par.set,                                # [ParamSet] exported parameters with names not containing the ID prefix
      properties.raw = properties.list$handling,             # [character] properties handled by the cpo.trafo / cpo.retrafo internally, after filtering for affect.*
      dataformat = dataformat,                               # [character(1)] data format as received by trafo / retrafo
      strict.factors = !dataformat.factor.with.ordered,      # [logical(1)] whether factors and ordereds are distinguished
      fix.factors = fix.factors,                             # [logical(1)] whether to clean up factor levels in retrafo
      constructor = cpo.constructor,                         # [CPOConstructor] the constructor function used to create this object
      control.type = control.type)                           # [named list] list(retrafo, invert) of one of "functional", "object", "dual.function":
                                                             #   how state is communicated. Needed by RetrafoState.R:getPrettyState()
    # --- Target Operating CPO relevant things
    if (cpo.type == "target") {
      cpo$convertfrom = type.from                            # [character(1)] task type to convert from.
      cpo$convertto = type.to                                # [character(1)] task type to convert to.
      cpo$constant.invert = constant.invert                  # [logical(1)] whether retrafo creates / modifies state.invert
    }
    if (length(getCPOAffect(cpo))) {
      # data is subset, so the overall 'properties' is the maximal set
      cpo$properties$handling = union(cpo$properties$handling,  c("numerics", "factors", "ordered", "missings"))
    }
    requireCPOPackages(cpo)
    setCPOId(cpo, id)  # this also adjusts par.set and par.vals
  })

  # the following is necessary since codoc complains otherwise. The reason is:
  # When one *usually* writes a function out that has a function default parameter (e.g. function(a = function() {}) ), then
  # this function is stored in the formals() of that function as a *call* object (i.e. the AST representing the function).
  # However, funargs so far contains the function as a *closure* object, which we get from evaluating the call / promise.
  # Therefore we need to turn the function back into a call object here.
  funargs.nofun = lapply(funargs, function(x) {
    if (is.function(x)) {
      call("function", formals(x), body(x))
    } else {
      x
    }
  })

  cpo.constructor = addClasses(eval(call("function", as.pairlist(funargs.nofun), funbody)), "CPOConstructor")
  cpo.constructor
}

# check the validity of properties.data, properties.needed, properties.adding, properties.target and assemble
# the 'properties' list that will be part of the CPO
# @param properties.data [character] properties a CPO can handle
# @param properties.needed [character] properties that a unit coming after the current CPO must be able to handle
# @param properties.adding [character] properties that this CPO adds to a learner / another CPO when attached / prepended to it.
# @param properties.target [character] target properties (e.g. prediction type, task types)
# @param cpo.type [character(1)] one of "feature", "target", or "retrafoless" -- the type of CPO being built
# @param type.from [character(1)] only for target operating CPO: specify what type of task the CPO operates on
# @param type.to [character(1)] only for target operating CPO: specify what type of task results when the CPO is applied
# @return [list] list(properties, properties.adding, properties.needed) to be used as the `$properties` slot of a CPO object.
assembleProperties = function(properties.data, properties.needed, properties.adding, properties.target, cpo.type, type.from, type.to) {
  assertCharacter(properties.data, unique = TRUE)
  assertCharacter(properties.needed, unique = TRUE)
  assertCharacter(properties.adding, unique = TRUE)
  assertCharacter(properties.target, unique = TRUE)

  # retrafoless, feature only
  assertSubset(properties.data, cpo.dataproperties)
  assertSubset(properties.target, c(cpo.tasktypes, cpo.targetproperties))
  if (cpo.type != "target") {
    data.and.sometimes = c(cpo.dataproperties, paste0(cpo.dataproperties, ".sometimes"))
    assertSubset(properties.needed, data.and.sometimes)
    assertSubset(properties.adding, data.and.sometimes)
  }

  properties.handling = union(properties.data, properties.target)

  if (cpo.type == "target") {
    assertChoice(type.from, cpo.tasktypes)
    assertChoice(type.to, cpo.tasktypes)
    if (type.from != type.to) {
      properties.adding = union(properties.adding, type.from)
      properties.needed = union(properties.needed, type.to)
    }
    properties.handling = union(properties.handling, type.from)
  } else {
    properties.handling %c=% cpo.predict.properties
  }

  aux = handleSometimesProps(properties.needed)
  properties.needed = aux$proper
  properties.needed.max = c(properties.needed, aux$sometimes)

  aux = handleSometimesProps(properties.adding)
  properties.adding.min = aux$proper
  properties.adding = c(properties.adding.min, aux$sometimes)

  assertSubset(properties.adding, properties.handling)

  if (length({badprops = intersect(properties.adding, properties.needed)})) {
    stopf("properties.adding (including *.sometimes) and properties.needed (not including *.sometimes) must not contain the same properties, but both contained %s.",
      collapse(badprops, sep = ", "))
  }
  if (length({badprops = intersect(properties.adding.min, properties.needed.max)})) {
    stopf("properties.adding (not including *.sometimes) and properties.needed (including *.sometimes) must not contain the same properties, but both contained %s.",
      collapse(badprops, sep = ", "))
  }


  list(handling = properties.handling,
    adding = properties.adding,
    needed = properties.needed,
    adding.min = properties.adding.min,
    needed.max = properties.needed.max)
}

# separate properties and properties.sometimes
# @param props [character] a properties.x (adding or needed) variable. The expression of props matters,
#   it should be given as a single proper variable name
# @return [list]. list(proper, sometimes) the properties split up.
handleSometimesProps = function(props) {
  pname = as.character(substitute(props))
  props.sometimes = grep("\\.sometimes$", props, value = TRUE)
  props = grep("\\.sometimes$", props, value = TRUE, invert = TRUE)
  props.sometimes = sub(".sometimes", "", props.sometimes)
  if (length({bad.props = intersect(props, props.sometimes)})) {
    stopf("'%s' occurred in %s with '.sometimes' but also without.",
      collapse(bad.props, "', '"), pname)
  }
  list(proper = props, sometimes = props.sometimes)
}


# check the given parameter set and parameters for validity.
# par.set.
# @param par.set [ParamSet | NULL] parameter set given to CPOConstructor to create, or NULL if none given
# @param par.vals [list] named list of default parameter values
# @param export.params [logical(1) | character] TRUE if all parameters are to be exported, FALSE if none are to be
#   exported, a character vector of parameter names that are to be exported otherwise.
# @return [list] list(par.set, par.vals, export.params)
prepareParams = function(par.set, par.vals, export.params) {
  if (any(names(par.set$pars) %in% reserved.params)) {
    stopf("Parameters %s are reserved", collapse(reserved.params, ", "))
  }
  if (is.null(par.vals)) {
    par.vals = getParamSetDefaults(par.set)
  }
  if (length({badpars = setdiff(names(par.vals), names(par.set$pars))})) {
    stopf("Values '%s' given in par.vals that are not parameters", collapse(badpars, "', '"))
  }
  par.vals = convertItemsToNamesDVP(par.vals, par.set)
  checkParamsFeasible(par.set, par.vals)

  if (isTRUE(export.params)) {
    export.params = names2(par.set$pars)
  } else if (isFALSE(export.params)) {
    export.params = character(0)
  } else {
    assertSubset(export.params, names2(par.set$pars))
  }

  list(par.set = par.set, par.vals = par.vals, export.params = export.params)
}

# Create cpo.trafo and cpo.retrafo from provided expressions and put them into canonical form
#
# Internally, cpo.trafo and cpo.retrafo must be either function- or object-based, and must furthermore
# be split up
# @param funargs [list] named list of function arguments for cpo.trafo and cpo.retrafo. The names are
#   treated as parameter names, the values as default parameter values. This list must contain the
#   CPO parameters, as well as id, and export, and affect.* parameters. Only `data`, `control`, `predict.type`
#   must be absent and will be added here.
# @param cpo.trafo [language | function] the CPO trafo function, in a format as used by `makeFunction`
# @param cpo.retrafo [language | function] the CPO retrafo function, in a format as used by `makeFunction`
# @param cpo.train.invert [language | function] the CPO train.invert function, in a format as used by `makeFunction`
# @param cpo.invert [language | function] the CPO invert function, in a format as used by `makeFunction`
# @param eval.env [environment] the environment in which to evaluate `cpo.trafo` and `cpo.retrafo`
# @param cpo.name [character(1)] name of the CPO, for error messages
# @param cpo.type [character(1)] whether CPO is target or feature operating; must be one of "target", "feature", "feature.extended", "retrafoless"
# @param dataformat [character(1)] data format used by cpo.trafo and cpo.retrafo, in its internally used semantics. Must
#   be one of "most", "all", "factor", "onlyfactor", "numeric", "ordered", "df.features", "split", "task"
# @param constant.invert [logical(1)] whether TOCPO inversion is independent of retrafo data
# @return [list] list(cpo.trafo, cpo.retrafo, cpo.trafo.orig, cpo.retrafo.orig) trafo and retrafo as used internally when handling CPO. "*.orig"
#   is needed by print.CPOConstructor for pretty printing of the functions, since the internal representation of them gets modified and is not informative.
constructTrafoFunctions = function(funargs, cpo.trafo, cpo.retrafo, cpo.train.invert, cpo.invert, eval.env, cpo.name, cpo.type, dataformat, constant.invert) {

  fnames = c("cpo.trafo", "cpo.retrafo", "cpo.train.invert", "cpo.invert")

  # evaluate expressions that are not headless functions
  for (eval.name in fnames) {
    expr = get(eval.name)
    if (missing(expr)) {
      stopf("%s is missing.%s", if (eval.name == "cpo.trafo" && cpo.type %in% c("feature", "target"))
          "cpo.train" else eval.name, if (eval.name != "cpo.trafo")
            "\nNote that for functional CPOs, it has to be explicitly set to NULL." else "")
    }
    if (!(is.recursive(expr) && !is.function(expr) && identical(expr[[1]], quote(`{`)))) {
      assign(eval.name, eval(expr, envir = eval.env))
    }
  }

  ###
  # build and check cpo.trafo
  stateless = is.null(cpo.trafo)
  if (stateless && cpo.type %nin% c("target", "feature")) {
    stop("cpo.trafo may not be NULL. If you mean to create a stateless CPO, use makeCPO or makeCPOTargetOp.")
  }
  if (stateless && cpo.type == "target" && !constant.invert) {
    stop("constant.invert must be TRUE in stateless Target Operation CPO.")
  }
  required.arglist = funargs
  required.arglist$data = substitute()
  required.arglist$target = substitute()
  if (!stateless) {
    cpo.trafo = makeFunction(cpo.trafo, required.arglist, env = eval.env)
  }

  ###
  # build and check cpo.retrafo
  if (!is.null(cpo.retrafo)) {
    assert(cpo.type != "retrafoless")  # retrafoless cpo must have cpo.retrafo = NULL, should be enforced by API

    required.arglist = funargs
    required.arglist$data = substitute()
    if (!stateless) {
      required.arglist$control = substitute()
    }
    if (cpo.type %in% c("target", "target.extended")) {
      required.arglist$target = substitute()
    }
    cpo.retrafo = makeFunction(cpo.retrafo, required.arglist, env = eval.env)
  } else if (stateless) {
    stop("One of cpo.train or cpo.retrafo must be non-NULL, since one cannot have a stateless functional CPO.")
  }

  ###
  # build and check cpo.train.invert
  if (!is.null(cpo.train.invert)) {
    assert(cpo.type == "target")  # should be enforced by API
    if (constant.invert) {
      stop("cpo.train.invert can not be given if constant.invert is TRUE")
    }
    required.arglist = funargs
    required.arglist$data = substitute()
    required.arglist$control = substitute()
    cpo.train.invert = makeFunction(cpo.train.invert, required.arglist, env = eval.env)
  }

  ###
  # build and check cpo.invert
  if (!is.null(cpo.invert)) {
    assert(cpo.type %in% c("target", "target.extended"))
    required.arglist = funargs
    required.arglist$target = substitute()
    if (!stateless) {
      required.arglist$control.invert = substitute()
    }
    required.arglist$predict.type = substitute()
    cpo.invert = makeFunction(cpo.invert, required.arglist, env = eval.env)
  }

  # make sure simple target operating CPO trafo is either all functional or all object based
  if (cpo.type == "target") {
    inv.control.target.name = if (constant.invert) "cpo.invert" else "cpo.train.invert"
    inv.control.target = get(inv.control.target.name)
    if (is.null(cpo.retrafo) != is.null(inv.control.target)) {
      stopf("If cpo.retrafo is NULL, %s must also be NULL and vice versa if constant.invert is %s.",
        inv.control.target.name, constant.invert)
    }
  }

  function.interface = switch(cpo.type,
    feature = makeCallFeatureOpSimple,
    feature.extended = makeCallFeatureOpExtended,
    target = makeCallTargetOpSimple,
    target.extended = makeCallTargetOpExtended,
    retrafoless = makeCallRetrafoless)

  cpo.funs = function.interface(cpo.trafo, cpo.retrafo, cpo.train.invert, cpo.invert, dataformat, constant.invert)

  cpo.origs = sapply(fnames, get, envir = environment(), simplify = FALSE)  # cpo.trafo, cpo.retrafo, ...
  names(cpo.origs) = paste0(names(cpo.origs), ".orig")  # rename to cpo.trafo.orig, ...

  control.type = list()
  if (cpo.type != "retrafoless") {
    control.type$retrafo = if (is.null(cpo.retrafo)) "functional" else "object"
    if (cpo.type == "target" && control.type$retrafo == "functional") {
      # special case: the simple target operation CPO creates two functions for retrafo
      control.type$retrafo = "dual.functional"
    }
  }
  if (cpo.type %in% c("target.extended", "target")) {
    control.type$invert = if (is.null(cpo.invert)) "functional" else "object"
  }

  c(cpo.funs, cpo.origs, list(control.type = control.type))
}

# create a function with expressions 'expr' in the environment 'env'.
#
# the function gets the argument list 'required.arglist.
# if 'expr' is actually a function, we just check that it has at least all the
# arguments in 'required.arglist' (or that is has ellipses), that all
# arguments have the same default values as required.arglist, and that
# arguments of the function that are not 'required' have a default value so
# there won't be an error when the function gets called later.
# @param expr [language | function] if this is an expression starting with `{`, the created function will have
#   this expression as its body. Otherwise `expr` is interpreted as a function name or definition, the returned
#   function will just be the function given.
# @param required.arglist [list] named list of arguments that will be used as function parameters
#   for the newly created function. If `expr` is an expression starting with `{`, the names correspond to parameter
#   names, the values are the default parameter values, use `substitute()` for parameters without default.
#   If `expr` is a function or function definition, it is checked that `do.call(expr, required.arglist, quote = TRUE)` would not
#   give an error for missing values or unused arguments. For this, only the names of `required.arglist`
#   are used: the function parameters of `expr` are checked that they (1a) either contain `...` or (1b)
#   contain all parameter names in the list. It is also checked that (2) all function parameters of `expr`
#   that do not have a default value, occur in `required.arglist`.
# @parem env [environment] the environment that will be the parent.env of the function created, or in which
#   `expr` is evaluated.
# @return [function] a function which can handle arguments as requested by `required.arglist` and either has the function
#   body or is the function defined by `expr`.
makeFunction = function(expr, required.arglist, env = parent.frame()) {
  if (is.recursive(expr) && !is.function(expr) && identical(expr[[1]], quote(`{`))) {
    # we have a headless list of expressions
    # so we build our own function
    args = as.pairlist(required.arglist)
    newfun = eval(call("function", args, expr), envir = env)
  } else {
    newfun = expr
    assertFunction(newfun)
    if (!"..." %in% names(formals(newfun))) {
      # with a vararg function we tentatively trust the function
      # handles its arguments

      assertFunction(newfun, args = names(required.arglist))
    }
    for (arg in names(formals(newfun))) {
      if (arg == "...") {
        # can't say anything about ellipses
        next
      }
      if (identical(formals(newfun)[[arg]], substitute())) {
        # the argument has no default, so we only care that this it will always be given
        if (!arg %in% names(required.arglist)) {
          stopf("Bad argument %s.\nNeed a function with arguments %s.", arg, collapse(names(required.arglist)), sep = ", ")
        }
        next
      }
      if (!arg %in% names(required.arglist)) {
        # the argument is not required, but it has a default; that is fine.
        next
      }
      if (!identical(formals(newfun)[[arg]], required.arglist[[arg]])) {
        if (identical(formals(newfun)[[arg]], substitute())) {
          funargstr = "no default"
          funarg = "no default"
        } else {
          funarg = eval(formals(newfun)[[arg]], envir = env)
          funargstr = sprintf("default %s", collapse(funarg))
        }
        if (identical(required.arglist[[arg]], substitute())) {
          reqarg = "no default"
        } else {
          reqarg = required.arglist[[arg]]
        }
        # there is a chance that they evaluate to the same value, so we check again
        if (!identical(funarg, reqarg) && (!length(funarg) == 1 || !length(reqarg) == 1 || !is.na(funarg) || !is.na(reqarg))) {
          stopf("Given function parameter %s has %s, but required default is %s.",
                arg, funargstr, collapse(reqarg))
        }
      }
    }
  }
  newfun
}

