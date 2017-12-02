
#' @include makeCPO.R

#' @title CPO Multiplexer
#'
#' @template cpo_doc_intro
#'
#' \code{makeCPOMultiplex} creates a \code{\link{CPOConstructor}}, \code{cpoMultiplex}
#' \emph{is} a \code{\link{CPOConstructor}}.
#'
#' @param cpos [\code{list} of (\code{\link{CPO}} | \code{\link{CPOConstructor}})]\cr
#'   The CPOs to multiplex. If this is a named list, the
#'   names must be unique and represent the index by which
#'   \code{selected.cpo} selects CPOs. They are also the IDs that will
#'   be given to the CPOs upon construction. If the list is not named,
#'   the IDs (or default names, in case of \code{\link{CPOConstructor}}s),
#'   are used instead, and need to be unique.
#'
#'   All \code{\link{CPO}}s in the list must either be all Feature Operation CPOs,
#'   all Target Operation CPOs performing the same conversion, or all Retrafoless CPOs.
#' @param selected.cpo [\code{character(1)}]\cr
#'   Selected CPO. Will default to the first item of \code{cpos}
#'   if \code{NULL}. Default is \code{NULL}.
#' @template cpo_doc_outro
#' @family special CPOs
#' @export
makeCPOMultiplex = function(cpos, selected.cpo = NULL) {
  assertList(cpos, c("CPO", "CPOConstructor"), min.len = 1)

  constructed = constructCPOList(cpos)

  # check selected.cpo parameter
  if (is.null(selected.cpo)) {
    selected.cpo = names(constructed)[1]
  }
  assertChoice(selected.cpo, names(constructed))


  # make sure all CPOs have the same operating type, and if target op, also the same conversion.
  ctinfo = collectCPOTypeInfo(constructed)

  # construct combined parameter list, complete with 'requires' addition
  param.list = lapply(names(constructed), function(n) {
    ps = getParamSet(constructed[[n]])
    ps$pars = sapply(ps$pars, function(par) {
      if (is.null(par$requires)) {
        par$requires = substitute(selected.cpo == n, list(n = n))
      } else {
        subst = list(n = n, oreq = par$requires)
        par$requires = substitute(selected.cpo == n && oreq, subst)
      }
      par
    })
    ps
  })

  # conjoin all parameters
  paramset = c(pSSLrn(selected.cpo = selected.cpo: discrete[names(constructed)]),
    do.call(base::c, param.list))

  # get the parameter values
  paramvals = unlist(unname(lapply(constructed, getHyperPars)), recursive = FALSE)


  props = collectProperties(constructed, "multiplex")
  props.creator = propertiesToMakeCPOProperties(props, ctinfo$otype)

  makeWrappingCPOConstructor(function(data, target, selected.cpo, ...) {
      cpo = constructed[[selected.cpo]]
      cpo = setHyperPars(cpo, par.vals = list(...)[getParamIds(getParamSet(cpo))])
    }, paramset, paramvals, props.creator, ctinfo, "multiplex")
}
#' @export
#' @rdname makeCPOMultiplex
cpoMultiplex = makeFauxCPOConstructor(mlrCPO::makeCPOMultiplex, "multiplex", "other", default.id.null = TRUE)
registerCPO(list(name = "cpoMultiplex", cponame = "multiplex"), "meta", NULL, "Apply one of a given set of CPOs, each having their hyperparameters exported.")

#' @title Build data-dependent CPOs
#'
#' @template cpo_doc_intro
#'
#' @description
#' The meta CPO which determines what CPO to apply to a data depending on
#' a provided function. Many parameters coincide with the parameters of \code{\link{makeCPO}},
#' it is suggested to read the relevant parameter description there.
#'
#' \code{makeCPOCase} creates a \code{\link{CPOConstructor}}, while \code{cpoCase} can be
#' used as \code{\link{CPOConstructor}} itself.
#'
#' @param par.set [\code{\link[ParamHelpers:makeParamSet]{ParamSet}}]\cr
#'   Parameters (additionally to the exported CPOs) of the CPO. Default is the empty ParamSet.
#' @param par.vals [\code{list}]\cr
#'   Named list of default parameter values for the CPO. These are used additionally to the
#'   parameter default values of \code{par.set}. It is often more elegant to use
#'   these default values, and not \code{par.vals}. Default is \code{list()}.
#'   Default is \code{list()}.
#' @param export.cpos [\code{list} of \code{CPO}]\cr
#'   List of \code{CPO} objects that have their hyperparameters exported. If this is a named list, the
#'   names must be unique and represent the parameter name by which
#'   they are given to the \code{cpo.build} function. They are also the IDs that will
#'   be given to the CPOs upon construction. If the list is not named,
#'   the IDs (or default names, in case of \code{\link{CPOConstructor}}s),
#'   are used instead, and need to be unique.
#'
#'   All \code{\link{CPO}}s in the list must either be all Feature Operation CPOs,
#'   all Target Operation CPOs performing the same conversion, or all Retrafoless CPOs.
#'
#'   The \code{cpo.build} function needs to have an argument for each of the names in the list. The \code{CPO} objects
#'   are pre-configured by the framework to have the hyperparameter settings as set by the ones exported by \code{cpoCase}.
#'   Default is \code{list()}.
#' @param dataformat [\code{character(1)}]\cr
#'   Indicate what format the data should be as seen by \dQuote{cpo.build}. See the parameter in \code{\link{makeCPO}}
#'   for details.
#'
#'   Note that if the \code{\link{CPO}}s in \code{export.cpos} are Retrafoless CPOs, this must be either \dQuote{task} or \dQuote{df.all}.
#'   Default is \dQuote{df.features}.
#' @param dataformat.factor.with.ordered [\code{logical(1)}]\cr
#'   Whether to treat \code{ordered} typed features as \code{factor} typed features. See the parameter in \code{\link{makeCPO}}.
#'   Default is \code{TRUE}.
#' @param properties.data [\code{character}]\cr
#'   See the parameter in \code{\link{makeCPO}}.
#'
#'   The properties of the resulting \code{\link{CPO}} are calculated from the constituent \code{\link{CPO}}s automatically in the
#'   most lenient way. If this parameter is not \code{NULL}, the calculated the given properties are used instead of the calculated properties.
#'
#'   Default is \code{NULL}.
#' @param properties.adding [\code{character}]\cr
#'   See the parameter in \code{\link{makeCPO}}.
#'
#'   The properties of the resulting \code{\link{CPO}} are calculated from the constituent \code{\link{CPO}}s automatically in the
#'   most lenient way. If this parameter is not \code{NULL}, the calculated the given properties are used instead of the calculated properties.
#'
#'   Default is \code{NULL}.
#' @param properties.needed [\code{character}]\cr
#'   See the parameter in \code{\link{makeCPO}}.
#'
#'   The properties of the resulting \code{\link{CPO}} are calculated from the constituent \code{\link{CPO}}s automatically in the
#'   most lenient way. If this parameter is not \code{NULL}, the calculated the given properties are used instead of the calculated properties.
#'
#'   Default is \code{NULL}.
#' @param properties.target [\code{character}]\cr
#'   See the parameter in \code{\link{makeCPO}}.
#'
#'   The properties of the resulting \code{\link{CPO}} are calculated from the constituent \code{\link{CPO}}s automatically in the
#'   most lenient way. If this parameter is not \code{NULL}, the calculated the given properties are used instead of the calculated properties.
#'
#'   Default is \code{NULL}.
#' @param cpo.build [\code{function}]\cr
#'   This function works similar to \code{cpo.trafo} in \code{\link{makeCPO}}: It has the arguments \code{data}, \code{target}, one argument for each
#'   hyperparameter declared in \code{par.set}. However, it also has one parameter for each entry in \code{export.cpos}, named by each item
#'   in that list. The \code{cpoCase} framework supplies the pre-configured \code{CPO}s (pre-configured as the exported hyperparameters of \code{cpoCase}
#'   demand) to the \code{cpo.build} code via these parameters. The return value of \code{cpo.build} must be a \code{CPO}, which will then be used on the data.
#'
#'   Just as \code{cpo.trafo} in \code{\link{makeCPO}}, this can also be a \sQuote{headless} function; it then must be written as an expression, starting
#'   with a \code{\{}.
#' @template cpo_doc_outro
#' @family special CPOs
#' @export
makeCPOCase = function(par.set = makeParamSet(), par.vals = list(), export.cpos = list(),
                   dataformat = c("df.features", "split", "df.all", "task", "factor", "ordered", "numeric"),
                   dataformat.factor.with.ordered = TRUE,
                   properties.data = NULL, properties.adding = NULL, properties.needed = NULL,
                   properties.target = NULL, cpo.build) {
  dataformat = match.arg(dataformat)

  constructed = constructCPOList(export.cpos)

  ctinfo = collectCPOTypeInfo(constructed)

  if (ctinfo$otype == "retrafoless" && dataformat %nin% c("df.all", "task")) {
    stopf("cpoCase was given retrafoless CPOs, so dataformat must be 'df.all' or 'task', but was '%s'.",
      dataformat)
  }

  paramset.pass.on = par.set
  paramvals.pass.on = insert(getParamSetDefaults(paramset.pass.on), par.vals)
  assertSubset(names(paramvals.pass.on), getParamIds(paramset.pass.on))
  if (length({name.collision = intersect(getParamIds(paramset.pass.on), names(constructed))})) {
    stopf("Names of cpo.build function arguments and export elements clash: %s", collapse(name.collision, sep = ", "))
  }

  if (length({badreserved = intersect(reserved.params, getParamIds(paramset.pass.on))})) {
    stopf("Reserved parameter name(s) '%s' cannot be used.", collapse(badreserved, "', '"))
  }

  paramset.others = do.call(base::c, lapply(constructed, getParamSet))
  paramvals.others = unlist(unname(lapply(constructed, getHyperPars)), recursive = FALSE)

  # collect properties
  props = collectProperties(constructed, "multiplex")
  props.creator = propertiesToMakeCPOProperties(props, ctinfo$otype)
  # if override given, overwrite collected properties
  for (pkind in c("data", "adding", "needed", "target")) {
    pfullkind = paste0("properties.", pkind)
    if (!is.null(get(pfullkind))) {
      props.creator[[pfullkind]] = get(pfullkind)
    }
  }

  required.arglist = lapply(c(getParamIds(paramset.pass.on), names(constructed)), function(dummy) substitute())
  required.arglist = insert(required.arglist, pv.pass.on)
  required.arglist$data = substitute()
  required.arglist$target = substitute()
  buildfun = makeFunction(substitute(cpo.build), required.arglist, env = parent.frame())

  fullaffect = list(type = c("numeric", "factor", "ordered", "other"),
    index = integer(0), names = character(0), pattern = NULL, invert = FALSE, pattern.ignore.case = FALSE,
    pattern.perl = FALSE, pattern.fixed = FALSE)

  makeWrappingCPOConstructor(function(data, target, ...) {
      args = list(...)
      buildfunargs = c(args[getParamIds(paramset.pass.on)],
        lapply(constructed, function(cpo) {
          setHyperPars(cpo, par.vals = args[getParamIds(getParamSet(cpo))])
        }))
      indata = prepareTrafoInput(data, dataformat, !dataformat.factor.with.ordered,
        cpo.all.properties, fullaffect, FALSE, "case")$indata
      do.call(buildfun, c(buildfunargs, list(data = indata, target = target)))
    },
    c(paramset.pass.on, paramset.others),
    c(paramvals.pass.on, paramvals.others),
    props.creator, ctinfo, "case")
}
#' @export
#' @rdname makeCPOCase
cpoCase = makeFauxCPOConstructor(mlrCPO::makeCPOCase, "case", "other", default.id.null = TRUE)
registerCPO(list(name = "cpoCase", cponame = "case"), "meta", NULL, "Apply a CPO constructed depending on the data.")

#' @title Transform CPO Hyperparameters.
#'
#' @template cpo_doc_intro
#'
#' @description
#' Transforms hyperparameters, or establishes dependencies between them.
#' The \code{\link{CPO}} given to \code{cpoTransformParams} gets wrapped
#' inside a new \code{\link{CPO}} with different hyperparameters. The parameters
#' for which a transformation is given are not exported (unless also given
#' in \code{additional.parameters}).
#'
#' @param cpo [\code{\link{CPO}}]\cr
#'   The CPO to use. Currently this may only have a single \link{OperatingType}.
#' @param transformations [named \code{list} of \code{language}]\cr
#'   This list is contains \code{\link[base]{expression}}s or \code{\link[base:substitute]{quote}}s
#'   that are evaluated in the context of the \emph{externally given} hyperparameters and
#'   then give the values of the internal hyperparameters. The name of each list element
#'   determines to what hyperparameter of \code{cpo} the result of the expression is written.
#'
#'   Expressions can not depend on the results of other expressions.
#'
#'   Hyperparameters of \code{cpo} named in this list are not exported by the TransformParams CPO. It is,
#'   however, possible to create synonymous parameters in \code{additional.parameters}.
#' @param additional.parameters [\code{\link[ParamHelpers:makeParamSet]{ParamSet}}]\cr
#'   Additional parameters to create, on which expressions in \code{transformations} may depend.
#'   They may contain the same names as \code{transformations}, but may not have names of hyperparameters
#'   of \code{cpo} that are \emph{not} in \code{transformations}.
#' @param par.vals [\code{list}]\cr
#'   Optional default values of parameters in \code{additional.parameters}. These override the ParamSet's
#'   default values. Default is \code{list()}. These must only concern parameters in \code{additional.parameters},
#'   not the ones in \code{cpo}.
#' @template cpo_doc_outro
#' @family special CPOs
#' @export
cpoTransformParams = function(cpo, transformations, additional.parameters = makeParamSet(), par.vals = list()) {
  assertClass(additional.parameters, "ParamSet")
  assertList(transformations, names = "unique")
  assert(all(sapply(transformations, is.language)))
  assertClass(cpo, "CPO")
  assertList(par.vals, names = "unique")
  assertSubset(names(par.vals), getParamIds(additional.parameters))

  ctinfo = collectCPOTypeInfo(constructed)

  orig.param.ids = getParamIds(getParamSet(cpo))
  exp.params$pars = dropNamed(getParamSet(cpo)$pars, names(transformations))

  exp.par.vals = dropNamed(getHyperPars(cpo), names(transformations))

  if (length({badparams = intersect(getParamIds(additional.parameters), getParamIds(exp.params))})) {
    stop("Parameter(s) '%s' of cpo listed in additional.parameters. That is only allowed if for parameters that also occur as names of transformations.",
      collapse(badparams, "', '"))
  }

  if (length({notfound = setdiff(names(transformations), orig.param.ids)})) {
    stop("transformations element(s) '%s' not parameter of cpo.",
      collapse(notfound, "', '"))
  }

  exp.params %c=% additional.parameters
  exp.par.vals %c=% par.vals

  if (length({badreserved = intersect(reserved.params, getParamIds(exp.params))})) {
    stopf("Reserved parameter name(s) '%s' cannot be used.", collapse(badreserved, "', '"))
  }

  props.creator = propertiesToMakeCPOProperties(getCPOProperties(cpo, get.internal = TRUE))

  enclosing = parent.frame()

  makeWrappingCPOConstructor(function(data, target, ...) {
      pv = list(...)
      otherpv = lapply(transformations, eval, envir = pv, enclos = enclosing)
      pv = insert(pv, otherpv)[orig.param.ids]
      setHyperPars(cpo, pv)
    },
    exp.params, exp.par.vals, props.creator, ctinfo, "transformparams")(id = NULL)
}
registerCPO(list(name = "cpoTransformParams", cponame = "transformparams"), "meta", NULL, "Transform a CPO's Hyperparameters.")

# Given a list of CPOs, construct them and make sure they are uniquely named
#
# Helper function for multiplexer etc, so the 'cpos' parameter behaves as
# one expects.
# @param cpos [list of CPO | CPOConstructor] possibly named list of CPOs
# @return cpos [named list of CPO]. uniquely named list of constructed CPOs
constructCPOList = function(cpos) {
  # get names, if not given
  has.names = !is.null(names(cpos))
  if (!has.names) {
    names(cpos) = sapply(cpos, function(c) {
      if ("CPOConstructor" %in% class(c)) {
        getCPOName(c)
      } else if (is.nullcpo(c)) {
        getCPOName(c)
      } else {
        collapse(sapply(as.list(c), getCPOId), ".")
      }
    })
  }

  # check for duplicate names
  if (length({dupnames = unique(names(cpos)[duplicated(names(cpos))])})) {
    stopf("%s must be unique, but duplicates found: %s",
          ifelse(has.names, "names of parameter 'cpos'", "CPO types given"), collapse(dupnames, sep = ", "))
  }
  assertList(cpos, names = "unique")

  # construct CPOs, if necessary
  constructed = lapply(names(cpos), function(n) {
    if ("CPOConstructor" %in% class(cpos[[n]])) {
      cpos[[n]](id = n)
    } else {
      cpos[[n]]
    }
  })

  setNames(constructed, names(cpos))
}

# Given a list of constructed CPOs, return information about commonalities
#
# cpoMultiplexer, cpoCase must both make sure that all passed CPOs behave the
# same (i.e. all the same OperatingType, all the same conversion etc.
#
# This function (1) checks these are the same, (2) gives warnings for small
# misalignments (e.g. a predict.type that only some of the CPOs can do), and
# (3) returns a list of common properties.
#
# @param constructed [list of CPO] the CPOs to collect
# @return [list]:
#   list(otype [character(1)] operating type: target, feature, retrafoless
#        convertfrom, convertto [character(1)] task type to convert from / to
#        constant.invert [logical(1)] whether invert is constant for ALL cpos
#        predict.type [named character] predict type map
#   )
collectCPOTypeInfo = function(constructed) {
  convertfrom = NULL
  convertto = NULL
  constant.invert = TRUE
  predict.type = character(0)
  ##
  # get operating type
  otype = unique(sapply(constructed, getCPOOperatingType))
  if (length(otype) > 1) {
    stopf("cpoMultiplex can only handle CPOs with all the same OperatingType, but found CPOs with Operating Types '%s'",
      collapse(otype, "', '"))
  }
  if (length(otype) == 0) {  # happens with NULLCPO
    otype = "feature"
  }

  if (otype == "target") {
    ##
    # get convertfrom
    convertfrom = unique(extractSubList(constructed, "convertfrom"))
    if (length(convertfrom) > 1) {
      stopf("cpoMultiplex can only handle target operation CPOs that perform all the same conversion, but found CPOs with different input types '%s'",
        collapse(convertfrom, "', '"))
    }
    assert(length(convertfrom) == 1)

    ##
    # get convertto
    convertto = unique(extractSubList(constructed, "convertto"))
    if (length(convertto) > 1) {
      stopf("cpoMultiplex can only handle target operation CPOs that perform all the same conversion, but found CPOs with different output types '%s'",
        collapse(convertto, "', '"))
    }
    assert(length(convertto) == 1)

    ##
    # get constant.invert
    constant.invert = Reduce(`&&`, extractSubList(constructed, "constant.invert"))

    ##
    # get predict.type
    all.pt = names(cpo.identity.predict.type.map)
    pmaps = sapply(constructed, function(cpo) {
      getCPOPredictType(cpo)[all.pt]
    })
    rownames(pmaps) = all.pt

    unique.transformations = names(Filter(function(x) x == 1,
      apply(pmaps, function(x) length(unique(x)))))

    if ("response" %nin% unique.transformations) {
      stop("cpoMultiplex can not combine target operation CPOs that need different predict.type to predict response.")
    }

    impossible.transformations = names(Filter(function(x) x > 0,
      apply(pmaps, function(x) sum(is.na(x)))))

    assert("response" %nin% impossible.transformations)  # creator of CPO should ensure this.

    if (length({nonunique = setdiff(all.pt, unique.transformations)})) {
      # the unique transformations are either the same in all, or not present in any
      # none of which would surprise the user.
      messagef("cpoMultiplex dropping predict.type%s '%s', since constituent CPOs do not all have the ability, or disagree on the requirements.",
        ifelse(length(nonunique) > 1, "s", ""), collapse(nonunique, "', '"))
    }

    possible.transformations = setdiff(unique.transformations, impossible.transformations)

    predict.type = getCPOPredictType(constructed[[1]])[possible.transformations]
    assert(!any(is.na(multiplex.predict.type)))
  }
  list(otype = otype, convertfrom = convertfrom, convertto = convertto,
    constant.invert = constant.invert, predict.type = predict.type)
}



# Convert list of properties into the respective properties.* arguments of makeCPO*.
#
# complete with .sometimes for the setdiff between adding, and adding.min, or needed and needed.max.
#
# @param properties [CPO properties]
# @param operation [character(1)] the operation type of the CPO to make(): one of "feature", "target", "retrafoless"
# @return list(properties.data, properties.adding, properties.needed, properties.target)
propertiesToMakeCPOProperties = function(properties, operation = c("feature", "target", "retrafoless")) {

  operation = match.arg(operation)

  assertSubset(properties$adding.min, properties$adding)
  assertSubset(properties$needed, properties$needed.max)

  if (operation == "target") {
    permissible.adding.needed = c(cpo.targetproperties, cpo.tasktypes)
    subset.adding.needed = cpo.targetproperties
  } else {
    permissible.adding.needed = cpo.dataproperties
    subset.adding.needed = cpo.dataproperties
  }

  toSometimes = function(one, two) {
    assertSubset(one, permissible.adding.needed)
    assertSubset(two, permissible.adding.needed)
    one = intersect(one, subset.adding.needed)
    two = intersect(two, subset.adding.needed)
    sym.setdiff = c(setdiff(one, two), setdiff(two, one))
    c(intersect(one, two), paste0(sym.setdiff, ".sometimes"))
  }

  list(properties.data = intersect(properties$handling, cpo.dataproperties),
    properties.adding = toSometimes(properties$adding.min, properties$adding),
    properties.needed = toSometimes(properties$needed.max, properties$needed),
    properties.target = intersect(properties$handling, c(cpo.tasktypes, cpo.targetproperties)))
}

# compute the combined properties of multiple CPOs
#
# There are two dual ways of combining CPOs: the multiplexer way, where
# one out of many CPOs gets applied, and the cbind way, where all CPOs get
# applied at the same time.
#
# Properties are handled in the following way:
# property       multiplex      cbind
# handling       union          intersect
# adding         union          intersect
# adding.min     intersect      intersect
# needed         intersect      union
# needed.max     union          union
#
# @param clist [list of CPO]
# @param coltype [character(1)] one of "multiplex", "cbind"
# @return [CPO properties] set according to the table above.
collectProperties = function(clist, coltype = c("multiplex", "cbind")) {
  coltype = match.arg(coltype)

  allprops = lapply(clist, getCPOProperties)

  # *A*pply *S*etop to *S*ublist
  # do.union TRUE -> union, otherwise intersection
  ass = function(sublist, do.union) {
    if (do.union) {
      setop = union
      initial = character(0)
    } else {
      setop = intersect
      initial = cpo.all.properties
    }
    Reduce(setop, extractSubList(allprops, sublist, simplify = FALSE), initial)
  }

  if (coltype == "multiplex") {
    when.to.union = c("handling", "adding", "needed.max")
  } else {
    when.to.union = c("needed", "needed.max")
  }
  lapply(c("handling", "adding", "adding.min", "needed", "needed.max"),
    function(x) ass(x, x %in% when.to.union))
}

# Build a CPOConstructor that wraps a CPO
#
# Helper function for cpoCase and multiplexer: Create a CPOConstructor that wraps
# a free CPO.
# @param cpo.selector [function] the function that selects the CPO
# @param ctinfo: list(otype [character(1)] operating type
#                     convertto [character(1)] if "target" otype: task type to convert to
#                     predict.type [named character] if "target" otype: predict.type.map
#                     constant.invert [logical(1)] whether to use constant inverter
# @param paramset [ParamSet] parameters to use
# @param paramvals [named list] default parameter values
# @param props.creator [list] list of properties.data, properties.adding, properties.needed, properties.target
# @param cpo.name [character(1)] name of the new CPO
makeWrappingCPOConstructor = function(cpo.selector, paramset, paramvals, props.creator, ctinfo, cpo.name) {
  assertFunction(cpo.selector)
  cpo.trafo = function(data, ...) {
    cpo = cpo.selector(data, ...)
    if (!identical({badop = getCPOOperatingType(cpo)}, ctinfo$otype) && length(badop) > 0) {
      stopf("meta CPO got a CPO with operating type '%s', when only '%s' was allowed.",
        collapse(badop, "', '"), ctionfo$otype)
    }
    res = data %>>% cpo
    control = list(retrafo = retrafo(res), inverter = inverter(res))
    control.invert = inverter(res)
    clearRI(res)
  }
  cpo.retrafo = function(data, control, ...) { data %>>% control }

  cpo.retrafo.inverter = function(data, target, control, ...) {
    res = target %>>% control
    control.invert = inverter(res)
    clearRI(res)
  }

  cpo.invert = function(target, predict.type, control) {
    invertCPO(control$element, target, predict.type)
  }

  maker = switch(ctinfo$otype,
    feature = makeCPOExtendedTrafo,
    target = makeCPOExtendedTargetOp,
    retrafoless = makeCPORetrafoless)

  arguments = list(cpo.name = cpo.name, par.set = paramset, par.vals = paramvals,
    dataformat = "task")

  arguments.addnl = switch(ctinfo$otype,
    feature = list(cpo.trafo = cpo.trafo, cpo.retrafo = cpo.retrafo),
    target = list(cpo.trafo = cpo.trafo, cpo.retrafo = cpo.retrafo.inverter,
      cpo.invert = cpo.invert, task.type.out = ctinfo$convertto,
      predict.type.map = ctinfo$predict.type, constant.invert = ctinfo$constant.invert),
    retrafoless = list(cpo.trafo = cpo.trafo))

  constructor = do.call(maker, c(arguments, arguments.addnl, props.creator))
}


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

  constructor = addClasses(constructor, "CPOConstructor")
  constructor
}

#' @export
identicalCPO.FauxCPOConstructed = function(cpo1, cpo2) {
  identical(cpo1$old.constructor, cpo2$old.constructor)
}
