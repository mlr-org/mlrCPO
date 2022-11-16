
#' @include makeCPO.R fauxCPOConstructor.R NULLCPO.R

#' @title CPO Multiplexer
#'
#' @template cpo_doc_intro
#'
#' @description
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
    }, simplify = FALSE)
    ps
  })

  # conjoin all parameters
  paramset = c(pSSLrn(selected.cpo = selected.cpo: discrete[names(constructed)]),
    do.call(base::c, param.list))

  # get the parameter values
  paramvals = insert(getParamSetDefaults(paramset), unlist(unname(lapply(constructed, getHyperPars)), recursive = FALSE))


  props = collectProperties(constructed, "multiplex")
  props.creator = propertiesToMakeCPOProperties(props, ctinfo$otype)

  makeWrappingCPOConstructor(function(data, target, selected.cpo, ...) {
      cpo = constructed[[selected.cpo]]
      cpo = setHyperPars(cpo, par.vals = list(...)[getParamIds(getParamSet(cpo))])
    }, paramset, paramvals, props.creator, ctinfo, "multiplex")
}
#' @export
#' @rdname makeCPOMultiplex
cpoMultiplex = makeFauxCPOConstructor(makeCPOMultiplex, "multiplex", "other", default.id.null = TRUE)  # nolint
registerCPO(list(name = "cpoMultiplex", cponame = "multiplex"), "meta", NULL, "Apply one of a given set of CPOs, each having their hyperparameters exported.")

#' @title Build Data-Dependent CPOs
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

  required.arglist = sapply(c(getParamIds(paramset.pass.on), names(constructed)), function(dummy) substitute(), simplify = FALSE)
  required.arglist = insert(required.arglist, paramvals.pass.on)
  required.arglist$data = substitute()
  required.arglist$target = substitute()

  cpo.build.expr = substitute(cpo.build)
  if (!(is.recursive(cpo.build.expr) && !is.function(cpo.build.expr) && identical(cpo.build.expr[[1]], quote(`{`)))) {
    cpo.build.expr = cpo.build
  }
  buildfun = makeFunction(cpo.build.expr, required.arglist, env = parent.frame())

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
        cpo.all.properties, fullaffect, FALSE, "feature", "case")$indata
      do.call(buildfun, c(buildfunargs, list(data = indata$data, target = indata$target)))
    },
    c(paramset.pass.on, paramset.others),
    c(paramvals.pass.on, paramvals.others),
    props.creator, ctinfo, "case")
}
#' @export
#' @rdname makeCPOCase
cpoCase = makeFauxCPOConstructor(makeCPOCase, "case", "other", default.id.null = TRUE)  # nolint
registerCPO(list(name = "cpoCase", cponame = "case"), "meta", NULL, "Apply a CPO constructed depending on the data.")

#' @title Transform CPO Hyperparameters
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
#'   The CPO to use. Currently this may only have a single \link{OperatingType}. Default is \code{NULLCPO}.
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
#'
#'   Default is \code{list()}.
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
cpoTransformParams = function(cpo = NULLCPO, transformations = list(), additional.parameters = makeParamSet(), par.vals = list()) {
  assertClass(cpo, "CPO")
  assertClass(additional.parameters, "ParamSet")
  assertList(transformations, names = "unique")
  assert(all(vlapply(transformations, is.language)))
  assertList(par.vals, names = "unique")
  assertSubset(names(par.vals), getParamIds(additional.parameters))

  ctinfo = collectCPOTypeInfo(list(cpo))

  orig.param.ids = getParamIds(getParamSet(cpo))
  exp.params = getParamSet(cpo)
  exp.params$pars = dropNamed(exp.params$pars, names2(transformations))

  exp.par.vals = dropNamed(getHyperPars(cpo), names2(transformations))

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
      setHyperPars(cpo, par.vals = pv)
    },
    exp.params, exp.par.vals, props.creator, ctinfo, "transformparams")(id = NULL)
}

#' @title Caches the Result of CPO Transformations
#'
#' @template cpo_doc_intro
#'
#' @description
#' Given a \code{\link{CPO}} to wrap, this caches an intermediate result (in fact, the \code{\link{retrafo}} object) whenever the
#' CPO is applied to a Task or data.frame. This can reduce computation
#' time when the same CPO is often applied to the same data, e.g. in a
#' resampling or tuning evaluation.
#'
#' The hyperparameters of the CPO are not exported, since in many cases changing the
#' hyperparameters will also change the result and would defeat the point of caching.
#' To switch between different settings of the same \code{\link{CPO}}, consider using
#' \code{\link{cpoMultiplex}}.
#'
#' The cache is kept in an \code{\link[base]{environment}}; therefore, it does not
#' communicate with other threads or processes when using parallelization at a
#' level before the cache gets filled.
#'
#' Caching needs the \sQuote{digest} package to be installed.
#'
#' @param cpo [\code{\link{CPO}}]\cr
#'   The \code{\link{CPO}} to wrap. The \code{\link{CPO}} may only have a single
#'   \code{\link{OperatingType}}. Default is \code{NULLCPO}.
#' @param cache.entries [\code{numeric(1)}]\cr
#'   Number of entries in the least recently used cache.
#' @template cpo_doc_outro
#' @export
cpoCache = function(cpo = NULLCPO, cache.entries = 1024) {
  usage.counter = 0
  usage.tracker = numeric(cache.entries)
  cache = vector("list", cache.entries)
  cache.index = setNames(as.list(seq_len(cache.entries)), rep("", cache.entries))

  assertClass(cpo, "CPO")

  ctinfo = collectCPOTypeInfo(list(cpo))

  props.creator = propertiesToMakeCPOProperties(getCPOProperties(cpo, get.internal = TRUE))

  makeWrappingCPOConstructor(function(data, target, ...) {
      td = getTaskDesc(data)
      if (!td$has.blocking && !td$has.weights) {
        if (!is.null(td$positive)) {
          entry = paste0("hash.", digest::digest(td$positive, algo = "sha512"))
        } else {
          entry = "hash"
        }
        if (is.null(data$env[[entry]])) {
          data$env[[entry]] = digest::digest(data$env$data, algo = "sha512")
        }
        hash = data$env[[entry]]
      } else {
        # include weights & blocking
        hash = digest::digest(data, algo = "sha512")
      }
      if (is.null({index = cache.index[[hash]]})) {
        index = which.min(usage.tracker)
        names(cache.index)[index] = hash
        # TODO: duplicating some work here, the makeWrappingCPOConstructor interface needs to change slightly
        cache[[index]] = retrafo(cpo %>>% data)
      }
      usage.counter %+=% 1
      usage.tracker[index] = usage.counter
      cache[[index]]
    },
    makeParamSet(), list(), props.creator, ctinfo, "cache", packages = "digest")(id = NULL)
}


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
        collapse(vcapply(as.list(c), function(x) firstNonNull(getCPOId(x), getCPOName(x))), ".")
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
  otype = unique(unlist(lapply(constructed, getCPOOperatingType)))
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
    assert(!any(is.na(predict.type)))
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
    if (length(sym.setdiff)) {
      sym.setdiff = paste0(sym.setdiff, ".sometimes")
    }
    c(intersect(one, two), sym.setdiff)
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

  if (!length(clist)) {
    am = switch(coltype, multiplex = character(0),
      cbind = cpo.dataproperties)
    return(list(handling = cpo.all.properties,
      adding = am, adding.min = am,
      needed = character(0), needed.max = character(0)))
  }

  allprops = lapply(clist, getCPOProperties, get.internal = TRUE)

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
  sapply(c("handling", "adding", "adding.min", "needed", "needed.max"),
    function(x) ass(x, x %in% when.to.union), simplify = FALSE)
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
# @param ... [any] optional additional parameters to makeCPO*
makeWrappingCPOConstructor = function(cpo.selector, paramset, paramvals, props.creator, ctinfo, cpo.name, ...) {
  assertFunction(cpo.selector)
  cpo.trafo = function(data, ...) {
    cpo = cpo.selector(data, ...)
    if (!identical({badop = getCPOOperatingType(cpo)}, ctinfo$otype) && length(badop) > 0) {
      stopf("meta CPO got a CPO with operating type '%s', when only '%s' was allowed.",
        collapse(badop, "', '"), ctinfo$otype)
    }
    res = data %>>% cpo
    if (is.retrafo(cpo)) {
      retr = cpo
    } else {
      retr = retrafo(res)
    }
    control = retr
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

  constructor = do.call(maker, c(arguments, arguments.addnl, props.creator, list(...)))
}


###
# registering needs to go to the end of the file, since all the functions above need to be defined for this

cpoTransformParams = wrapFauxCPOConstructor(cpoTransformParams)  # nolint
registerCPO(cpoTransformParams, "meta", NULL, "Transform a CPO's Hyperparameters.")

cpoCache = wrapFauxCPOConstructor(cpoCache, cpo.name = "cache", cpo.type.extended = "feature.extended")  # nolint
registerCPO(cpoCache, "meta", NULL, "Cache a CPO's results.")
