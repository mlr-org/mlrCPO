
#' @include makeCPO.R

#' @title CPO Multiplexer
#'
#' @template cpo_description
#'
#' @param cpos [\code{list} of (\code{CPO} | \code{CPOConstructor})]\cr
#'   The CPOs to multiplex. If this is a named list, the
#'   names must be unique and represent the IDs that will
#'   be given to the CPOs upon construction.
#' @param selected.cpo [\code{character(1)}]\cr
#'   Selected CPO. Will default to the first item of \code{cpos}
#'   if \code{NULL}. Default is \code{NULL}.
#' @template arg_cpo_id
#' @family CPO
#' @export
cpoMultiplex = function(cpos, selected.cpo = NULL, id = NULL, export = "export.default",
    affect.type = NULL, affect.index = integer(0), affect.names = character(0), affect.pattern = NULL,
    affect.invert = FALSE, affect.pattern.ignore.case = FALSE, affect.pattern.perl = FALSE, affect.pattern.fixed = FALSE) {
  assertList(cpos, c("CPO", "CPOConstructor"), min.len = 1)  # FIXME: require databound
  has.names = !is.null(names(cpos))
  if (!has.names) {
    names(cpos) = sapply(cpos, function(c) {
      if ("CPOConstructor" %in% class(c)) {
        getCPOName(c)
      } else if ("CPOPrimitive" %in% class(c)) {
        firstNonNull(getCPOId(c), getCPOName(c))
      } else {
        c = as.list(c)[[1]]
        firstNonNull(getCPOId(c), getCPOName(c))
      }
    })
  }
  dupnames = unique(names(cpos)[duplicated(names(cpos))])
  if (length(dupnames)) {
    stopf("%s must be unique, but duplicates found: %s",
          ifelse(has.names, "names of parameter 'cpos'", "CPO types given"), collapse(dupnames, sep = ", "))
  }
  assertList(cpos, names = "unique")

  if (is.null(selected.cpo)) {
    selected.cpo = names(cpos)[1]
  }
  assertChoice(selected.cpo, names(cpos))

  constructed = lapply(names(cpos), function(n) {
    if ("CPOConstructor" %in% class(cpos[[n]])) {
      cpos[[n]](id = n)
    } else {
      cpos[[n]]
    }
  })
  names(constructed) = names(cpos)

  paramset.list = do.call(base::c, lapply(names(constructed), function(n) {
    ps = getParamSet(constructed[[n]])
    ps$pars = lapply(ps$pars, function(p) {
      if (is.null(p$requires)) {
        p$requires = substitute(selected.cpo == n, list(n = n))
      } else {
        p$requires = substitute((selected.cpo == n) && (otherreq), list(n = n, otherreq = p$requires))
      }
      p
    })
    ps
  }))

  paramset = c(pSSLrn(selected.cpo = selected.cpo: discrete[names(cpos)]), paramset.list)

  pv = unlist(unname(lapply(constructed, getHyperPars)), recursive = FALSE)

  pr = collectProperties(constructed)

  rl = makeCPOExtended("multiplex", .par.set = paramset, .par.vals = pv, .dataformat = "task", .properties = pr$properties, .properties.adding = pr$properties.adding,
          .properties.needed = pr$properties.needed, .properties.target = pr$properties.target, cpo.trafo = function(data, target, selected.cpo, ...) {
            cpo = constructed[[selected.cpo]]
            cpo = setHyperPars(cpo, par.vals = list(...)[names(getParamSet(cpo)$pars)])
            res = data %>>% cpo
            control = retrafo(res)
            retrafo(res) = NULL
            res
          }, cpo.retrafo = function(data, control, ...) { data %>>% control })
  setCPOId(rl(export = export, affect.type = affect.type, affect.index = affect.index, affect.names = affect.names, affect.pattern = affect.pattern,
    affect.invert = affect.invert, affect.pattern.ignore.case = affect.pattern.ignore.case, affect.pattern.perl = affect.pattern.perl,
    affect.pattern.fixed = affect.pattern.fixed), id = id)  # allow NULL id for multiplexer
}
registerCPO(list(name = "cpoMultiplex", cponame = "multiplex"), "meta", NULL, "Apply one of a given set of CPOs, each having their hyperparameters exported.")

#' @title CPO Wrapper
#'
#' Applies the \code{CPO} that is given to the \code{CPO} hyperparameter.
#'
#' @template cpo_description
#'
#' @param cpo [\code{CPO}]\cr
#'   The CPO to wrap.
#' @template arg_cpo_id
#' @family CPO
#' @export
cpoWrap = makeCPOExtended("wrap", .par.set = makeParamSet(makeUntypedLearnerParam("cpo")), .dataformat = "task",  # nolint
                   cpo.trafo = { control = retrafo({res = data %>>% cpo}) ; res }, cpo.retrafo = { data %>>% control })
# FIXME: require databound
registerCPO(cpoWrap, "meta", NULL, "Apply a freely chosen CPOs, without exporting its hyperparameters.")



#' @title Build data-dependent CPOs
#'
#' @description
#' The meta CPO which determines what CPO to apply to a data depending on
#' a provided function
#'
#' @param ...
#'   Parameters of the CPO, in the format of \code{\link[ParamHelpers]{pSS}}.
#' @param .par.set [\code{ParamSet}]\cr
#'   Optional parameter set. If this is not \code{NULL}, the \dQuote{...} parameters are ignored.
#'   Default is \code{NULL}.
#' @param .par.vals [\code{list}]\cr
#'   Named list of default parameter values for the CPO. These are used additionally to the
#'   parameter default values in \dQuote{...} and \code{.par.set}. It is preferred to use
#'   these default values, and not \code{.par.vals}. Default is \code{list()}.
#' @param .export [\code{list} of \code{CPO}]\cr
#'   List of \code{CPO} objects that have their hyperparameters exported. If this is not a named list, the item's \code{\link{getCPOId}}
#'   is used for names. The \code{cpo.build} function needs to have an argument for each of the names in the list. The \code{CPO} objects
#'   are pre-configured by the framework to have the hyperparameter settings as set by the ones exported by \code{cpoCase}.
#' @param .dataformat [\code{character(1)}]\cr
#'   Indicate what format the data should be as seen by \dQuote{cpo.trafo} and \dQuote{cpo.retrafo}. Possibilities are:
#'   \tabular{lll}{
#'     \bold{dataformat} \tab \bold{data}               \tab \bold{target}         \cr
#'     df.all      \tab data.frame with target cols     \tab target colnames       \cr
#'     df.features \tab data.frame without target       \tab data.frame of target  \cr
#'     task        \tab full task                       \tab target colnames       \cr
#'     split       \tab list of data.frames by type     \tab data.frame of target  \cr
#'     [type]      \tab data.frame of [type] feats only \tab data.frame of target  \cr
#'   }
#'   [type] can be any one of \dQuote{factor}, \dQuote{numeric}, \dQuote{ordered}.\cr
#'   For \code{.dataformat} \code{==} \dQuote{split}, the list has entries \dQuote{factor}, \dQuote{numeric},
#'   \dQuote{other}, and possibly \dQuote{ordered}--the last one only present if \code{.dataformat.factor.with.ordered}
#'   is \code{FALSE}.
#'
#'   If the CPO is a Target Operation CPO, the return value of both \dQuote{cpo.trafo} and \dQuote{cpo.retrafo}
#'   must be either a task if \code{.dataformat} is \dQuote{task}, the complete (modified) data.frame
#'   if \code{.dataformat} is \dQuote{df.all}, and a data.frame containing only the target column(s) otherwise.
#'   Default is \dQuote{df.features}.
#'
#'   If the CPO is a Feature Operation CPO, then the return value must be in the same format as the one requested.
#'   E.g. if \code{.dataformat} is \dQuote{split}, the return value must be a named list with entries \dQuote{numeric},
#'   \dQuote{factor}, and \dQuote{other}. The types of the returned data may be arbitrary: In the given example,
#'   the \dQuote{factor} slot of the returned list may contain numeric data. (Note however that if data is returned
#'   that has a type not already present in the data, \dQuote{.properties.needed} must specify this.)
#'
#'   If \code{.dataformat} is either \dQuote{df.all} or \dQuote{task}, the
#'   target column(s) in the returned value must be identical with the target column(s) given as input.
#'
#'   If \dQuote{.dataformat} is \dQuote{split}, the \dQuote{$numeric} slot of the returned
#'   object may also be a \code{matrix}. If \dQuote{.dataformat} is \dQuote{numeric}, the returned object may also be a
#'   matrix.
#' @param .dataformat.factor.with.ordered [\code{logical(1)}]\cr
#'   Whether to treat \code{ordered} typed features as \code{factor} typed features. This affects how \code{.dataformat} is handled, and only
#'   has an effect if \code{dataformat} is \dQuote{split} or \dQuote{factor}.
#' @param .properties [\code{character}]\cr
#'   The kind if data that the CPO will be able to handle. This can be one or many of: \dQuote{numerics},
#'   \dQuote{factors}, \dQuote{ordered}, \dQuote{missings}.
#'   There should be a bias towards including properties. If a property is absent, the preproc
#'   operator will reject the data. If an operation e.g. only works on numeric columns that have no
#'   missings (like PCA), it is recommended to give all properties, ignore the columns that
#'   are not numeric (using \dQuote{.dataformat} = \dQuote{split}), and giving an error when
#'   there are missings in the numeric columns (since missings in factorial features are not a problem).
#'   Defaults to the maximal set.
#' @param .properties.adding [\code{character}]\cr
#'   Can be one or many of the same values as \dQuote{.properties} for Feature Operation CPOs, and one or many of the same values as \dQuote{.properties.target}
#'   for Target Operation CPOs. These properties get added to a Learner (or CPO) coming after / behind this CPO. When a CPO imputes missing values, for example,
#'   this should be \dQuote{missings}. This must be a subset of \dQuote{.properties} or \dQuote{.properties.target}. Default is
#'   \code{character(0)}.
#' @param .properties.needed [\code{character}]\cr
#'   Can be one or many of the same values as \dQuote{.properties} for Feature Operation CPOs,
#'   and one or many of the same values as \dQuote{.properties.target}. These properties are required
#'   from a Learner (or CPO) coming after / behind this CPO. E.g., when a CPO converts factors to
#'   numerics, this should be \dQuote{numerics} (and \dQuote{.properties.adding} should be \dQuote{factors}).
#'   Default is \code{character(0)}.
#' @param .properties.target [\code{character}]\cr
#'   For Feature Operation CPOs, this can be one or many of \dQuote{cluster}, \dQuote{classif}, \dQuote{multilabel}, \dQuote{regr}, \dQuote{surv},
#'   \dQuote{oneclass}, \dQuote{twoclass}, \dQuote{multiclass}. Just as \code{.properties}, it
#'   indicates what kind of data a CPO can work with. Data given as data.frame needs the \dQuote{cluster} property. Default is the maximal set.
#'
#'   For Target Operation CPOs, this should only be given if the CPO operates on classification tasks. It must then be a subset of \dQuote{oneclass},
#'   \dQuote{twoclass}, or \dQuote{multiclass}. Otherwise, it should be \code{character(0)}. Default is \code{character(0)}.
#' @param cpo.build [\code{function}]\cr
#'   This function works similar to \code{cpo.trafo} in \code{\link{makeCPO}}: It has the arguments \code{data}, \code{target}, one argument for each
#'   hyperparameter declared in \code{.par.set} or \code{...}. However, it also has one parameter for each entry in \code{.export}, named as each item
#'   name in that list. The \code{cpoCase} framework supplies the pre-configured \code{CPO}s (pre-configured as the exported hyperparameters of \code{cpoCase}
#'   demand) to the \code{cpo.build} code via these parameters. The return value of \code{cpo.build} must be a \code{CPO}, which will then be used on the data.
#' @family CPO
#' @export
cpoCase = function(..., .par.set = NULL, .par.vals = list(), .export = list(),
                   .dataformat = c("df.features", "split", "df.all", "task", "factor", "ordered", "numeric"),
                   .dataformat.factor.with.ordered = TRUE,
                   .properties = NULL, .properties.adding = NULL, .properties.needed = NULL,
                   .properties.target = NULL, cpo.build) {
  .dataformat = match.arg(.dataformat)
  if (is.null(names(.export))) {
    names(.export) = sapply(.export, function(c) {
      if ("CPOConstructor" %in% class(c)) {
        stopf("If .export has no names, all CPOs must be constructed. %s is not.",
          getCPOName(c))
      } else if ("CPOPrimitive" %in% class(c)) {
        id = getCPOId(c)
        if (is.null(id)) {
          stopf("If .export has no names, all CPOs must have (unique) IDs.")
        }
        id
      } else {
        stopf("If .export has no names, compound CPOs can't be given.")
      }
    })
  }
  assertList(.export, types = c("CPO", "CPOConstructor"), names = "unique")  # FIXME: target bound separately

  constructed = lapply(names(.export), function(n) {
    if ("CPOConstructor" %in% class(.export[[n]])) {
      .export[[n]](id = n)
    } else {
      .export[[n]]
    }
  })
  names(constructed) = names(.export)

  if (is.null(.par.set)) {
    .par.set = pSSLrn(..., .pss.env = parent.frame())
  }

  paramset.pass.on = .par.set
  pv.pass.on = insert(getParamSetDefaults(paramset.pass.on), .par.vals)
  if (length(paramset.pass.on$pars)) {
    assertSubset(names(pv.pass.on), names(paramset.pass.on$pars))
  } else {
    assert(length(pv.pass.on) == 0)
  }
  name.collision = intersect(names(paramset.pass.on$pars), names(.export))
  if (length(name.collision)) {
    stopf("Names of cpo.build function arguments and .export elements clash: %s", collapse(name.collision, sep = ", "))
  }

  paramset.others = do.call(base::c, lapply(names(constructed), function(n) getParamSet(constructed[[n]])))
  pv.others = unlist(unname(lapply(constructed, getHyperPars)), recursive = FALSE)


  pr = collectProperties(constructed)
  .properties = firstNonNull(.properties.adding, pr$properties, (cpo.dataproperties))
  .properties.adding = firstNonNull(.properties.adding, pr$properties.adding, character(0))
  .properties.needed = firstNonNull(.properties.needed, pr$properties.needed, character(0))
  .properties.target = firstNonNull(.properties.target, pr$properties.target, c(cpo.targetproperties, cpo.tasktypes))

  required.arglist = lapply(c(paramset.pass.on$pars, .export), function(dummy) substitute())
  required.arglist = insert(required.arglist, pv.pass.on)
  required.arglist$data = substitute()
  required.arglist$target = substitute()

  buildfun = makeFunction(substitute(cpo.build), required.arglist, env = parent.frame())

  fullaffect = list(type = c("numeric", "factor", "ordered", "other"),
    index = integer(0), names = character(0), pattern = NULL, invert = FALSE, pattern.ignore.case = FALSE,
    pattern.perl = FALSE, pattern.fixed = FALSE)

  if (.dataformat == "split") {
    .dataformat = ifelse(.dataformat.factor.with.ordered, "most", "all")
  } else if (.dataformat == "factor" && !.dataformat.factor.with.ordered) {
    .dataformat = "onlyfactor"
  }

  rl = makeCPOExtended("case", .par.set = c(paramset.pass.on, paramset.others), .par.vals = c(pv.pass.on, pv.others),
    .dataformat = "task", .dataformat.factor.with.ordered = FALSE, .properties = .properties, .properties.adding = .properties.adding,
    .properties.needed = .properties.needed, .properties.target = .properties.target,
    cpo.trafo = function(data, target, ...) {
      args = list(...)
      buildfunargs = c(args[names(paramset.pass.on$pars)], lapply(.export, function(cpo)
        setHyperPars(cpo, par.vals = args[names(getParamSet(cpo)$pars)])))
      tin = prepareTrafoInput(data, .dataformat, c(.properties, .properties.target, cpo.predict.properties), fullaffect, FALSE, "case")

      cpo = do.call(buildfun, insert(buildfunargs, tin$indata))
      result = data %>>% cpo
      control = retrafo(result)
      retrafo(result) = NULL
      result
    },
    cpo.retrafo = function(data, control, ...) {
      data %>>% control
    })
  setCPOId(rl(), id = NULL)
}

# intersect: properties get intersected instead of union'd
collectProperties = function(constructed, do.intersect = FALSE) {
  oc = constructed
  constructed = Filter(function(x) !is.nullcpo(x), constructed)
  allprops = lapply(constructed, getCPOProperties, only.data = TRUE)
  all.allprops = lapply(constructed, function(x) setdiff(getCPOProperties(x)$properties, c(cpo.dataproperties, cpo.predict.properties)))

  if (do.intersect) {
  list(
      properties = Reduce(intersect, extractSubList(allprops, "properties", simplify = FALSE), cpo.dataproperties),
      properties.needed = Reduce(union, extractSubList(allprops, "properties.needed", simplify = FALSE), character(0)),
      properties.adding = Reduce(intersect, extractSubList(allprops, "properties.adding", simplify = FALSE),
        if (!length(oc) || length(oc) > length(constructed)) character(0) else cpo.dataproperties),
      properties.target = Reduce(intersect, all.allprops, c(cpo.tasktypes, cpo.targetproperties)))
  } else {
    list(
        properties = Reduce(union, extractSubList(allprops, "properties", simplify = FALSE)),
        properties.needed = Reduce(intersect, extractSubList(allprops, "properties.needed", simplify = FALSE)),
        properties.adding = Reduce(union, extractSubList(allprops, "properties.adding", simplify = FALSE)),
        properties.target = Reduce(union, all.allprops))
  }
}
