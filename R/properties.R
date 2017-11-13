# properties.R contains getXXX and setXXX functions that apply to CPO objects.
#' @include callCPO.R

#' @title Get the CPO object's Name
#'
#' @description
#' Return the name given at creation as \dQuote{.cpo.name} to the
#' CPO creator. If the CPO object has an ID, it will be appended.
#'
#' @param cpo [\code{\link{CPO}}]\cr
#'   The CPO object.
#'
#' @family CPO
#' @export
getCPOName = function(cpo) {
  UseMethod("getCPOName")
}

#' @title Set the ID of a CPO object.
#'
#' @description
#' Setting the ID of a CPO to a value will prefix all its
#' parameter names with this ID. This makes it possible to
#' compose CPOs that have clashing parameter names.
#'
#' @param cpo [\code{CPO}]\cr
#'   The \code{CPO} object to modify.
#' @param id [\code{character(1)} | \code{NULL}]\cr
#'   The ID. If this is \code{NULL}, the ID is set to the
#'   default for the CPO at hand.
#' @return [\code{CPO}] the CPO with modified id.
#' @family CPO
#' @export
setCPOId = function(cpo, id) {
  if (!is.null(id)) {
    assertString(id)
  }
  UseMethod("setCPOId")
}

#' @export
setCPOId.default = function(cpo, id) {
  stop("setCPOId for object not defined.")
}


#' @title Get the ID of a CPO object.
#'
#' @description
#' The ID of a CPO to a value will prefix all its
#' parameter names with this ID.
#'
#' @param cpo [\code{CPO}]\cr
#'   The object to query.
#'
#' @family CPO
#' @export
getCPOId = function(cpo) {
  UseMethod("getCPOId")
}

#' @title Get the Properties of the given CPO object
#'
#' @description
#' The properties of a CPO object determine the kind of data the CPO will be able to handle, and how
#' it transforms data. Each entry can be one of: \dQuote{numerics}, \dQuote{factors}, \dQuote{ordered},
#' \dQuote{missings}.
#'
#' This function returns a list of three values: \dQuote{properties}, \dQuote{properties.adding}, and
#' \dQuote{properties.needed}.
#'
#' The \dQuote{properties} determines what data the CPO handles. If a property of a given data set is absent,
#' the preproc operator will reject the data.
#'
#' \dQuote{properties.adding} can be one or many of the same values as \dQuote{properties}. These properties
#' get added to a Learner (or CPO) coming after / behind this CPO. When a CPO imputes missing values, for example,
#' this is \dQuote{missings}. This is always a subset of \dQuote{properties}.
#'
#' \dQuote{properties.needed} can be one or many of the same values as \dQuote{properties}. These properties
#' are required from a Learner (or CPO) coming after / behind this CPO. E.g., when a CPO converts factors to
#' numerics, this is \dQuote{numerics} (and \dQuote{properties.adding} is \dQuote{factors}).
#'
#' @param cpo [\code{CPO}]\cr
#'   The CPO to query.
#'
#' @param only.data [\code{logical(1)}]\cr
#'   Only get the CPO properties relevant for data (not target or task types). Default is \code{FALSE}.
#'
#' @aliases CPOProperties
#' @export
getCPOProperties = function(cpo, only.data = FALSE) {
  assertFlag(only.data)
  UseMethod("getCPOProperties")
}

#' @title Get the Selection Arguments for affected CPOs
#'
#' @description
#' Get the \code{affected.*} arguments from when the CPO was constructed.
#'
#' @param cpo [\code{CPO}]\cr
#'   The CPO.
#' @param drop.defaults [\code{logical(1)}]\cr
#'   Whether to only return the arguments that deviate from the default.
#'   Default is \code{TRUE}.
#'
#' @family CPO
#' @export
getCPOAffect = function(cpo, drop.defaults = TRUE) {
  UseMethod("getCPOAffect")
}

#' @title Get the CPO Object Type
#'
#' @description
#' Get the type / functionality provided by the given CPO object.
#' There is a fundamental distinction between \code{CPO} objects
#' and \code{CPOTrained} objects, the latter of which can provide either
#' retrafo or inverter functionality, or both. \code{CPOTrained} are sublassed into
#' \code{CPOInverter} (only inverter functionality), or
#' \code{CPORetrafo} (retrafo, possibly also inverter). To get more information
#' about a \code{CPORetrafo} object's capabilities, use \code{\link{getCPOInvertCapability}}.
#'
#'
#' @param cpo [\code{CPO} | \code{CPOTrained}]\cr
#'   The CPO.
#'
#' @return [\code{character(1)}]: \dQuote{CPO} if the given object is a CPO,
#'   \dQuote{CPOInverter} if the object is an inverter only,
#'   \dQuote{CPORetrafo} if the object is a retrafo object (which may have inverter capabilities),
#'   \dQuote{NULLCPO} if the object is \code{NULLCPO}.
#'
#' @export
getCPOObjectType = function(cpo) {
  UseMethod("getCPOObjectType")
}

#' @title Get the CPO's Inverter Capability
#'
#' @description
#' Both \code{CPORetrafo} and \code{CPOInverter} objects can be used for
#' \code{\link{invert}} in principle. However, some \code{CPORetrafo}
#' objects forbid inversion (use the \code{CPOInverter} object instead),
#' and some \code{CPORetrafo} objects are NO-OPS when called with \code{\link{invert}}.
#'
#' @param cpo [\code{CPOTrained}]\cr
#'   The \code{CPOTrained} object to query.
#'
#' @return [\code{character(1)}]: \dQuote{inverter} if given object is an inverter only,
#'   \dQuote{hybrid} if given object is retrafo and inverter,
#'   \dQuote{retrafo.only} if given object is retrafo only,
#'   \dQuote{retrafo} if given object is a retrafo that gives a NO-OP if used with \code{\link{invert}}.
#'
#' @export
getCPOInvertCapability = function(cpo) {
  UseMethod("getCPOInvertCapability")
}

#' @title Get the CPO predict.type
#'
#' @description
#' Get the possible predict.types this CPO is able to handle.
#'
#' @param cpo [\code{CPO}]\cr
#'   The CPO.
#'
#' @export
getCPOPredictType = function(cpo) {
  UseMethod("getCPOPredictType")
}

#' @title Determine the Operating Type of the CPO
#'
#' @description
#' Gives the \dQuote{operating type}, of a CPO or Retrafo, i.e. the part of a given data set it operates on.
#' This can be \dQuote{target} for a CPO / Retrafo / Inverter that
#' manipulates target columns, \dQuote{feature} for
#' a CPO / Retrafo that manipulates non-target columns,
#' or \dQuote{traindata} for a CPO that only handles training data
#' (and hence can manipulate both feature and target columns, but produces no retrafo).
#'
#' For a composite CPO / Retrafo of different operating types, all
#' types are returned. \code{NULLCPO} has no operating type.
#'
#' @param cpo [\code{CPO} | \code{CPOTrained}]\cr
#'   The CPO, Retrafo, or Inverter to inspect.
#'
#' @export
getCPOOperatingType = function(cpo) {
  UseMethod("getCPOOperatingType")
}

# Param Sets and related

#' @export
getParamSet.CPO = function(x) {
  x$par.set
}

#' @export
getParamSet.CPOTrainedPrimitive = function(x) {
  c(x$cpo$bare.par.set, x$cpo$unexported.par.set)
}

#' @export
getParamSet.CPOTrained = function(x) {
  stop("Cannot get param set of compound retrafo. Use as.list to get individual elements")
}

#' @export
getHyperPars.CPO = function(learner, for.fun = c("train", "predict", "both")) {
  learner$par.vals
}

#' @export
getHyperPars.CPOTrainedPrimitive = function(learner, for.fun = c("train", "predict", "both")) {
  getBareHyperPars(learner$cpo)
}

#' @export
getHyperPars.CPOTrained = function(learner, for.fun = c("train", "predict", "both")) {
  stop("Cannot get parameters of compound retrafo. Use as.list to get individual elements")
}

#' @export
setHyperPars2.CPO = function(learner, par.vals = list()) {
  badpars = setdiff(names(par.vals), names(learner$par.set$pars))
  if (length(badpars)) {
    stopf("CPO %s does not have parameter%s %s", getLearnerName(learner),
          ifelse(length(badpars) > 1, "s", ""), collapse(badpars, ", "))
  }
  checkParamsFeasible(learner$par.set, par.vals)
  learner$par.vals = insert(learner$par.vals, par.vals)
  learner
}

#' @export
setHyperPars2.CPOTrained = function(learner, par.vals = list()) {
  stopf("Cannot change parameter values of retrafo / inverter object\n%s\n%s\n",
    "To create a retrafo / inverter with a specific state use makeRetrafoFromState.",
    "Get the state of an existing retrafo / inverter using getRetrafoState.")
}

# Properties

#' @export
getCPOProperties.CPO = function(cpo, only.data = FALSE) {
  if (only.data) {
    lapply(cpo$properties, intersect, y = cpo.dataproperties)
  } else {
    cpo$properties
  }
}

#' @export
getCPOProperties.CPOTrained = function(cpo, only.data = FALSE) {
  if (!is.null(cpo$prev.retrafo)) {
    props = compositeProperties(getCPOProperties(cpo$prev.retrafo), cpo$cpo$properties, "[PREVIOUS RETRAFO CHAIN]", cpo$cpo$name)
  } else {
    props = cpo$cpo$properties
  }
  if (only.data) {
    lapply(props, intersect, y = cpo.dataproperties)
  } else {
    props
  }
}

# CPO ID, NAME

#' @export
getCPOName.CPO = function(cpo) {
  cpo$name
}

#' @export
getCPOName.CPOTrainedPrimitive = function(cpo) {
  cpo$cpo$name
}

#' @export
getCPOName.CPOTrained = function(cpo) {
  paste(getCPOName(cpo$prev.retrafo), cpo$cpo$name, sep = ".")
}

#' @export
getCPOName.CPOConstructor = function(cpo) {
  environment(cpo)$.cpo.name
}

#' @export
getCPOId.CPOPrimitive = function(cpo) {
  cpo$id
}

#' @export
getCPOId.CPO = function(cpo) {
  stop("Compound CPOs have no IDs.")
}

#' @export
setCPOId.CPO = function(cpo, id) {
  stop("Cannot set ID of compound CPO.")
}

# When changing the ID, we need to change each parameter's name, which
# should have the form <ID>.<bare.par.name>
# This means we need to modify $par.set AND $par.vals
#' @export
setCPOId.CPOPrimitive = function(cpo, id) {

  cpo$id = id
  cpo$debug.name = if (is.null(id) || id == cpo$name) cpo$name else sprintf("%s<%s>", cpo$id, cpo$name)
  cpo$par.vals = getBareHyperPars(cpo, FALSE)
  cpo$par.set = cpo$bare.par.set
  pars = cpo$par.set$pars
  if (!is.null(id) && length(pars)) {
    trans = setNames(paste(id, names(pars), sep = "."), names(pars))
    names(pars) = trans
    pars = lapply(pars, function(x) {
      x$id = trans[x$id]
      if (!is.null(x$requires)) {
        x$requires = renameNonfunctionNames(x$requires, trans)
      }
      x
    })
    cpo$par.set$pars = pars
    names(cpo$par.vals) = trans[names(cpo$par.vals)]
  }
  cpo
}

# CPO Type

#' @export
getCPOObjectType.CPO = function(cpo) {
  "CPO"
}

#' @export
getCPOObjectType.CPORetrafo = function(cpo) {
  "CPORetrafo"
}

#' @export
getCPOObjectType.CPOInverter = function(cpo) {
  "CPOInverter"
}

#' @export
getCPOInvertCapability.CPOInverter = function(cpo) {
  "inverter"
}

#' @export
getCPOInvertCapability.CPORetrafo = function(cpo) {
  "retrafo"
}

#' @export
getCPOInvertCapability.CPORetrafoOnly = function(cpo) {
  "retrafo.only"
}

#' @export
getCPOInvertCapability.CPORetrafoHybrid = function(cpo) {
  "hybrid"
}

# Operating Type

#' @export
getCPOOperatingType.CPO = function(cpo) {
  cpo$operating.type
}

#' @export
getCPOOperatingType.CPOTrained = function(cpo) {
  switch(getCPOInvertCapability(cpo),
    inverter = "target",
    retrafo = "feature",
    retrafo.only = "feature",
    hybrid = c("target", "feature"))
}

# Predict Type

#' @export
getCPOPredictType.CPO = function(cpo) {
  names(cpo$predict.type)
}

#' @export
getCPOPredictType.CPOTrained = function(cpo) {
  names(cpo$predict.type)
}

# Normalize "affect.*" arguments of CPOs

#' @export
getCPOAffect.CPOPrimitive = function(cpo, drop.defaults = TRUE) {
  affect.args = cpo$affect.args
  if (!drop.defaults) {
    if (!length(getCPOAffect(cpo))) {
      affect.args$type = c("numeric", "factor", "ordered", "other")
    }
    return(affect.args)
  }
  if (setequal(affect.args$type, c("numeric", "factor", "ordered", "other"))) {
    affect.args$type = NULL
  }
  if (!length(affect.args$index)) {
    affect.args$index = NULL
  }
  if (!length(affect.args$names)) {
    affect.args$names = NULL
  }
  if (is.null(affect.args$pattern)) {
    affect.args$pattern.ignore.case = NULL
    affect.args$pattern.perl = NULL
    affect.args$pattern.fixed = NULL
  }
  Filter(function(x) !is.null(x) && !identical(x, FALSE), affect.args)
}

#' @export
getCPOAffect.CPO = function(cpo, drop.defaults = TRUE) {
  stop("Compound CPOs have no affect arguments.")
}

