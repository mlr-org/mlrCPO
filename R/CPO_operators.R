
##################################
### CPO Trafo Composition      ###
##################################
# (as opposed to retrafo, inverter ops)

# CPO %>>% CPO
# Just creates a 'CPOPipeline' object
#' @export
composeCPO.CPO = function(cpo1, cpo2) {
  assertClass(cpo2, "CPO")
  parameterClashAssert(cpo1, cpo2, cpo1$debug.name, cpo2$debug.name)
  newprops = compositeProperties(cpo1$properties, cpo2$properties, cpo1$debug.name, cpo2$debug.name)
  newpt = chainPredictType(cpo1$predict.type, cpo2$predict.type, cpo1$debug.name, cpo2$debug.name)

  makeS3Obj(c("CPOPipeline", "CPO"),
    # --- CPO Part
    name = paste(cpo2$name, cpo1$name, sep = "."),
    debug.name = paste(cpo1$debug.name, cpo2$debug.name, sep = " >> "),
    par.set = c(cpo1$par.set, cpo2$par.set),
    par.vals = c(cpo1$par.vals, cpo2$par.vals),
    properties = newprops,
    operating.type = unique(c(cpo1$operating.type, cpo2$operating.type)),
    predict.type = newpt,
    # --- CPOPipeline part
    first = cpo1,
    second = cpo2)
}

##################################
### Retrafo Composition        ###
##################################

# RETRAFO %>>% RETRAFO
# Check types, check properties.
# Since "Retrafo" and "Inverter" are fundamentally the
# same class, some special cases need to be handled.
#' @export
composeCPO.CPOTrained = function(cpo1, cpo2) {
  assertClass(cpo2, "CPOTrained")
  if (is.nullcpo(cpo2)) {
    return(cpo1)
  }
  is.prim = "CPOTrainedPrimitive" %in% class(cpo2)
  assert(is.prim == is.null(cpo2$prev.retrafo))

  objtype = c(getCPOObjectType(cpo1), getCPOObjectType(cpo2))
  invcap = c(getCPOInvertCapability(cpo1), getCPOInvertCapability(cpo2))

  # are we composing the retrafo part?
  retrafo.composition = all(objtype == "CPORetrafo")  # invcap are retrafo, retrafo.only, or hybrid
  # is the inverter part a noop composition?
  inverter.noop.composition = all(invcap == "retrafo")
  # do we need to compose inverter at all?
  inverter.composition = !inverter.noop.composition && all(invcap != "retrafo.only")
  # are both involved CPOs actual inverters?
  pure.inverter.composition = all(objtype == "CPOInverter")

  if (!retrafo.composition && !inverter.composition) {
    # don't need to check inverter.noop.composition, since that is only true if retrafo.composition.
    stopf("Cannot compose retrafos with capability %s with retrafos with capability %s.",
      getCPOInvertCapability(cpo1), getCPOInvertCapability(cpo2))
  }
  if (!is.prim) {
    cpo1 = composeCPO(cpo1, cpo2$prev.retrafo)
  }

  # check for properties match
  if (retrafo.composition) {
    cpo2$properties.needed = compositeProperties(
        list(properties = character(0), properties.adding = character(0), properties.needed = cpo1$properties.needed),
        cpo2$cpo$properties, getCPOName(cpo1), cpo2$cpo$name)$properties.needed
  }
  if (inverter.composition) {
    if (pure.inverter.composition) {
      # pure inverter chaining: do myopic property checking
      assertString(cpo1$cpo$convertto)
      assertString(cpo2$cpo$convertfrom)
      if (cpo1$convertto != cpo2$convertfrom) {
        stopf("Incompatible chaining of inverters: %s converts to %s, but %s needs %s.",
          cpo1$cpo$name, cpo1$cpo$convertto, cpo2$cpo$name, cpo2$cpo$convertfrom)
      }
      compositeProperties(cpo1$cpo$properties, cpo2$cpo$properties, getCPOName(cpo1), cpo2$cpo$name)  # just for checking
    }
    assert(length(cpo1$predict.type) <= 2,  # at least one predict.type cannot be the identity (since otherwise its a noop-composition)
      length(cpo2$predict.type) <= 2)       # the identity (and only the identity) has more than 2 elements.
  }

  cpo2$predict.type = chainPredictType(cpo1$predict.type, cpo2$cpo$predict.type, getCPOName(cpo1), cpo2$cpo$name)
  cpo2$prev.retrafo = cpo1

  if (retrafo.composition) {
    # any chance of the result not just being an inverter?
    if (inverter.noop.composition) {
      # two CPORetrafos --> CPORetrafo
      newclass = "CPORetrafo"
    } else if (inverter.composition) {
      # at least one non-pure CPORetrafo, but the CPORetrafo allows inversion
      newclass = c("CPORetrafoHybrid", "CPORetrafo")
    } else {
      # at least one retrafo.only
      newclass = c("CPORetrafoOnly", "CPORetrafo")
    }
  } else {
    newclass = "CPOInverter"
  }

  class(cpo2) = setdiff(class(cpo2), c("CPOTrainedPrimitive", "CPORetrafoHybrid", "CPORetrafoOnly", "CPORetrafo", "CPOInverter")))

  addClasses(cpo2, newclass)
}

# chain CPOs with predict.type pt1 %>>% pt2
# the 'predict.type' slot is a map (i.e. a named vector of character that maps a -> predict.type[a])
# when chaining CPOs, the predict.types need to be chained as well.
chainPredictType = function(pt1, pt2, name1, name2) {
  result = sapply(pt1, function(x) unname(pt2[x]))
  result = result[!is.na(result)]
  if (isPropertyStrict() && !length(result)) {
    # So this is a bit of a weird situation: The CPO chain would work for trafo AND retrafo, but not for predictions.
    stopf("Incompatible chaining of inverters: %s needs a predict.type being%s '%s', but %s can only deliver type%s '%s'.",
      name1, ifelse(length(pt1) == 1, " one of", ""), collapse(pt1, sep = "', '"),
      name2, ifelse(length(pt2) > 1, "s", ""), collapse(pt2, sep = "', '"))
  }
  result
}

##################################
### Splitting                  ###
##################################

# CPO splitting
# Splitting a primitive object gives a list of that object
#' @export
as.list.CPOPrimitive = function(x, ...) {
  assert(length(list(...)) == 0)
  list(x)
}

# Compound objects are a binary tree, so
# splitting a compound object recursively calls as.list to both children
# and then concatenates.
#' @export
as.list.CPOPipeline = function(x, ...) {
  first = x$first
  second = x$second
  first$par.vals = subsetParams(x$par.vals, first$par.set)
  second$par.vals = subsetParams(x$par.vals, second$par.set)
  c(as.list(first), as.list(second))
}

# RETRAFO splitting
# retrafos are a linked list, so on a basic level what happens is
# c(as.list(x$prev.retrafo), list({x$prev.retrafo = NULL ; x}))
# However, some other things that happen in composeCPO.CPOTrained need to be undone.
#' @export
as.list.CPOTrained = function(x, ...) {
  assert(length(list(...)) == 0)
  prev = if (!is.null(x$prev.retrafo)) as.list(x$prev.retrafo)
  x$prev.retrafo = NULL
  if ("retrafo" %in% x$kind) {
    x$properties.needed = x$cpo$properties$properties.needed
  }
  x$predict.type = x$cpo$predict.type
  x$bound = x$cpo$bound
  if (identical(x$kind, "retrafo")) {
    if (x$cpo$hybrid.retrafo) {
      x$kind = c("retrafo", "inverter")
    }
  }
  class(x) = unique(c("CPOTrainedPrimitive", class(x)))
  c(prev, list(x))
}

##################################
### Chaining                   ###
##################################

#' @title Turn a list of preprocessing operators into a single chained one
#'
#' @description
#' Chain a list of preprocessing operators, or retrafo objects, turning \code{list(a, b, c)} into
#' \code{a \%>>\% b \%>>\% c}. This is the inverse operation of \code{as.list},
#' applied on a \code{CPO} chain.
#'
#' @param pplist [\code{list} of \code{CPO} | \code{list} of \code{CPOTrained}]\cr
#'   A list of \code{CPO} or \code{CPOTrained} objects.
#'
#' @family CPO
#' @export
pipeCPO = function(pplist) {
  assert(checkList(pplist, types = "CPO"),
    checkList(pplist, types = "CPOTrained"))
  Reduce(composeCPO, c(list(NULLCPO), pplist))
}

##################################
### Getters and Setters        ###
##################################

# Param Sets and related

#' @export
getParamSet.CPO = function(x) {
  x$par.set
}

#' @export
getParamSet.CPOTrainedPrimitive = function(x) {
  x$cpo$par.set
}

#' @export
getHyperPars.CPO = function(learner, for.fun = c("train", "predict", "both")) {
  learner$par.vals
}

#' @export
getHyperPars.CPOTrainedPrimitive = function(learner, for.fun = c("train", "predict", "both")) {
  learner$cpo$par.vals
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
setCPOId.CPO = function(cpo) {
  stop("Cannot set ID of compound CPO.")
}

# When changing the ID, we need to change each parameter's name, which
# should have the form <ID>.<bare.par.name>
# This means we need to modify $par.set AND $par.vals
#' @export
setCPOId.CPOPrimitive = function(cpo, id) {
  if (is.null(id)) {
    id = cpo$name
  }
  cpo$id = id
  cpo$debug.name = if (id == cpo$name) cpo$name else sprintf("%s<%s>", cpo$id, cpo$name)
  cpo$par.vals = getBareHyperPars(cpo)
  cpo$par.set = cpo$bare.par.set
  pars = cpo$par.set$pars
  if (length(pars)) {
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

