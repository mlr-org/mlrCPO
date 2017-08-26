
##################################
### CPO Trafo Composition      ###
##################################
# (as opposed to retrafo, inverter ops)

# CPO %>>% CPO
# Just creates a 'CPOPipeline' object
#' @export
composeCPO.CPO = function(cpo1, cpo2) {
  assertClass(cpo2, "CPO")
  parameterClashAssert(cpo1, cpo2, cpo1$name, cpo2$name)
  newprops = compositeProperties(cpo1$properties, cpo2$properties, cpo1$name, cpo2$name)
  newpt = chainPredictType(cpo1$predict.type, cpo2$predict.type, cpo1$name, cpo2$name)

  makeS3Obj(c("CPOPipeline", "CPO"),
    # --- CPO Part
    bare.name = paste(cpo2$bare.name, cpo1$bare.name, sep = "."),
    name = paste(cpo1$name, cpo2$name, sep = " >> "),
    par.set = c(cpo1$par.set, cpo2$par.set),
    par.vals = c(cpo1$par.vals, cpo2$par.vals),
    properties = newprops,
    bound = unique(cpo1$bound, cpo2$bound),
    predict.type = newpt,
    # --- CPOPipeline part
    first = cpo1,
    second = cpo2)
}

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

##################################
### Retrafo Composition        ###
##################################

# RETRAFO %>>% RETRAFO
# Check types, check properties.
# Since "Retrafo" and "Inverter" are fundamentally the
# same class, some special cases need to be handled.
#' @export
composeCPO.CPORetrafo = function(cpo1, cpo2) {
  assertClass(cpo2, "CPORetrafo")
  is.prim = "CPORetrafoPrimitive" %in% class(cpo2)
  assert(is.prim == is.null(cpo2$prev.retrafo))
  newkind = intersect(cpo1$kind, cpo2$kind)
  if (!length(newkind)) {
    stopf("Cannot compose retrafos of kind %s with retrafos of kind %s.",
      collapse(cpo1$kind), collapse(cpo2$kind))
  }
  if (!is.prim) {
    cpo1 = composeCPO(cpo1, cpo2$prev.retrafo)
  }
  class(cpo2) = setdiff(class(cpo2), "CPORetrafoPrimitive")

  # check for properties match
  if ("retrafo" %in% newkind) {
    cpo2$properties.needed = compositeProperties(
        list(properties = character(0), properties.adding = character(0), properties.needed = cpo1$properties.needed),
        cpo2$cpo$properties, getCPOName(cpo1), cpo2$cpo$bare.name)$properties.needed
  }
  if ("inverter" %in% newkind) {
    if (length(newkind) == 1) {
      # pure inverter chaining: do myopic property checking
      assert(length(cpo1$kind) == 1)
      assertString(cpo1$cpo$convertto)
      assertString(cpo2$cpo$convertfrom)
      if (cpo1$convertto != cpo2$convertfrom) {
        stopf("Incompatible chaining of inverters: %s converts to %s, but %s needs %s.",
          cpo1$cpo$barelname, cpo1$cpo$convertto, cpo2$cpo$bare.name, cpo2$cpo$bare.name)
      }
      compositeProperties(cpo1$cpo$properties, cpo2$cpo$properties, cpo1$cpo$bare.name, cpo2$cpo$bare.name)  # just for checking
    }
    assert(length(cpo1$predict.type) <= 2)  # predict.type cannot be the identity
    assert(length(cpo2$predict.type) <= 2)  # the identity (and only the identity) has more than 2 elements.
  }

  cpo2$predict.type = chainPredictType(cpo1$predict.type, cpo2$cpo$predict.type, getCPOName(cpo1), cpo2$cpo$bare.name)
  cpo2$prev.retrafo = cpo1
  cpo2$bound = unique(cpo2$cpo$bound, cpo1$bound)
  cpo2$kind = newkind
  cpo2
}

# chain CPOs with predict.type pt1 %>>% pt2  # FIXME: sort this where it belongs
# the 'predict.type' slot is a map (i.e. a named vector of character that maps a -> predict.type[a])
# when chaining CPOs, the predict.types need to be chained as well.
chainPredictType = function(pt1, pt2, name1, name2) {
  result = sapply(pt1, function(x) unname(pt2[x]))
  result = result[!is.na(result)]
  if (!length(result)) {
    # So this is a bit of a weird situation: The CPO chain would work for trafo AND retrafo, but not for predictions.
    stopf("Incompatible chaining of inverters: %s needs a predict.type being%s '%s', but %s can only deliver type%s '%s'.",
      name1, ifelse(length(pt1) == 1, " one of", ""), collapse(pt1, sep = "', '"),
      name2, ifelse(length(pt2) > 1, "s", ""), collapse(pt2, sep = "', '"))
  }
  result
}

# RETRAFO splitting
# retrafos are a linked list, so on a basic level what happens is
# c(as.list(x$prev.retrafo), list({x$prev.retrafo = NULL ; x}))
# However, some other things that happen in composeCPO.CPORetrafo need to be undone.
#' @export
as.list.CPORetrafo = function(x, ...) {
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
  class(x) = unique(c("CPORetrafoPrimitive", class(x)))
  c(prev, list(x))
}



# Param Sets
#' @export
getParamSet.CPO = function(x) {
  x$par.set
}

#' @export
getHyperPars.CPO = function(learner, for.fun = c("train", "predict", "both")) {
  learner$par.vals
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
getParamSet.CPORetrafoPrimitive = function(x) {
  x$cpo$par.set
}

#' @export
getHyperPars.CPORetrafoPrimitive = function(learner, for.fun = c("train", "predict", "both")) {
  learner$cpo$par.vals
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

# CPO ID, NAME

#' @export
getCPOName.CPO = function(cpo) {
  cpo$name
}

#' @export
getCPOProperties.CPORetrafo = function(cpo, only.data = FALSE) {
  if (!is.null(cpo$prev.retrafo)) {
    props = compositeProperties(getCPOProperties(cpo$prev.retrafo), cpo$cpo$properties, "[PREVIOUS RETRAFO CHAIN]", cpo$cpo$bare.name)
  } else {
    props = cpo$cpo$properties
  }
  if (only.data) {
    lapply(props, intersect, y = cpo.dataproperties)
  } else {
    props
  }
}

#' @export
getCPOName.CPORetrafoPrimitive = function(cpo) {
  cpo$cpo$bare.name
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


# When changing the ID, we need to change each parameter's name, which
# should have the form <ID>.<bare.par.name>
# This means we need to modify $par.set AND $par.vals
#' @export
setCPOId.CPOPrimitive = function(cpo, id) {
  if (is.null(id)) {
    id = cpo$bare.name
  }
  cpo$id = id
  cpo$name = if (id == cpo$bare.name) cpo$bare.name else sprintf("%s<%s>", cpo$id, cpo$bare.name)
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
