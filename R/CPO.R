#' @include CPOFormatCheck.R
##################################
### Creators                   ###
##################################

# Creates the "Inverter" S3 object. Both "Inverter" and "Retrafo"
# have the class "CPORetrafo", with slight differences between them.
# Since a Retrafo can sometimes do the work of an Inverter, we can't
# use S3 to differentiate between them effectively.
makeCPOInverter = function(cpo, state, prev.inverter, data, shapeinfo) {
  if (!"Task" %in% class(data)) {
    data = makeClusterTask("CPO Generated", data, check.data = FALSE)
  }

  inverter = makeCPORetrafoBasic(cpo, state, prev.inverter, "inverter")
  # --- only in pure "inverter" kind
  inverter$indatatd = getTaskDesc(data)
  inverter$truth = prepareRetrafoData(data, cpo$datasplit, cpo$properties$properties, shapeinfo, cpo$bare.name)$target
  inverter
}

# Creates the "Retrafo" S3 object. See comment above 'makeCPOInverter'
makeCPORetrafo = function(cpo, state, prev.retrafo, shapeinfo.input, shapeinfo.output) {
  retrafo = makeCPORetrafoBasic(cpo, state, prev.retrafo, c("retrafo", if (cpo$hybrid.inverter) "inverter"))
  # --- only in "retrafo" kind
  retrafo$shapeinfo.input = shapeinfo.input
  retrafo$shapeinfo.output = shapeinfo.output
  retrafo$properties.needed = cpo$properties$properties.needed
  retrafo
}

# Creates an object of class "CPORetrafo", which
# serves as basis for both "Inverter" and "Retrafo" objects.
makeCPORetrafoBasic = function(cpo, state, prev.retrafo, kind) {
  retrafo = makeS3Obj(c("CPORetrafoPrimitive", "CPORetrafo"),
    cpo = setCPOId(cpo, NULL),
    state = state,
    prev.retrafo = NULL,
    # --- Target Bound things
    bound = cpo$bound,
    predict.type = cpo$predict.type,  # named list type to predict --> needed type
    kind = kind)
  if (!is.null(prev.retrafo)) {
    retrafo = composeCPO(prev.retrafo, retrafo)
  }
  retrafo
}


##################################
### Primary Operations         ###
##################################
# CPO is a tree datastructure. CPOPrimitive are
# the leaves, CPOPipeline the nodes.
# CPORetrafo is a linked list, which gets automatically
# constructed in 'callCPO'.

# Call the (possibly compound) CPO pipeline.
# Internal function; the user-facing functions also makes some checks
# and strips the retrafo and inverter tags.
# Parameters:
#   cpo, data: obvious
#   build.retrafo: boolean, whether to create 'retrafo' object
#   prev.retrafo: possible retrafo linked list that the newly created retrafo gets appended to
#   build.inverter: boolean, whether to create 'inverter' object, if applicable
#   prev.inverter: possible inverter linked list to append the new inverter to
# Returns:
#  list(data, retrafo, inverter)
callCPO = function(cpo, data, build.retrafo, prev.retrafo, build.inverter, prev.inverter) {
  UseMethod("callCPO")
}

# TRAFO main function
# - checks the inbound and outbound data is in the right format
# - data will be turned into the shape requested by the cpo
# - properties check (inbound, and outbound)
# - automatically subsets 'args' to the relevant ones for cpo
# - collects control / cpo.retrafo from called function
# - returns list(data, retrafo = [CPORetrafo object])
# attaches prev.retrafo to the returned retrafo object, if present.
callCPO.CPOPrimitive = function(cpo, data, build.retrafo, prev.retrafo, build.inverter, prev.inverter) {

  if (!"CPOCbind" %in% class(cpo)) {
    cpo$bare.par.set$pars = c(cpo$bare.par.set$pars, cpo$unexported.pars)
    cpo = setCPOId(cpo, cpo$id)
    cpo$par.vals = c(cpo$par.vals, cpo$unexported.args)
  }

  checkAllParams(cpo$par.vals, cpo$par.set, cpo$name)
  if (is.nullcpo(prev.retrafo)) {
    prev.retrafo = NULL
  }
  if (is.nullcpo(prev.inverter)) {
    prev.inverter = NULL
  }
  if (!build.inverter) {
    assertNull(prev.inverter)
    inverter = NULL
  }
  if (is.null(prev.retrafo)) {
    prevneeded = character(0)
  } else {
    prevneeded = prev.retrafo$properties.needed
    assertCharacter(prevneeded, unique = TRUE)
    assertSubset(prevneeded, cpo$properties$properties)  # this should never happen, since we test this during CPO composition
  }



  tin = prepareTrafoInput(data, cpo$datasplit, cpo$properties$properties.data, getCPOAffect(cpo, FALSE), cpo$fix.factors, cpo$name)
  if (!cpo$data.dependent) {
    assert(cpo$bound == "targetbound")
    tin$indata$data = NULL
  }
  if (is.null(cpo$trafo)) {
    # stateless trafo-less CPO
    tin$indata$target = NULL
    result = do.call(cpo$trafo, insert(getBareHyperPars(cpo), tin$indata))
  } else {
    result = do.call(cpo$trafo, insert(getBareHyperPars(cpo), tin$indata))

    trafoenv = .ENV
  }
  assertChoice(cpo$type, c("functional", "object"))
  if (cpo$type == "functional") {
    state = trafoenv$cpo.retrafo
    if (cpo$bound == "targetbound") {
      requiredargs = c("df.features", "predict.type")
      if (is.null(state) || !isTRUE(checkFunction(state, args = requiredargs, nargs = 2))) {
        stopf('.data.dependent targetbound CPO %s cpo.trafo must set a variable "cpo.retrafo"\n%s"%s".',
          cpo$name, "to a function with two arguments ", collapse(requiredargs, sep = '", "'))
      }
    } else if (is.null(state) || !isTRUE(checkFunction(state, nargs = 1))) {
      stopf("CPO %s cpo.trafo did not set a variable 'cpo.retrafo' to a function with one argument.", cpo$name)
    }
    if (!"data" %in% names(formals(state)) && referencesNonfunctionNames(body(state), "data") && cpo$data.dependent) {
      warning(paste("The function given as cpo.retrafo references a 'data' variable.",
        "Beware that the 'data' variable as given as an argument to the surrounding function",
        "will not be accessible when cpo.retrafo is called.",
        "If you still need to access this data, copy it to a variable with a different name.",
        "If this warning is a false positive and you assign the 'data' variable properly, you can avoid",
        "this warning by giving it a name different from 'data'.", sep = "\n"))
    }
    trafoenv$data = NULL
  } else {  # cpo$type == "object"
    if (!cpo$stateless && !"control" %in% ls(trafoenv)) {
      stopf("CPO %s cpo.trafo did not create a 'control' object. Use the .stateless flag on creation if you don't need a control object.", cpo$name)
    }
    state = if (cpo$stateless) NULL else trafoenv$control
  }

  # the properties of the output should only be the input properties + the ones we're adding
  allowed.properties = union(tin$properties, cpo$properties$properties.needed)
  tout = handleTrafoOutput(result, data, tin$tempdata, cpo$datasplit, allowed.properties, cpo$properties$properties.adding,
    cpo$bound == "targetbound", cpo$convertto, tin$subset.index, cpo$name)



  retrafo = if (build.retrafo) makeCPORetrafo(cpo, state, prev.retrafo, tin$shapeinfo, tout$shapeinfo) else prev.retrafo

  inverter = if (build.inverter && cpo$bound == "targetbound") makeCPOInverter(cpo, state, prev.inverter, data, tin$shapeinfo) else prev.inverter

  list(data = tout$outdata, retrafo = retrafo, inverter = inverter)
}

# call cpo$first, then cpo$second, and chain the retrafos.
#
# A CPO tree looks like this:
#
#                CPOPipeline
#               /[first]    \[second]
#     CPOPipeline           CPOPipeline
#     /[first]  \[second]   /[first]  \[second]
# CPOPrim1    CPOPrim2  CPOPrim3    CPOPrim4
#
# callCPO calls go as:
#
#    1 -------> 2 -------> 3 -------> 4
#
# Retrafos are a chained list, where slot 'prev.retrafo' points to the previous retrafo object:
#
# retr.1 <-- retr.2 <-- retr.3 <-- retr.4
#
callCPO.CPOPipeline = function(cpo, data, build.retrafo, prev.retrafo, build.inverter, prev.inverter) {
  checkAllParams(cpo$par.vals, cpo$par.set, cpo$name)
  first = cpo$first
  second = cpo$second
  first$par.vals = subsetParams(cpo$par.vals, first$par.set)
  second$par.vals = subsetParams(cpo$par.vals, second$par.set)
  intermediate = callCPO(first, data, build.retrafo, prev.retrafo, build.inverter, prev.inverter)
  callCPO(second, intermediate$data, build.retrafo, intermediate$retrafo, build.inverter, intermediate$inverter)
}

# RETRAFO main function
# - checks the inbound and outbound data is in the right format
# - checks the shape of input and output is as was before
# - data will be turned into the shape requested by the cpo
# - properties check (inbound, and outbound)
# - automatically subsets 'args' to the relevant ones for cpo
# - possibly calls next.retrafo
# - returns the resulting data

# receiver.properties are the properties of the next layer
applyCPORetrafoEx = function(retrafo, data, build.inverter, prev.inverter) {

  assertClass(retrafo, "CPORetrafo")
  cpo = retrafo$cpo

  if (!"retrafo" %in% retrafo$kind) {
    stop("Object %s is an inverter, not a retrafo.", cpo$bare.name)
  }

  if (!is.null(retrafo$prev.retrafo)) {
    assertSubset(retrafo$prev.retrafo$properties.needed, cpo$properties$properties)  # this is already tested during composition
    assertClass(retrafo$prev.retrafo, "CPORetrafo")
    upper.result = applyCPORetrafoEx(retrafo$prev.retrafo, data, build.inverter, prev.inverter)
    data = upper.result$data
    prev.inverter = upper.result$inverter
  }

  if (cpo$bound == "targetbound") {
    return(callCPO(cpo, data, FALSE, NULL, build.inverter, prev.inverter))
  }
  assert(cpo$bound == "databound")

  tin = prepareRetrafoInput(data, cpo$datasplit, cpo$properties$properties.data, retrafo$shapeinfo.input, cpo$bare.name)

  assertChoice(cpo$type, c("functional", "object"))
  if (cpo$type == "functional") {
    result = retrafo$state(tin$indata)
  } else {  # cpo$type == "object"
    args = getBareHyperPars(cpo)
    args$data = tin$indata
    if (!cpo$stateless) {
      args$control = retrafo$state
    }
    result = do.call(cpo$retrafo, args)
  }

  # the properties of the output should only be the input properties + the ones we're adding
  allowed.properties = union(tin$properties, cpo$properties$properties.needed)

  list(data = handleRetrafoOutput(result, data, tin$tempdata, cpo$datasplit, allowed.properties,
    cpo$properties$properties.adding, retrafo$shapeinfo.output, tin$subset.index, cpo$bare.name),
    inverter = prev.inverter)
}

# User-facing cpo retrafo application to a data object.
# does checks, removes retrafo / inverter attributes, and calls 'applyCPORetrafoEx'
#' @export
applyCPO.CPORetrafo = function(cpo, data) {
  retrafo = cpo
  build.inverter = hasTagInvert(data)
  prev.inverter = inverter(data)
  if (is.nullcpo(prev.inverter)) {
    prev.inverter = NULL
  }
  inverter(data) = NULL
  data = tagInvert(data, FALSE)
  if (!build.inverter && !is.null(prev.inverter)) {
    stop("Data had 'inverter' attribute set, but not the 'keep.inverter' tag.")
  }
  if (!is.null(prev.inverter)) {
    assertClass(prev.inverter, "CPORetrafo")
  }

  prev.retrafo = retrafo(data)
  if (is.nullcpo(prev.retrafo)) {
    prev.retrafo = NULL
  }
  retrafo(data) = NULL

  result = applyCPORetrafoEx(retrafo, data, build.inverter, prev.inverter)
  data = result$data
  retrafo(data) = prev.retrafo
  inverter(data) = result$inverter
  tagInvert(data, build.inverter)
}


##################################
### CPO Trafo Operations       ###
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

# CPO %>>% Learner

# attachCPO does four things:
#  1) Check that properties of CPOs and learners agree
#  2) Check that parameter names don't clash
#  3) possibly create a wrapper around the learner to get a 'CPOLearner' *
#  4) Modify CPO, Parameters, Properties of the learner
# * When attaching to a learner, we could, in principle, create a long chain of
# wrappers around a learner. This makes retrieving the operations, that go
# on in a learner with a long pipeline, relatively complicated.
# Therefore, if the learner is already a CPOLearner, we just change the
# attached CPO to a compound CPO.
#' @export
attachCPO.CPO = function(cpo, learner) {
  learner = checkLearner(learner)
  if (!learner$type %in% union(cpo$properties$properties.needed, setdiff(cpo$properties$properties, cpo$properties$properties.adding))) {
    stopf("Cannot combine CPO that outputs type %s with learner of type %s.",
      cpo$convertto, learner$type)
  }

  parameterClashAssert(cpo, learner, cpo$name, getLearnerName(learner))
  if (!"CPOLearner" %in% class(learner)) {
    learner = makeBaseWrapper(learner$id, learner$type, learner,
      learner.subclass = "CPOLearner", model.subclass = "CPOModel")
    learner$predict.type = learner$next.learner$predict.type
  } else {
    cpo = composeCPO(cpo, learner$cpo)
  }
  learner$cpo = cpo
  learner$properties = compositeCPOLearnerProps(cpo, learner$next.learner)
  learner$type = intersect(learner$properties, cpo.tasktypes)
  assertString(learner$type)
  learner$properties = setdiff(learner$properties, cpo.tasktypes)
  learner$par.vals = cpo$par.vals
  learner$par.set = cpo$par.set
  learner$id = paste(learner$id, cpo$bare.name, sep = ".")

  # possibly need to reset 'predict.type', or just change it to something else.
  prev.predict.type = learner$next.learner$predict.type
  next.predict.type = "response"
  if (prev.predict.type %in% c("response", getLearnerProperties(learner))) {  # if the previous predict.type is still supported
    next.predict.type = prev.predict.type
  }
  setPredictType(learner, next.predict.type)
}

# Get the properties that a learner must have when a CPO with given properties is attached.
# the learner has only one 'properties' slot, the CPO has more than one. Also, the CPOs
# don't concern all properties a learner may have, so absence of a property in a CPO doesn't
# necessarily mean that it needs to be removed from the learner.
compositeCPOLearnerProps = function(cpo, learner) {
  props = setdiff(getLearnerProperties(learner), "weights")
  props = union(props, getLearnerType(learner))
  # relevant: we only have an influence on these properties.
  relevant = c(cpo.dataproperties, cpo.targetproperties, cpo.tasktypes, "prob", "se")
  props.relevant = intersect(props, relevant)
  props.relevant = compositeProperties(cpo$properties,
    list(properties = props.relevant, properties.adding = character(0), properties.needed = character(0)),
    cpo$name, getLearnerName(learner))$properties  # checks for property problems automatically
  c(props.relevant, setdiff(props, relevant))
}

# wraps around callCPO and makeChainModel
#' @export
trainLearner.CPOLearner = function(.learner, .task, .subset = NULL, ...) {
  if (!is.null(.subset)) {
    .task = subsetTask(.task, .subset)
  }

  cpo = .learner$cpo
  cpo$par.vals = subsetParams(.learner$par.vals, cpo$par.set)

  # note that an inverter for a model makes no sense, since the inverter is crucially bound to
  # the data that is supposed to be *predicted*.
  retrafo(.task) = NULL
  inverter(.task) = NULL
  .task = tagInvert(.task, FALSE)
  transformed = callCPO(cpo, .task, TRUE, NULL, FALSE, NULL)

  model = makeChainModel(train(.learner$next.learner, transformed$data), "CPOWrappedModel")
  model$retrafo = transformed$retrafo
  model
}

# Wraps around applyCPORetrafoEx and invertCPO
#' @export
predictLearner.CPOLearner = function(.learner, .model, .newdata, ...) {
  retrafod = applyCPORetrafoEx(.model$learner.model$retrafo, .newdata, TRUE, NULL)
  prediction = NextMethod(.newdata = retrafod$data)
  if (!is.null(retrafod$inverter)) {
    invertCPO(retrafod$inverter, prediction, .learner$predict.type)$new.prediction
  } else {
    prediction
  }
}

# get CPO from learner
# Care needs to be taken that the learner's parameter values that concern the CPO are kept.
singleLearnerCPO.CPOLearner = function(learner) {
  cpo = learner$cpo
  cpo$par.vals = subsetParams(learner$par.vals, cpo$par.set)
  cpo
}

#' @export
# Target Bound CPOs have the possibility of mapping some predict.type values
# of a wrapped learner to different predict.type values of the base learner.
# Therefore we need to overload setPredictType.
setPredictType.CPOLearner = function(learner, predict.type) {
  assertChoice(predict.type, c("response", "prob", "se"))
  ptconvert = learner$cpo$predict.type
  supported.below = c(intersect(getLearnerProperties(learner$next.learner), c("prob", "se")), "response")
  supported.here = names(ptconvert)[ptconvert %in% supported.below]
  assertSetEqual(supported.here, c(intersect(getLearnerProperties(learner), c("prob", "se")), "response"))
  if (!predict.type %in% supported.here) {
    stopf("Trying to predict %s, but %s does not support that.", predict.type, learner$id)
  }
  learner$predict.type = predict.type
  learner$next.learner = setPredictType(learner$next.learner, ptconvert[predict.type])
  learner
}

# DATA %>>% CPO
# Basically wraps around callCPO with some checks and handling of attributes
#' @export
applyCPO.CPO = function(cpo, task) {
  if ("Task" %in% class(task) && !is.null(getTaskWeights(task))) {
    stop("CPO can not handle tasks with weights!")
  }
  build.inverter = hasTagInvert(task)
  prev.inverter = inverter(task)
  if (is.nullcpo(prev.inverter)) {
    prev.inverter = NULL
  }
  if (!build.inverter && !is.null(prev.inverter)) {
    stop("Data had 'inverter' attribute set, but not the 'keep.inverter' tag.")
  }
  if (!is.null(prev.inverter)) {
    assertClass(prev.inverter, "CPORetrafo")
  }
  prev.retrafo = retrafo(task)

  retrafo(task) = NULL
  inverter(task) = NULL
  task = tagInvert(task, FALSE)

  result = callCPO(cpo, task, TRUE, prev.retrafo, build.inverter, prev.inverter)
  task = result$data
  retrafo(task) = result$retrafo
  inverter(task) = result$inverter
  tagInvert(task, build.inverter)
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

# get par.vals with bare par.set names, i.e. the param names without the ID
getBareHyperPars = function(cpo) {
  assertClass(cpo, "CPOPrimitive")
  args = cpo$par.vals
  namestranslation = setNames(names2(cpo$bare.par.set$pars),
    names(cpo$par.set$pars))
  setNames(args, namestranslation[names(args)])
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

#' @export
getCPOBound.CPO = function(cpo) {
  cpo$bound
}

#' @export
getCPOKind.CPO = function(cpo) {
  "trafo"
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

##################################
### Retrafo Operations         ###
##################################

# get RETRAFO from mlr model
# possibly concatenate with another retrafo 'prev'
singleModelRetrafo.CPOModel = function(model, prev) {
  retrafo = model$learner.model$retrafo
  if (!is.null(prev)) {
    retrafo = composeCPO(prev, retrafo)
  }
  retrafo
}

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
    if (x$cpo$hybrid.inverter) {
      x$kind = c("retrafo", "inverter")
    }
  }
  class(x) = unique(c("CPORetrafoPrimitive", class(x)))
  c(prev, list(x))
}

# RETRAFO State
# The state is basically the control object, or the trafo-function's environment
# We also keep the shapeinfo.input and shapeinfo.output information
#' @export
getRetrafoState.CPORetrafoPrimitive = function(retrafo.object) {
  cpo = retrafo.object$cpo
  if (!"retrafo" %in% retrafo.object$kind) {
    stop("Cannot get state of inverter")
  }
  assertChoice(cpo$type, c("functional", "object"))
  if (cpo$type == "functional") {
    res = as.list(environment(retrafo.object$state))
    if (!"cpo.retrafo" %in% names(res)) {
      res$cpo.retrafo = retrafo.object$state
    } else if (!identical(res$cpo.retrafo, retrafo.object$state)) {
      stopf("Could not get coherent state of CPO Retrafo %s, since 'cpo.retrafo' in\n%s",
        cpo$name, "the environment of the retrafo function is not identical to the retrafo function.")
    }
  } else {  # cpo$type == "object
    res = getBareHyperPars(cpo)
    res$control = retrafo.object$state
  }
  # c() to drop the retrafo.object's class
  res$data = c(retrafo.object[c("shapeinfo.input", "shapeinfo.output")])  # nolint
  res
}

#' @export
# rebuilds a retrafo object from a given state. It does that by
# constructing a "bare" (empty) retrafo object and fills in the missing slots.
makeRetrafoFromState.CPOConstructor = function(constructor, state) {
  assertList(state, names = "unique")
  bare = constructor()

  data = state$data
  state$data = NULL
  assertSetEqual(names(data), c("shapeinfo.input", "shapeinfo.output"))

  assertChoice(bare$type, c("functional", "object"))
  if (bare$type == "functional") {
    assertSubset("cpo.retrafo", names(state))
    bare$par.vals = list()

    newstate = state$cpo.retrafo
    # update newstate's environment to actually contain the
    # values set in the 'state'
    env = new.env(parent = parent.env(environment(newstate)))
    list2env(state, envir = env)
    environment(newstate) = env
    # also set the 'cpo.retrafo' in the env to point to the current 'newstate' function.
    # if we did not do this, the 'cpo.retrafo' variable visible to newstate would
    # be the same function *but with a different environment* -- recursion would break
    # (this is because of 'environment(newstate) = env' above)
    env$cpo.retrafo = newstate
  } else {  # bare$type == "object
    assertSubset("control", names(state))
    newstate = state$control
    state$control = NULL
    bare$par.vals = state
    assertSubset(names(bare$par.vals), names(bare$bare.par.set$pars))
    if (length(state)) {
      names(bare$par.vals) = paste(bare$id, names2(bare$par.vals), sep = ".")
    }
  }

  makeCPORetrafo(bare, newstate, NULL, data$shapeinfo.input, data$shapeinfo.output)
}

# Param Sets

#' @export
getParamSet.CPORetrafoPrimitive = function(x) {
  x$cpo$par.set
}

#' @export
getHyperPars.CPORetrafoPrimitive = function(learner, for.fun = c("train", "predict", "both")) {
  learner$cpo$par.vals
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
getCPOBound.CPORetrafo = function(cpo) {
  cpo$bound
}

#' @export
getCPOKind.CPORetrafo = function(cpo) {
  cpo$kind
}

#' @export
getCPOPredictType.CPORetrafo = function(cpo) {
  names(cpo$predict.type)
}

#' @export
getCPOName.CPOConstructor = function(cpo) {
  environment(cpo)$.cpo.name
}

#' @export
getCPOId.CPOPrimitive = function(cpo) {
  cpo$id
}
