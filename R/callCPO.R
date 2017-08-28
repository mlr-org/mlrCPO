# callCPO.R: functions that are involved in calling CPOs and retrafos.
# They handle input checks, call the relevant CPO functions, and construct
# relevant Retrafo / Inverter objects.

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

  inverter = makeCPORetrafoBasic(cpo, state, prev.inverter, "CPOInverter")
  # --- only in pure "inverter" kind
  inverter$indatatd = getTaskDesc(data)
  inverter$truth = prepareRetrafoData(data, cpo$datasplit, cpo$properties$properties, shapeinfo, cpo$name)$target
  inverter
}

# Creates the "Retrafo" S3 object. See comment above 'makeCPOInverter'
makeCPOFeatureRetrafo = function(cpo, state, prev.retrafo, shapeinfo.input, shapeinfo.output) {
  retrafo = makeCPORetrafoBasic(cpo, state, prev.retrafo, c("CPOFeatureRetrafo", if (cpo$hybrid.retrafo) "CPOInverter"))
  # --- only in "CPOFeatureRetrafo"
  retrafo$shapeinfo.input = shapeinfo.input
  retrafo$shapeinfo.output = shapeinfo.output
  retrafo$properties.needed = cpo$properties$properties.needed
  retrafo
}

# Creates an object of class "CPORetrafo", which
# serves as basis for both "Inverter" and "Retrafo" objects.
makeCPORetrafoBasic = function(cpo, state, prev.retrafo, subclass) {
  retrafo = makeS3Obj(c("CPORetrafoPrimitive", subclass, "CPORetrafo"),
    cpo = setCPOId(cpo, NULL),
    state = state,
    prev.retrafo = NULL,
    # --- Target Bound things
    bound = cpo$bound,
    predict.type = cpo$predict.type)  # named list type to predict --> needed type
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

  checkAllParams(cpo$par.vals, cpo$par.set, cpo$debug.name)

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

  tin = prepareTrafoInput(data, cpo$datasplit, cpo$properties$properties.data, getCPOAffect(cpo, FALSE), cpo$fix.factors, cpo$debug.name)
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
  assertChoice(cpo$control.type, c("functional", "object"))
  if (cpo$control.type == "functional") {
    state = trafoenv$cpo.retrafo
    if (cpo$bound == "targetbound") {
      requiredargs = c("df.features", "predict.type")
      if (is.null(state) || !isTRUE(checkFunction(state, args = requiredargs, nargs = 2))) {
        stopf('.data.dependent targetbound CPO %s cpo.trafo must set a variable "cpo.retrafo"\n%s"%s".',
          cpo$debug.name, "to a function with two arguments ", collapse(requiredargs, sep = '", "'))
      }
    } else if (is.null(state) || !isTRUE(checkFunction(state, nargs = 1))) {
      stopf("CPO %s cpo.trafo did not set a variable 'cpo.retrafo' to a function with one argument.", cpo$debug.name)
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
  } else {  # cpo$control.type == "object"
    if (!cpo$stateless && !"control" %in% ls(trafoenv)) {
      stopf("CPO %s cpo.trafo did not create a 'control' object. Use the .stateless flag on creation if you don't need a control object.", cpo$debug.name)
    }
    state = if (cpo$stateless) NULL else trafoenv$control
  }

  # the properties of the output should only be the input properties + the ones we're adding
  allowed.properties = union(tin$properties, cpo$properties$properties.needed)
  tout = handleTrafoOutput(result, data, tin$tempdata, cpo$datasplit, allowed.properties, cpo$properties$properties.adding,
    cpo$bound == "targetbound", cpo$convertto, tin$subset.index, cpo$debug.name)

  retrafo = if (build.retrafo) makeCPOFeatureRetrafo(cpo, state, prev.retrafo, tin$shapeinfo, tout$shapeinfo) else prev.retrafo

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
  checkAllParams(cpo$par.vals, cpo$par.set, cpo$debug.name)
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
    stop("Object %s is an inverter, not a retrafo.", cpo$name)
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

  tin = prepareRetrafoInput(data, cpo$datasplit, cpo$properties$properties.data, retrafo$shapeinfo.input, cpo$name)

  assertChoice(cpo$control.type, c("functional", "object"))
  if (cpo$control.type == "functional") {
    result = retrafo$state(tin$indata)
  } else {  # cpo$control.type == "object"
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
    cpo$properties$properties.adding, retrafo$shapeinfo.output, tin$subset.index, cpo$name),
    inverter = prev.inverter)
}

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

# get par.vals with bare par.set names, i.e. the param names without the ID
getBareHyperPars = function(cpo) {
  assertClass(cpo, "CPOPrimitive")
  args = cpo$par.vals
  namestranslation = setNames(names2(cpo$bare.par.set$pars),
    names(cpo$par.set$pars))
  setNames(args, namestranslation[names(args)])
}


#' @export
getCPOPredictType.CPORetrafo = function(cpo) {
  names(cpo$predict.type)
}

checkAllParams = function(par.vals, par.set, name) {
  present = names(par.vals)

  # these parameters are either present or have fulfilled requirements
  needed = names(Filter(function(x) {
    x$id %in% names(par.vals) ||
          is.null(x$requires) || isTRUE(try(eval(x$requires, envir = par.vals), silent = TRUE))
  }, par.set$pars))

  missing.pars = setdiff(needed, present)
  if (length(missing.pars)) {
    plur = length(missing.pars) > 1
    stopf("Parameter%s %s of CPO %s %s missing\n%s", ifelse(plur, "s", ""),
      collapse(missing.pars, sep = ", "), name, ifelse(plur, "are", "is"),
      "Either give it during construction, or with setHyperPars.")
  }
}
