# Callcpo.R: functions that are involved in calling CPOs and retrafos.
# They handle input checks, call the relevant CPO functions, and construct
# relevant Retrafo / Inverter objects.

#' @include CPOFormatCheck.R
##################################
### Creators                   ###
##################################

# Creates the "Inverter" S3 object. Both "Inverter" and "Retrafo"
# have the class "CPOTrained", with slight differences between them.
# Since a Retrafo can sometimes do the work of an Inverter, we can't
# use S3 to differentiate between them effectively.
makeCPOInverter = function(cpo, state, prev.inverter, data, shapeinfo) {
  if (!"Task" %in% class(data)) {
    data = makeClusterTask("CPO Generated", data, check.data = FALSE)
  }

  inverter = makeCPOTrainedBasic(cpo, state, "CPOInverter")
  # --- only in pure "inverter" kind
  inverter$indatatd = getTaskDesc(data)
  inverter$truth = prepareRetrafoData(data, cpo$datasplit, cpo$properties$properties, shapeinfo, cpo$name)$target
  composeCPO(nullToNullcpo(prev.inverter), inverter)
}

# Creates the "Retrafo" S3 object. See comment above 'makeCPOInverter'
makeCPORetrafo = function(cpo, state, prev.retrafo, shapeinfo.input, shapeinfo.output) {
  retrafo = makeCPOTrainedBasic(cpo, state, prev.retrafo, getCPORetrafoSubclasses(cpo))
  # --- only in "CPORetrafo"
  retrafo$properties.needed = cpo$properties$properties.needed  # is updated when chaining retrafos
  retrafo$shapeinfo.input = shapeinfo.input
  retrafo$shapeinfo.output = shapeinfo.output

  composeCPO(nullToNullcpo(prev.retrafo), retrafo)
}

# Creates an object of class "CPOTrained", which
# serves as basis for both "Inverter" and "Retrafo" objects.
makeCPOTrainedBasic = function(cpo, state, prev.retrafo, subclass) {
  retrafo = makeS3Obj(c("CPOTrainedPrimitive", subclass, "CPOTrained"),
    cpo = setCPOId(cpo, NULL),
    state = state,
    prev.retrafo = NULL,
    # --- Target Bound things
    predict.type = cpo$predict.type)  # named list type to predict --> needed type
}

##################################
### Primary Operations         ###
##################################
# CPO is a tree datastructure. CPOPrimitive are
# the leaves, CPOPipeline the nodes.
# CPOTrained is a linked list, which gets automatically
# constructed in 'callCPO'.

# Call the (possibly compound) CPO pipeline.
# Internal function; the user-facing functions (applyCPO) also make some checks
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
# - returns list(data, retrafo = [CPOTrained object])
# attaches prev.retrafo to the returned retrafo object, if present.
callCPO.CPOPrimitive = function(cpo, data, build.retrafo, prev.retrafo, build.inverter, prev.inverter) {

  assert(build.inverter, is.null(prev.inverter))

  checkAllParams(cpo$par.vals, cpo$par.set, cpo$debug.name)

  prev.retrafo = nullcpoToNull(prev.retrafo)
  prev.inverter = nullcpoToNull(prev.inverter)

  if (is.null(prev.retrafo)) {
    prevneeded = character(0)
  } else {
    prevneeded = prev.retrafo$properties.needed
    assertCharacter(prevneeded, unique = TRUE)
    if (isPropertyStrict()) {
      assertSubset(prevneeded, cpo$properties$properties)  # this should never happen, since we test this during CPO composition
    }
  }

  tin = prepareTrafoInput(data, cpo$datasplit, cpo$properties$properties.data, getCPOAffect(cpo, FALSE), cpo$fix.factors, cpo$debug.name)
  if (!cpo$data.dependent) {
    assert(cpo$operating.type = "target")
    tin$indata$data = NULL
  }
  if (is.null(cpo$retrafo)) {
    # stateless trafo-less CPO
    assert(cpo$control.type == "stateless")
    tin$indata$target = NULL
  }

  result = do.call(cpo$trafo, insert(getBareHyperPars(cpo), tin$indata))

  assertChoice(cpo$control.type, c("functional", "object", "stateless"))
  if (cpo$control.type != "stateless") {
    trafoenv = .ENV
  }
  if (cpo$control.type == "functional") {
    state = trafoenv$cpo.retrafo
    if (cpo$operating.type == "target") {
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
  } else if (cpo$control.type == "object") {
    if (!"control" %in% ls(trafoenv)) {
      stopf("CPO %s cpo.trafo did not create a 'control' object. Use the .stateless flag on creation if you don't need a control object.", cpo$debug.name)
    }
    state = trafoenv$control
  } else {  # cpo$control.type == "stateless"
    state = NULL
  }

  # the properties of the output should only be the input properties + the ones we're adding
  allowed.properties = union(tin$properties, cpo$properties$properties.needed)
  tout = handleTrafoOutput(result, if (cpo$operating.type != "traindata") data, tin$tempdata, cpo$datasplit, allowed.properties, cpo$properties$properties.adding,
    cpo$bound == "targetbound", cpo$convertto, tin$subset.index, cpo$debug.name)

  retrafo = if (build.retrafo && cpo$operating.type != "traindata") {
      makeCPORetrafo(cpo, state, prev.retrafo, tin$shapeinfo, tout$shapeinfo)
    } else {
      prev.retrafo
    }

  inverter = if (build.inverter && cpo$operating.type == "target") {
      makeCPOInverter(cpo, state, prev.inverter, data, tin$shapeinfo)
    } else {
      prev.inverter
    }

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
# This is the retrafo equivalent to callCPO. However, since CPOTrained
# is a different data structure than compound CPO ("CPOPipeline"), we don't need
# any S3 here.
callCPOTrained = function(retrafo, data, build.inverter, prev.inverter) {

  assertClass(retrafo, "CPOTrained")
  cpo = retrafo$cpo

  if (!"CPORetrafo" %in% class(retrafo)) {
    stop("Object %s is an inverter, not a retrafo.", cpo$name)
  }

  if (!is.null(retrafo$prev.retrafo)) {
    if (isPropertyStrict()) {
      assertSubset(retrafo$prev.retrafo$properties.needed, cpo$properties$properties)  # this is already tested during composition
    }
    upper.result = callCPOTrained(retrafo$prev.retrafo, data, build.inverter, prev.inverter)
    data = upper.result$data
    prev.inverter = upper.result$inverter
  }

  if (cpo$operating.type == "target") {
    return(callCPO(cpo, data, FALSE, NULL, build.inverter, prev.inverter))
  }
  assert(cpo$operating.type == "feature")  # "traindata" has no retrafo!

  tin = prepareRetrafoInput(data, cpo$datasplit, cpo$properties$properties.data, retrafo$shapeinfo.input, cpo$name)

  assertChoice(cpo$control.type, c("functional", "object", "stateless"))
  if (cpo$control.type == "functional") {
    result = retrafo$state(tin$indata)
  } else {  # cpo$control.type %in% c("stateless", "object")
    args = getBareHyperPars(cpo)
    args$data = tin$indata
    if (cpo$control.type == "object") {
      args$control = retrafo$state
      retrafo.fun = cpo$retrafo
    } else {
      retrafo.fun = cpo$trafo
    }
    result = do.call(retrafo.fun, args)
  }
  # the properties of the output should only be the input properties + the ones we're adding
  allowed.properties = union(tin$properties, cpo$properties$properties.needed)

  list(data = handleRetrafoOutput(result, data, tin$tempdata, cpo$datasplit, allowed.properties,
    cpo$properties$properties.adding, retrafo$shapeinfo.output, tin$subset.index, cpo$name),
    inverter = prev.inverter)
}

# Basically wraps around callCPO with some checks and handling of attributes
# This is also called for CPOTrained; there it calls 'callCPOTrained'
#' @export
applyCPO.CPO = function(cpo, task) {
  if ("Task" %in% class(task) && !is.null(getTaskWeights(task))) {
    stop("CPO can not handle tasks with weights!")
  }

  prev.inverter = nullcpoToNull(inverter(task))
  assert(checkNull(prev.inverter), checkClass(prev.inverter, "CPOInverter"))
  inverter(task) = NULL

  prev.retrafo = nullcpoToNull(retrafo(task))
  assert(checkNull(prev.inverter), checkClass(prev.inverter, "CPORetrafo"))
  retrafo(task) = NULL

  if ("CPOTrained" %in% class(cpo)) {
    result = callCPOTrained(cpo, task, TRUE, prev.inverter)
    task = result$data
    retrafo(task) = prev.retrafo
  } else {
    result = callCPO(cpo, task, TRUE, prev.retrafo, TRUE, prev.inverter)
    task = result$data
    retrafo(task) = result$retrafo
  }
  inverter(task) = result$inverter
  task
}

# User-facing cpo retrafo application to a data object.
#' @export
applyCPO.CPORetrafo = applyCPO.CPO

# get par.vals with bare par.set names, i.e. the param names without the ID
getBareHyperPars = function(cpo) {
  assertClass(cpo, "CPOPrimitive")
  args = cpo$par.vals
  namestranslation = setNames(names2(cpo$bare.par.set$pars),
    names(cpo$par.set$pars))
  setNames(args, namestranslation[names(args)])
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

# when a CPRetrafo is generated from a cpo, what (sub)class should it have?
# Normal (feature operating) CPOs are "CPORetrafo". Stateless
# ("hybrid.retrafo") target operating CPOs get to be "CPORetrafoHybrid" + "CPORetrafo";
# otherwise target operating CPOs are "CPORetrafoOnly" + "CPORetrafo".
getCPORetrafoSubclasses = function(cpo) {
  c(if (cpo$operating.type == "target") {
      if (cpo$control.type == "stateless") {
        "CPORetrafoHybrid"
      } else {
        "CPORetrafoOnly"
      }
    }, "CPORetrafo")
}
