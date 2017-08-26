

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
