# callInterface.R: wrapper functions for user-supplied cpo.trafo, cpo.retrafo etc. functions, that
# unify their interface for internal usage.
#
# The important functions are of the form makeXXXCallYYY, where XXX names the stage where the
# call is made (trafo, retrafo, invert) and YYY describes kind of CPO and may contain
# different amount of information (simple vs. extended, feature vs. target, functional vs. object);
# any of these may also be parameters of the function instead of part of the name.
#
# These functions should all return a function that
# - has parameters corresponding to the ones that are given in callCPO and callCPOTrained do.call lines:
#   (asterisk: only present in some cases)
#   - trafo: data, target, data.reduced* (i.e. task/df.all -> df.features), target.reduced*, build.inverter, hyperparameters
#   - retrafo: data (reduced), target*
#   - invert: predict.type, target
# - checks for presence and correctness of state objects
# - returns:
#   - trafo: list(result, state, state.invert)
#   - retrafo: list(result, state.invert)
#   - invert: result
#
# These functions are currently called in makeCPO.R:constructTrafoFunctions and then assigned to the
# cpo.trafo, cpo.retrafo etc. slots in a created CPO object. Then when the CPO is applied to data,
# callCPO and callCPOTrained call these.


# Retrafoless call
# Simple: nothing happens.
makeTrafoCallRetrafoless = function(cpo.trafo) {
  function(data.reduced, target.reduced, dataformat, build.inverter, ...) {
    list(result = cpo.trafo(...),
      state = NULL,
      state.invert = NULL)
  }
}

# feature operation CPO
makeTrafoCallFeatureOpSimple = function(cpo.trafo, cpo.retrafo) {
  if (is.null(cpo.retrafo)) {
    # functional, cannot be stateless
    function(data, target, data.reduced, target.reduced, build.inverter, ...) {
      cpo.retrafo = cpo.trafo(data = data, target = target, ...)
      checkFunctionReturn(cpo.retrafo, "data", "cpo.retrafo", "cpo.train")
      list(result = cpo.retrafo(data.reduced),
        state = cpo.retrafo,
        state.invert = NULL)
    }
  } else if (is.null(cpo.trafo)) {
    #stateless
    function(data, target, data.reduced, target.reduced, build.inverter, ...) {
      list(result = cpo.retrafo(data = data, ...),
        state = NULL,
        state.invert = NULL)
    }
  } else {
    # object based
    function(data, target, data.reduced, target.reduced, build.inverter, ...) {
      control = cpo.trafo(data = data, target = target, ...)
      list(result = cpo.retrafo(data = data.reduced, control = control, ...),
        state = control,
        state.invert = NULL)
    }
  }
}

makeTrafoCallFeatureOpExtended = function(cpo.trafo, cpo.retrafo) {
  cpo.trafo = captureEnvWrapper(cpo.trafo)
  function(data.reduced, target.reduced, build.inverter, ...) {
    .ENV = NULL
    result = cpo.trafo(...)
    if (is.null(cpo.retrafo)) {
      # functional
      state = getVarCreated(.ENV, "cpo.retrafo", "cpo.trafo")
      checkFunctionReturn(state, "data", "cpo.retrafo", "cpo.trafo")
    } else {
      # object based
      state = getVarCreated(.ENV, "control", "cpo.trafo")
    }
    list(result = result, state = state, state.invert = NULL)
  }
}

makeRetrafoCallFeatureOp = function(cpo.trafo, cpo.retrafo) {
  function(data, target, state, ...) {
    if (is.null(cpo.retrafo)) {
      # functional, state is cpo.retrafo
      list(result = state(data),
        state.invert = NULL)
    } else if (is.null(cpo.trafo)) {
      # stateless
      list(result = cpo.retrafo(data = data, ...),
        state.invert = NULL)
    } else {
      # object based
      list(result = cpo.retrafo(control = state, data = data, ...),
        state.invert = NULL)
    }
  }
}

makeTrafoCallTargetOpSimple = function(cpo.trafo, cpo.retrafo, cpo.train.invert, cpo.invert, dataformat, constant.invert) {
  inv.control.target.name = if (constant.invert) "cpo.invert" else "cpo.train.invert"
  inv.control.target = get(inv.control.target.name)
  if (is.null(cpo.retrafo) != is.null(inv.control.target)) {
    stopf("If cpo.retrafo is NULL, %s must also be NULL and vice versa if constant.invert is %s.",
      inv.control.target.name, constant.invert)
  }
  if (is.null(cpo.trafo)) {
    # stateless
    if (!constant.invert) {
      stop("constant.invert must be TRUE in stateless Target Operation CPO.")
    }
    function(data.reduced, target.reduced, data, target, build.inverter, ...) {
      if (dataformat %in% c("df.all", "task")) {
        # if dataformat is df.all or task we simulate the retrafo case:
        # data is data.reduced, target is data.
        target = data
      }
      list(result = cpo.retrafo(data = data.reduced, target = target, ...),
        state = NULL,
        state.invert = NULL)
    }
  } else {
    if (is.null(cpo.retrafo)) {
      # functional
      cpo.trafo = captureEnvWrapper(cpo.trafo)
    }
    function(data.reduced, target.reduced, data, target, build.inverter, ...) {
      target.reduced = if (dataformat %in% c("df.all", "task")) data else target
      if (is.null(cpo.retrafo)) {
        # functional
        .ENV = NULL
        cpo.trafo(data = data, target = target, ...)
        cpo.retrafo = getVarCreated(.ENV, "cpo.retrafo", "cpo.train")
        checkFunctionReturn(cpo.retrafo, c("data", "target"), "cpo.retrafo", "cpo.train")
        inv.control.target = getVarCreated(.ENV, inv.control.target.name, "cpo.train")
        if (constant.invert) {
          # returned function is cpo.invert
          req.args = c("target", "predict.type")
        } else {
          # returned function is cpo.train.invert
          req.args = "data"
        }
        checkFunctionReturn(inv.control.target, req.args, inv.control.target.name, "cpo.train")

        if (constant.invert) {
          state = cpo.retrafo
          state.invert = inv.control.target
        } else if (build.inverter) {
          state = list(cpo.retrafo = cpo.retrafo,
            cpo.train.invert = inv.control.target)
          state.invert = inv.control.target(data.reduced)
          if (is.null(cpo.invert)) {
            # functional invert
            checkFunctionReturn(state.invert, c("target", "predict.type"))
          }
        } else {
          state.invert = NULL
        }
        list(result = cpo.retrafo(data = data.reduced, target = target.reduced),
          state = state, state.invert = state.invert)
      } else {
        state = cpo.trafo(data = data, target = target, ...)
        if (constant.invert) {
          state.invert = state
        } else if (build.inverter) {
          state.invert = cpo.train.invert(data = data.reduced, control = state, ...)
          if (is.null(cpo.invert)) {
            # functional invert
            checkFunctionReturn(state.invert, c("target", "predict.type"))
          }
        } else {
          state.invert = NULL
        }
        list(result = cpo.retrafo(data = data.reduced, target = target.reduced, control = state, ...),
          state = state, state.invert = state.invert)
      }
    }
  }
}

makeRetrafoCallTargetOpSimple = function(cpo.trafo, cpo.retrafo, cpo.train.invert, cpo.invert, constant.invert) {
  if (is.null(cpo.trafo)) {
    # stateless
    function(state, data, target, ...) {
      list(result = if (!is.null(target)) cpo.retrafo(data = data, target = target, ...),
        state.invert = NULL)
    }
  } else {
    function(data, target, state, ...) {
      # re-apply trafo
      if (is.null(target)) {
        result = NULL  # no result to generate
      } else {
        if (is.null(cpo.retrafo)) {
          if (constant.invert) {
            cpo.retrafo = state
          } else {
            cpo.retrafo = state$cpo.retrafo
          }
          # functional
          result = cpo.retrafo(data = data, target = target)
        } else {
          # object based
          result = cpo.retrafo(data = data, target = target, control = state, ...)
        }
      }
      # train invert
      if (constant.invert) {
        state.invert = NULL
      } else {
        if (is.null(cpo.train.invert)) {
          # functional
          state.invert = state$cpo.train.invert(data)
        } else {
          state.invert = cpo.train.invert(data = data, state = state, ...)
        }
        if (is.null(cpo.invert)) {
          # functional invert
          checkFunctionReturn(state.invert, c("target", "predict.type"))
        }
      }
      list(result = result,
        state.invert = state.invert)
    }
  }
}

makeTrafoCallTargetOpExtended = function(cpo.trafo, cpo.retrafo, cpo.invert) {
  cpo.trafo = captureEnvWrapper(cpo.trafo)
  function(data, target, data.reduced, target.reduced, build.inverter, ...) {
    .ENV = NULL
    result = cpo.trafo(data = data, target = target, ...)
    if (is.null(cpo.retrafo)) {
      state = getVarCreated(.ENV, "cpo.retrafo", "cpo.trafo")
      checkFunctionReturn(state, c("data", "target"), "cpo.retrafo", "cpo.trafo")
    } else {
      state = getVarCreated(.ENV, "control", "cpo.trafo")
    }
    if (is.null(cpo.invert)) {
      state.invert = getVarCreated(.ENV, "cpo.invert", "cpo.trafo")
      checkFunctionReturn(state.invert, c("target", "predict.type"), "cpo.invert", "cpo.trafo")
    } else {
      state.invert = getVarCreated(.ENV, "control.invert", "cpo.trafo")
    }
    if (!build.inverter) {
      state.invert = NULL
    }
    list(result = result,
      state = state,
      state.invert = state.invert)
  }
}

makeRetrafoCallTargetOpExtended = function(cpo.trafo, cpo.retrafo, cpo.invert, constant.invert) {
  if (!constant.invert && !is.null(cpo.retrafo)) {
    cpo.retrafo = captureEnvWrapper(cpo.retrafo)
  }
  function(data, target, state, ...) {
    if (is.null(target) && constant.invert) {
      list(result = NULL, state.invert = NULL)
    } else {
      .ENV = NULL
      if (is.null(cpo.retrafo)) {
        if (!constant.invert) {
          state = captureEnvWrapper(state)
        }
        result = state(data = data, target = target, ...)
      } else {
        result = cpo.retrafo(data = data, target = target, control = state, ...)
      }
      if (!constant.invert) {
        # get inverter state
        if (is.null(cpo.invert)) {
          state.invert = getVarCreated(.ENV, "cpo.invert", "cpo.retrafo")
          checkFunctionReturn(state.invert, c("target", "predict.type"), "cpo.invert", "cpo.retrafo")
        } else {
          state.invert = getVarCreated(.ENV, "control.invert", "cpo.retrafo")
        }
      } else {
        state.invert = NULL
      }
      if (is.null(target)) {
        result = NULL
      }
      list(result = result, state.invert = state.invert)
    }
  }
}

makeInverterCall = function(cpo.trafo, cpo.invert) {
  function(target, predict.type, state, ...) {
    if (is.null(cpo.trafo)) {
      # stateless
      cpo.invert(target = target, predict.type = predict.type, ...)
    } else if (is.null(cpo.invert)) {
      # functional invert
      state(target = target, predict.type = predict.type)
    } else {
      # object based invert
      cpo.invert(target = target, predict.type = predict.type, control.invert = control.invert, ...)
    }
  }
}


# Check function generated by functional trafo
#
# Check that the function generated by user supplied function
# satisfies criteria, and give some warnings in some cases.
# These are: Check that number of parameters is as requested.
# If the number of parameters is > 1, check that the parameter
# names are as needed. Give a warning if 'data' or 'target'
# value is referenced inside function.
# @param fun [function] the function (as returned by the user) to check
# @param requiredargs [character] the arguments that the function must have
# @param fun.name [character(1)] the name of the function (cpo.trafo, cpo.retrafo etc) for message printing
# @param source.name [character(1)] the name where the function comes from, for message printing
# @return [invisible(NULL)]
checkFunctionReturn = function(fun, requiredargs, fun.name, source.name) {
  assert(length(requiredargs >= 1))
  if (is.null(fun)) {
    stopf("%s did not create a %s function.", source.name, fun.name)
  }
  if (length(requiredargs) > 1) {
    if (!isTRUE(checkFunction(fun, args = requiredargs, nargs = length(requiredargs)))) {
      stopf("%s as created by %s does not have (only) the required arguments %s",
        fun.name, source.name, collapse(requiredargs, sep = ", "))
    }
  } else {
    if (!isTRUE(checkFunction(fun, nargs = 1))) {
      stopf("%s as created by %s must have exactly one argument", fun.name, source.name)
    }
  }
  bad.references = Filter(function(varname) {
    varname %nin% names(formals(fun)) &&
      referencesNonfunctionNames(body(fun), varname)
  }, c("data", "target"))
  if (length(bad.references)) {
    warningf(paste("The function given as %s references a %s variable.",
      "Beware that the 'data' and 'target' variable as given as an argument to the surrounding function",
      "will not be accessible when %s is called.",
      "If you still need to access this data, copy it to a variable with a different name.",
      "If this warning is a false positive and you assign the 'data' variable properly, you can avoid",
      "this warning by renaming the 'data' variable.", sep = "\n"),
      fun.name, collapse(bad.references, sep = " and a "), fun.name)
  }
}

# Check whether function created a variable with the name, and get that variable
#
# Check that the function generated the requested variable name.
# This is used with a 'captureEnvWrapper' wrapped function,
# env should be the .ENV environment captured.
#
# This is env[[var]] with extra user friendly warnings
#
# @param env [environment] The environment to check
# @param var [character(1)] variable name
# @param source.name [character(1)] name of the function that created env
# @return [any]. env[[var]]
getVarCreated = function(env, var, source.name) {
  if (var %nin% env) {
    stopf("CPO's %s did not create a '%s' object.", source.name, var)
  }
  env[[var]]
}

# capture the environment of the call to 'fun'
#
# This puts a wrapper around a function so when it is called, the call environment
# will be assigned to a variable `.ENV`.
#
# Example:
# ```
# f = function(x) x
# fw = captureEnvWrapper(f)
# fw(10)
# ls(.ENV)  # .ENV variable was created, contains "x"
# .ENV$x    # == 10, since fw(10) assigned 10 to x
# @param fun [function] the function to wrap
# @return [function] a function that behaves the same, as `fun`, but also creates
#   the variable `.ENV` when called.
captureEnvWrapper = function(fun) {
  envcapture = quote({ assign(".ENV", environment(), envir = parent.frame()) ; 0 })
  envcapture[[3]] = body(fun)
  body(fun) = envcapture
  environment(fun) = new.env(parent = environment(fun))
  fun
}
