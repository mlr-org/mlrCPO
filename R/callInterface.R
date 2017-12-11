# callInterface.R: wrapper functions for user-supplied cpo.trafo, cpo.retrafo etc. functions, that
# unify their interface for internal usage.
#
# The important functions are of the form makeCallYYY, where YYY names the kind of CPO:
# Retrafoless, FeatureOp[Simple], TargetOp[Simple]. These functions tend to call
# makeXXXCallYYY, where XXX names the stage where the
# call is made (trafo, retrafo, invert).
#
# These functions should all return a list(cpo.trafo, cpo.retrafo, cpo.invert) of function that
# - have parameters corresponding to the ones that are given in callCPO and callCPORetrafo do.call lines:
#   - trafo: data, target, data.reduced (i.e. task/df.all -> df.features), target.reduced, build.inverter, [hyperparameters]
#   - retrafo: data (reduced), target* (task/df.all this is the complete task / df instead of target), [hyperparameters]
#   - invert: predict.type, target, [hyperparameters]
# - check for presence and correctness of state objects
# - return:
#   - trafo: list(result, state, state.invert)
#   - retrafo: list(result, state.invert)
#   - invert: result
#
# These functions are currently called in makeCPO.R:constructTrafoFunctions and then assigned to the
# cpo.trafo, cpo.retrafo etc. slots in a created CPO object. Then when the CPO is applied to data,
# callCPO and callCPORetrafo call these.

##################################
### Retrafoless CPO            ###
##################################
# Simple: very little happens. Only cpo.trafo is defined which does not return a state or state.invert
makeCallRetrafoless = function(cpo.trafo, cpo.retrafo, cpo.train.invert, cpo.invert, dataformat, constant.invert) {
  trafo = function(data.reduced, target.reduced, build.inverter, ...) {
    list(result = cpo.trafo(...),
      state = NULL,
      state.invert = NULL)
  }
  list(cpo.trafo = trafo,
    cpo.retrafo = NULL,
    cpo.invert = NULL)
}

##################################
### Feature Operation CPO      ###
##################################

# Simple Feature Op CPO
makeCallFeatureOpSimple = function(cpo.trafo, cpo.retrafo, cpo.train.invert, cpo.invert, dataformat, constant.invert) {
  list(cpo.trafo = makeTrafoCallFeatureOpSimple(cpo.trafo, cpo.retrafo),
    cpo.retrafo = makeRetrafoCallFeatureOp(cpo.trafo, cpo.retrafo),
    cpo.invert = NULL)
}

# Extended Feature Op CPO
makeCallFeatureOpExtended = function(cpo.trafo, cpo.retrafo, cpo.train.invert, cpo.invert, dataformat, constant.invert) {
  list(cpo.trafo = makeTrafoCallFeatureOpExtended(cpo.trafo, cpo.retrafo),
    cpo.retrafo = makeRetrafoCallFeatureOp(cpo.trafo, cpo.retrafo),
    cpo.invert = NULL)
}

# for simple trafo functions, the 'control' object is first trained
# using cpo.trafo (what the user supplied as 'cpo.train'), this control
# object is then used to transform the data.
makeTrafoCallFeatureOpSimple = function(cpo.trafo, cpo.retrafo) {
  if (is.null(cpo.retrafo)) {
    cpo.trafo = captureEnvWrapper(cpo.trafo)
    # functional, cannot be stateless
    function(data, target, data.reduced, target.reduced, build.inverter, ...) {
      .ENV = NULL  # nolint
      cpo.retrafo = cpo.trafo(data = data, target = target, ...)
      checkFunctionReturn(cpo.retrafo, "data", "cpo.retrafo", "cpo.train")
      clearEnv(.ENV)
      list(result = cpo.retrafo(data.reduced),
        state = cpo.retrafo,
        state.invert = NULL)
    }
  } else if (is.null(cpo.trafo)) {
    #stateless
    function(data, target, data.reduced, target.reduced, build.inverter, ...) {
      list(result = cpo.retrafo(data = data.reduced, ...),
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

# for extended retrafo, the cpo.trafo functions transforms the data
# and creates a state object inside its environment.
# The name of the state object depends on whether the cpo is functional
# or object based.
makeTrafoCallFeatureOpExtended = function(cpo.trafo, cpo.retrafo) {
  cpo.trafo = captureEnvWrapper(cpo.trafo)
  function(data.reduced, target.reduced, build.inverter, ...) {
    .ENV = NULL  # nolint
    result = cpo.trafo(...)
    if (is.null(cpo.retrafo)) {
      # functional
      state = getVarCreated(.ENV, "cpo.retrafo", "cpo.trafo")
      checkFunctionReturn(state, "data", "cpo.retrafo", "cpo.trafo")
      clearEnv(.ENV)
    } else {
      # object based
      state = getVarCreated(.ENV, "control", "cpo.trafo")
    }
    list(result = result, state = state, state.invert = NULL)
  }
}

# retrafo is the same for simple and extended: the
# state object is used as, or for, the retrafo function.
makeRetrafoCallFeatureOp = function(cpo.trafo, cpo.retrafo) {
  if (is.null(cpo.retrafo)) {
    # functional, state is cpo.retrafo
    function(data, target, state, ...) {
      list(result = state(data),
        state.invert = NULL)
    }
  } else if (is.null(cpo.trafo)) {
    # stateless
    function(data, target, state, ...) {
      list(result = cpo.retrafo(data = data, ...),
        state.invert = NULL)
    }
  } else {
    # object based
    function(data, target, state, ...) {
      list(result = cpo.retrafo(control = state, data = data, ...),
        state.invert = NULL)
    }
  }
}

##################################
### Target Operation CPO       ###
##################################

# simple target operation cpo
makeCallTargetOpSimple = function(cpo.trafo, cpo.retrafo, cpo.train.invert, cpo.invert, dataformat, constant.invert) {
  list(cpo.trafo = makeTrafoCallTargetOpSimple(cpo.trafo, cpo.retrafo, cpo.train.invert, cpo.invert, dataformat, constant.invert),
    cpo.retrafo = makeRetrafoCallTargetOpSimple(cpo.trafo, cpo.retrafo, cpo.train.invert, cpo.invert, constant.invert),
    cpo.invert = makeInvertCall(cpo.trafo, cpo.invert))
}

makeCallTargetOpExtended = function(cpo.trafo, cpo.retrafo, cpo.train.invert, cpo.invert, dataformat, constant.invert) {
  list(cpo.trafo = makeTrafoCallTargetOpExtended(cpo.trafo, cpo.retrafo, cpo.invert),
    cpo.retrafo = makeRetrafoCallTargetOpExtended(cpo.trafo, cpo.retrafo, cpo.invert, constant.invert),
    cpo.invert = makeInvertCall(cpo.trafo, cpo.invert))
}

# extended target operation cpo

# "simple" target operation cpos are complicated and differ slightly between functional and object based
#
# (making things simpler for the user apparently translates into making the backend more complicated)
#
# For functional, if constant.invert is TRUE:
#   cpo.trafo (what the user supplied as 'cpo.train') creates the cpo.retrafo and cpo.invert
#   in its namespace. cpo.retrafo is used to create the result.
#   the 'state' is cpo.retrafo, the 'state.invert' is cpo.invert.
#
# For functional, if constant.invert is FALSE:
#   cpo.trafo (what the user supplied as 'cpo.train') creates the cpo.retrafo and cpo.train.invert
#   in its namespace. cpo.retrafo is used to create the result.
#   the 'state' is a list(cpo.retrafo, cpo.train.invert), the 'state.invert' is the result of cpo.train.invert().
#
# For object based:
#   cpo.trafo (what the user supplied as 'cpo.train') returns a control object. This is used
#   with cpo.retrafo to get the result. If constant.invert is TRUE, the 'state' and the
#   'state.invert' is the returned control object. If constant.invert is FALSE, the 'state'
#   is the control object, the 'state.invert' is the result of cpo.train.invert(..., control)
makeTrafoCallTargetOpSimple = function(cpo.trafo, cpo.retrafo, cpo.train.invert, cpo.invert, dataformat, constant.invert) {
  inv.control.target.name = if (constant.invert) "cpo.invert" else "cpo.train.invert"
  if (is.null(cpo.trafo)) {
    # stateless
    function(data, target, data.reduced, target.reduced, build.inverter, ...) {
      if (dataformat %in% c("df.all", "task")) {
        # if dataformat is df.all or task we simulate the retrafo case:
        # data is data.reduced, target is data.
        target = data
      }
      list(result = cpo.retrafo(data = data.reduced, target = target, ...),
        state = NULL,
        state.invert = NULL)
    }
  } else if (is.null(cpo.retrafo)) {
      # functional
    cpo.trafo = captureEnvWrapper(cpo.trafo)
    function(data, target, data.reduced, target.reduced, build.inverter, ...) {
      target.reduced = if (dataformat %in% c("df.all", "task")) data else target

      .ENV = NULL  # nolint
      cpo.trafo(data = data, target = target, ...)
      clearEnv(.ENV)

      cpo.retrafo = getVarCreated(.ENV, "cpo.retrafo", "cpo.train")
      checkFunctionReturn(cpo.retrafo, c("data", "target"), "cpo.retrafo", "cpo.train")

      inv.control.target = getVarCreated(.ENV, inv.control.target.name, "cpo.train")
      req.args = if (constant.invert) c("target", "predict.type") else "data"
      checkFunctionReturn(inv.control.target, req.args, inv.control.target.name, "cpo.train")

      if (constant.invert) {
        state = cpo.retrafo
        state.invert = if (build.inverter) inv.control.target
      } else {
        if (is.null(cpo.invert)) {
          # functional invert --> need to capture .ENV
          inv.control.target = captureEnvWrapper(inv.control.target)
          .ENV = NULL  # nolint
        }

        state = list(cpo.retrafo = cpo.retrafo,
          cpo.train.invert = inv.control.target)
        if (build.inverter) {
          state.invert = inv.control.target(data.reduced)
          if (is.null(cpo.invert)) {
            # functional invert
            clearEnv(.ENV)
            checkFunctionReturn(state.invert, c("target", "predict.type"), "cpo.invert", "cpo.train.invert")
          }
        } else {
          state.invert = NULL
        }
      }
      list(result = cpo.retrafo(data = data.reduced, target = target.reduced),
        state = state, state.invert = state.invert)
    }
  } else {
    # object based
    if (!constant.invert && is.null(cpo.invert)) {
      cpo.train.invert = captureEnvWrapper(cpo.train.invert)
    }
    function(data, target, data.reduced, target.reduced, build.inverter, ...) {
      target.reduced = if (dataformat %in% c("df.all", "task")) data else target

      state = cpo.trafo(data = data, target = target, ...)
      if (constant.invert) {
        state.invert = state
      } else if (build.inverter) {
        .ENV = NULL  # nolint
        state.invert = cpo.train.invert(data = data.reduced, control = state, ...)
        if (is.null(cpo.invert)) {
          # functional invert
          clearEnv(.ENV)
          checkFunctionReturn(state.invert, c("target", "predict.type"), "cpo.invert", "cpo.train.invert")
        }
      } else {
        state.invert = NULL
      }
      list(result = cpo.retrafo(data = data.reduced, target = target.reduced, control = state, ...),
        state = state, state.invert = state.invert)
    }
  }
}

# "simple" target operation retrafo:
#
# The cpo.retrafo is supposed to (1) perform another trafo of the target column,
# if present, and (2) create an inverter state.
#
# The cpo.retrafo is either the state or contained in it (in functional CPO) or it
# is called with control = state (in object based CPO). Similarly with cpo.train.invert.
makeRetrafoCallTargetOpSimple = function(cpo.trafo, cpo.retrafo, cpo.train.invert, cpo.invert, constant.invert) {
  if (is.null(cpo.trafo)) {
    # stateless
    function(data, target, state, ...) {
      list(result = if (!is.null(target)) cpo.retrafo(data = data, target = target, ...),
        state.invert = NULL)
    }
  } else {
    if (!constant.invert && !is.null(cpo.train.invert)) {
      cpo.train.invert = captureEnvWrapper(cpo.train.invert)
    }
    function(data, target, state, ...) {
      # re-apply trafo
      if (is.null(target)) {
        # no result to generate
        result = NULL
      } else if (is.null(cpo.retrafo)) {
        # functional
        if (constant.invert) {
          cpo.retrafo = state
        } else {
          cpo.retrafo = state$cpo.retrafo
        }
        result = cpo.retrafo(data = data, target = target)
      } else {
        # object based
        result = cpo.retrafo(data = data, target = target, control = state, ...)
      }
      # train invert
      if (constant.invert) {
        state.invert = NULL
      } else {
        .ENV = NULL  # nolint
        # if cpo.invert is functional, we need to delete the call's
        # 'target' and 'data' variables. If cpo.train.invert was given
        # functionally, it was wrapped in makeTrafoCallTargetOpSimple already.
        if (is.null(cpo.train.invert)) {
          # functional
          state.invert = state$cpo.train.invert(data)
        } else {
          # object based
          state.invert = cpo.train.invert(data = data, control = state, ...)
        }
        if (is.null(cpo.invert)) {
          # 'cpo.invert' can itself be functional here, independent
          # on whether cpo.train was functional or not.
          clearEnv(.ENV)
          checkFunctionReturn(state.invert, c("target", "predict.type"), "cpo.invert", "cpo.train.invert")
        }
      }
      list(result = result,
        state.invert = state.invert)
    }
  }
}

# extended target operation cpo
#
# This is much simpler than TargetOpSimple, since the 'functional'ity of
# retrafo and invert is orthogonal. For either, check whether the corresponding
# function is NULL, if so: It is functional, check the 'cpo.XXX' value, otherwise
# use the 'controlXXX' value.
makeTrafoCallTargetOpExtended = function(cpo.trafo, cpo.retrafo, cpo.invert) {
  cpo.trafo = captureEnvWrapper(cpo.trafo)
  function(data, target, data.reduced, target.reduced, build.inverter, ...) {
    .ENV = NULL  # nolint
    result = cpo.trafo(data = data, target = target, ...)
    clearEnv(.ENV)
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

# extended target operation cpo
#
# Simpler than TargetOpSimple, since the user handles the complexity.
# Note that if the invert is constant AND target is NULL, we don't need
# to call retrafo at all. Otherwise we call it and get the cpo.invert
# if indicated by build.inverter.
makeRetrafoCallTargetOpExtended = function(cpo.trafo, cpo.retrafo, cpo.invert, constant.invert) {
  if (!constant.invert && !is.null(cpo.retrafo)) {
    cpo.retrafo = captureEnvWrapper(cpo.retrafo)
  }
  function(data, target, state, ...) {
    if (is.null(target) && constant.invert) {
      return(list(result = NULL, state.invert = NULL))
    }
    .ENV = NULL  # nolint
    if (is.null(cpo.retrafo)) {
      if (!constant.invert) {
        state = captureEnvWrapper(state)
      }
      result = state(data = data, target = target)
    } else {
      result = cpo.retrafo(data = data, target = target, control = state, ...)
    }
    if (!constant.invert) {
      # get inverter state
      if (is.null(cpo.invert)) {
        clearEnv(.ENV)
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

# target operation invert
makeInvertCall = function(cpo.trafo, cpo.invert) {
  if (is.null(cpo.trafo)) {
    # stateless
    function(target, predict.type, state, ...) {
      cpo.invert(target = target, predict.type = predict.type, ...)
    }
  } else if (is.null(cpo.invert)) {
    # functional invert
    function(target, predict.type, state, ...) {
      state(target = target, predict.type = predict.type)
    }
  } else {
    # object based invert
    function(target, predict.type, state, ...) {
      cpo.invert(target = target, predict.type = predict.type, control.invert = state, ...)
    }
  }
}


# Check function generated by functional trafo, and modify its environment
#
# Check that the function generated by user supplied function
# satisfies criteria, and give some warnings in some cases.
# These are: Check that number of parameters is as requested.
# If the number of parameters is > 1, check that the parameter
# names are as needed. Give a warning if 'data' or 'target'
# value is referenced inside function.
#
# @param fun [function] the function (as returned by the user) to check
# @param requiredargs [character] the arguments that the function must have
# @param fun.name [character(1)] the name of the function (cpo.trafo, cpo.retrafo etc) for message printing
# @param source.name [character(1)] the name where the function comes from, for message printing
# @return [invisible(NULL)]
checkFunctionReturn = function(fun, requiredargs, fun.name, source.name) {
  assert(length(requiredargs) >= 1)
  if (is.null(fun)) {
    stopf("%s did not create a %s function.", source.name, fun.name)
  }
  if (!isTRUE(checkFunction(fun))) {
    stopf("%s created by %s must be a function", fun.name, source.name)
  }
  if (length(requiredargs) > 1) {
    if (!isTRUE(checkFunction(fun, args = requiredargs, nargs = length(requiredargs)))) {
      fargs = names(formals(fun))
      # allow fun to have dotdotdot instead of some of its arguments
      if ("..." %nin% fargs || length(setdiff(fargs, c(requiredargs, "...")))) {
        stopf("%s as created by %s does not have (only) the required arguments %s",
          fun.name, source.name, collapse(requiredargs, sep = ", "))
      }
    }
  } else {
    if (!isTRUE(checkFunction(fun, nargs = 1))) {
      fargs = names(formals(fun))
      if ("..." %nin% fargs || length(fargs) > 2) {
        stopf("%s as created by %s must have exactly one argument", fun.name, source.name)
      }
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

# Remove 'data' and 'target' variables from environment
#
# This saves memory when the environment stays around as the environment
# of functionally created cpo.* function
#
# @param env [environment] the environment
# @return [NULL]
clearEnv = function(env) {
  assertEnvironment(env)
  env$data = NULL
  env$target = NULL
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
  if (var %nin% names(env)) {
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
  envcapture = quote({ assign(".ENV", environment(), envir = parent.frame()) ; 0 })  # nocov
  envcapture[[3]] = body(fun)
  body(fun) = envcapture
  environment(fun) = new.env(parent = environment(fun))
  fun
}
