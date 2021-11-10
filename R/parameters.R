
# get the defaults of a param.set as a list
# @param ps [ParamSet] parameter set to query
# @return [list] named list of default parameter values
getParamSetDefaults = function(ps) {
  lapply(ps$pars[vlapply(ps$pars, function(x) x$has.default)], function(x) x$default)
}

# check that ParamSets  ps1 and ps2 have distinct names; if not, give meaningful
# error message, referring to the objects by name1 and name2.
# This is used when CPOs are composed, or when a CPO is attached to a learner.
# @param obj1 [CPO | Learner] an object for which `getParamSet` is implemented. This object's `ParamSet` is checked for name collision.
# @param obj2 [CPO | Learner] an object for which `getParamSet` is implemented. This object's `ParamSet` is checked for name collision.
# @param name1 [character(1)] the name of `obj1`, used in error messages.
# @param name2 [character(1)] the name of `obj2`, used in error messages.
# @return [invisible(NULL)]
parameterClashAssert = function(obj1, obj2, name1, name2) {
  ps1 = getParamSet(obj1)
  ps2 = getParamSet(obj2)
  samenames = intersect(names(ps1$pars), names(ps2$pars))
  if (length(samenames)) {
    plur = length(samenames) > 1
    stopf("Parameter%s %s occur%s in both %s and %s\n%s", ifelse(plur, "s", ""),
      paste0('"', samenames, '"', collapse = ", "), ifelse(plur, "", "s"), name1, name2,
      "Use the id parameter when constructing, or setCPOId, to prevent name collisions.")
  }
}

# get the subset of par.vals described by par set.
# @param par.vals [list] named list of parameter values
# @param par.set [ParamSet] parameter set to subset by
# @return [list] subset of `par.vals`
subsetParams = function(par.vals, par.set) {
  par.vals[intersect(names(par.vals), getParamIds(par.set))]
}

# check that all parameters are feasible according to their limits
# 'infeasible' parameters according to requirements are allowed.
# Behaves according to the mlr soption `on.par.out.of.bounds`:
# if it is "stop" it throws an error, if it is "quiet" it ignores it,
# otherwise it gives a warning.
# @param par.set [ParamSet] the parameter set to check by
# @param par.vals [list] named list of parameter values
# @return [invisible(NULL)]
checkParamsFeasible = function(par.set, par.vals) {
  # names(par.vals) must be a subset of names(par.set$pars)
  oobreaction = firstNonNull(getMlrOption("on.par.out.of.bounds"), "stop")
  par.vals = par.vals = convertNamesToItemsDVP(par.vals, par.set)
  if (oobreaction != "quiet") {
    for (n in names(par.vals)) {
      curpar = par.set$pars[[n]]
      pv = par.vals[[n]]
      # all.equal fails when check.environment is TRUE (default, unfortunately), but ParamHelpers doesn't know
      # Therefore we check for specials extra and give no specials to ParamHelpers::isFeasible().
      specials = curpar$special.vals
      curpar$special.vals = list()
      matches.special = any(vlapply(specials, function(s) isTRUE(all.equal(s, pv, check.environment = FALSE))))
      if (!matches.special && !isFeasible(curpar, pv)) {
        msg = sprintf("%s is not feasible for parameter '%s'!", convertToShortString(par.vals[[n]]), n)
        if (oobreaction == "stop") {
          stop(msg)
        } else {
          warning(msg)
        }
      }
    }
  }
}

# convert from character vectors to  lists for discrete vector params
#
# ParamHelpers has a "discrete vector" parameter, which accepts lists as values.
# However, it may be useful to accept a *vector* of character values instead of list
# values.
#
# For example, consider `makeDiscreteParam("test", list(a = c(1, 2, 3), b = c(4, 5, 6)))`. We may want
# to treat it as a parameter that accepts the values `"a"`, `"b"`, `c("a", "b")`, `character(0)`, etc.
#
# If `par.vals` is a named list of parameter values that, for a discrete vector param, contains the
# names of its possible values, this function will convert it to a named list of parameters that instead
# contains proper lists of values for discrete vector parameters.
#
# ("DVP" = Discrete Vector Param)
# @param par.vals [list] named list of parameters, containing character vector values for discrete vector parameters.
# @param par.set [ParamSet] the parameter set for which parameters are present.
# @return [list] named list of parameters, containing named list values for discrete vector parameters.
convertNamesToItemsDVP = function(par.vals, par.set) {
  oobreaction = firstNonNull(getMlrOption("on.par.out.of.bounds"), "stop")
  reactionFn = switch(oobreaction, stop = stopf, warn = warningf, quiet = list)  # nolint
  for (n in names(par.set$pars)) {
    if (!n %in% names(par.vals)) {
      next
    }
    par = par.set$pars[[n]]
    if (par$type != "discretevector") {
      next
    }
    if (!all(vlapply(names(par$values), function(nn) identical(par$values[[nn]], nn)))) {
      next
    }
    if (!is.character(par.vals[[n]])) {
      reactionFn("Discrete Vector Parameter %s requires character vector, not list, as values.", n)
    }

    badpars = setdiff(par.vals[[n]], names(par$values))
    if (length(badpars)) {
      reactionFn("Value%s '%s' do%s not occur in (names of) feasible values for parameter %s.",
        ifelse(length(badpars) > 1, "s", ""), collapse(badpars, sep = "', '"),
        ifelse(length(badpars) > 1, "es", ""), n)
      next
    }
    par.vals[[n]] = par$values[par.vals[[n]]]
  }
  par.vals
}

# convert back from list to character vector for discrete vector params
# This inverts `convertNamesToItemsDVP`, see documentation there.
# @param par.vals [list] named list of parameters, containing named list values for discrete vector parameters.
# @param par.set [ParamSet] the parameter set for which parameters are present.
# @return [list] named list of parameters, containing character vector values for discrete vector parameters.
convertItemsToNamesDVP = function(par.vals, par.set) {
  for (n in names(par.set$pars)) {
    par = par.set$pars[[n]]
    if (par$type != "discretevector") {
      next
    }
    if (!all(vlapply(names(par$values), function(nn) identical(par$values[[nn]], nn)))) {
      next
    }
    if (all(vlapply(par.vals[[n]], is.character))) {
      par.vals[[n]] = as.character(par.vals[[n]])
    }
  }
  par.vals
}
