

getParamSetDefaults = function(ps) {
  lapply(ps$pars[vlapply(ps$pars, function(x) x$has.default)], function(x) x$default)
}

# check that ParamSets  ps1 and ps2 have distinct names; if not, give meaningful
# error message, referring to the objects by name1 and name2.
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
# check furthermore that this subset is complete,
# i.e. all parameters that have no unfulfilled requirements are there
subsetParams = function(par.vals, par.set) {
  par.vals[intersect(names(par.vals), names(par.set$pars))]
}

# check that all parameters are feasible according to their limits
# 'infeasible' parameters according to requirements are allowed
checkParamsFeasible = function(par.set, par.vals) {
  # names(par.vals) must be a subset of names(par.set$pars)
  oobreaction = firstNonNull(getMlrOption("on.par.out.of.bounds"), "stop")
  par.vals = par.vals = convertNamesToItemsDVP(par.vals, par.set)
  if (oobreaction != "quiet") {
    for (n in names(par.vals)) {
      if (!isFeasible(par.set$pars[[n]], par.vals[[n]])) {
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

# convert between character vectors and lists for discrete vector params
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
    if (!all(sapply(names(par$values), function(nn) identical(par$values[[nn]], nn)))) {
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

convertItemsToNamesDVP = function(par.vals, par.set) {
  for (n in names(par.set$pars)) {
    par = par.set$pars[[n]]
    if (par$type != "discretevector") {
      next
    }
    if (!all(sapply(names(par$values), function(nn) identical(par$values[[nn]], nn)))) {
      next
    }
    if (all(sapply(par.vals[[n]], is.character))) {
      par.vals[[n]] = as.character(par.vals[[n]])
    }
  }
  par.vals
}

# Manipulate requirement expressions: rename all variables from
# one name to another. This gets problematic when e.g. one variable
# is named 'c', because then c(1, 2, 3) gets broken. Therefore we
# go through the expressions and change only those requirements that
# are not function calls.
renameNonfunctionNames = function(expr, translate) {
  startfrom = 1
  if (is.call(expr)) {
    if (!is.recursive(expr)) {
      return(expr)
    }
    startfrom = 2
    if (is.recursive(expr[[1]])) {
      startfrom = 1
    } else if (length(expr) == 1) {
      return(expr)
    }
  }
  if (is.recursive(expr)) {
    for (idx in seq(startfrom, length(expr))) {
      expr[[idx]] = renameNonfunctionNames(expr[[idx]], translate)
    }
  } else if (is.symbol(expr) && as.character(expr) %in% names(translate)) {
    expr = as.symbol(translate[[as.character(expr)]])
  }
  return(expr)
}

