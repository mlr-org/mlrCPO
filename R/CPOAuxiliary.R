#' @title CPO: Composable Preprocessing Operators
#'
#' @description
#' FIXME to come
#'
#' @family CPO
#' @name CPO
NULL

##################################
### Retrafo                    ###
##################################

#' @export
retrafo.default = function(data) {
  res = attr(data, "retrafo")
  if (!any(c("data.frame", "Task") %in% class(data))) {
    warningf("data is not a Task or data.frame.\n%s\n%s",
      "are you sure you are applying 'retrafo' to the result",
      "of a %>>% transformation?")
  } else if (is.null(res)) {
    res = NULLCPO
  }
  res
}

#' @export
retrafo.WrappedModel = function(data) {
  NULLCPO
}

#' @export
`retrafo<-.default` = function(data, value) {
  if (!is.null(value)) {
    assertClass(value, "CPORetrafo")
  }
  if (!any(c("data.frame", "Task") %in% class(data))) {
    warningf("argument is neither a Task nor data.frame.\n%s\n%s",
      "are you sure you are applying it to the input or",
      "result of a %>>% transformation?")
  }
  if (is.nullcpo(value)) {
    value = NULL
  }
  attr(data, "retrafo") = value
  data
}

#' @export
`retrafo<-.WrappedModel` = function(data, value) {
  stop("Cannot change retrafo of a model!")
}


##################################
### Inverter                   ###
##################################

#' @export
inverter.default = function(data) {
  res = attr(data, "inverter")
  if (!any(c("data.frame", "Task") %in% class(data))) {
    warningf("data is not a Task or data.frame.\n%s\n%s",
      "are you sure you are applying 'retrafo' to the result",
      "of a %>>% transformation?")
  } else if (is.null(res)) {
    res = NULLCPO
  }
  res
}


#' @export
`inverter<-.default` = function(data, value) {
  if (!is.null(value)) {
    assertClass(value, "CPORetrafo")
    assert("inverter" %in% getCPOKind(value))
  }
  if (!any(c("data.frame", "Task") %in% class(data))) {
    warningf("argument is neither a Task nor data.frame.\n%s\n%s",
      "are you sure you are applying it to the input or",
      "result of a %>>% transformation?")
  }
  if (is.nullcpo(value)) {
    value = NULL
  }
  attr(data, "inverter") = value
  data
}

#' @title Set the Inverter Tag
#'
#' @description
#' Tag the data that when it is sent through
#' a \code{\link{\%>>\%}} chain, the inverter
#' will be kept.
#'
#' @param data [\code{data.frame} | \code{\link{Task}}]\cr
#'   The data to tag
#'
#' @param set [\code{logical(1)}]\cr
#'   Whether to set the tag  or unset it. Default is TRUE.
#'
#' @family CPO
#' @export
tagInvert = function(data, set = TRUE) {
  if (!any(c("data.frame", "Task") %in% class(data))) {
    stop("data is not a Task or data.frame.")
  }
  assertFlag(set)
  if (set) {
    attr(data, "keep.inverter") = TRUE
  } else {
    attr(data, "keep.inverter") = NULL
  }
  data
}

#' @title Get the Inverter Tag
#'
#' @description
#' Check whether the data is tagged for inverter saving
#' when it is sent through a \code{\link{\%>>\%}} chain.
#'
#' @param data [\code{data.frame} | \code{\link{Task}}]\cr
#'   The result of a \code{\link{\%>>\%}} chain applied to a data set.
#'
#' @family CPO
#' @export
hasTagInvert = function(data) {
  if (!any(c("data.frame", "Task") %in% class(data))) {
    stop("data is not a Task or data.frame.")
  }
  identical(attr(data, "keep.inverter"), TRUE)
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
#' @param pplist [\code{list} of \code{CPO} | \code{list} of \code{CPORetrafo}]\cr
#'   A list of \code{CPO} or \code{CPORetrafo} objects.
#'
#' @family CPO
#' @export
pipeCPO = function(pplist) {
  assert(checkList(pplist, types = "CPO"),
    checkList(pplist, types = "CPORetrafo"))
  Reduce(`%>>%`, c(list(NULLCPO), pplist))
}

##################################
### General Generic Functions  ###
##################################

#' @export
setHyperPars2.CPORetrafo = function(learner, par.vals = list()) {
  stopf("Cannot change parameter values of retrafo object\n%s\n%s\n",
    "To create a retrafo with a specific state use makeRetrafoFromState.",
    "Get the state of an existing retrafo using getRetrafoState.")
}

#' @export
removeHyperPars.CPOLearner = function(learner, ids) {
  i = intersect(names(learner$par.vals), ids)
  if (length(i) > 0) {
    stopf("CPO Parameters (%s) can not be removed", collapse(i, sep = ", "))
  }
  learner$next.learner = removeHyperPars(learner$next.learner, ids)
  learner
}

#' @export
predict.CPORetrafo = function(object, data, ...) {
  assert(length(list(...)) == 0)
  applyCPO(object, data)
}

#' @export
getParamSet.CPORetrafo = function(x) {
  stop("Cannot get param set of compound retrafo. Use as.list to get individual elements")
}

#' @export
getHyperPars.CPORetrafo = function(learner, for.fun = c("train", "predict", "both")) {
  stop("Cannot get parameters of compound retrafo. Use as.list to get individual elements")
}

#' @export
setCPOId.default = function(cpo, id) {
  stop("setCPOId for object not defined.")
}

#' @export
getCPOName.CPORetrafo = function(cpo) {
  paste(sapply(as.list(cpo), getCPOName), collapse = " => ")
}

#' @export
invertCPO.CPO = function(inverter, prediction, predict.type) {
  stop("Cannot invert prediction with a CPO object; need a CPORetrafo object.")
}

##################################
### Auxiliaries                ###
##################################

# deparseJoin: deparse, but work with longer than 500 char expressions, mostly.
# Note that this is a heuristic for user messages only, the result can not be
# parsed again!
deparseJoin = function(what, sep = " ") {
  collapse(deparse(what, 500), sep = sep)
}

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

noMissingAssert = function(paramlist) {
  lapply(names(paramlist), function(x) {
    if (identical(paramlist[[x]], substitute())) {
      stopf("Parameter %s missing, with no default.", x)
    }
  })
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
  oobreaction = coalesce(getMlrOption("on.par.out.of.bounds"), "stop")
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
  oobreaction = coalesce(getMlrOption("on.par.out.of.bounds"), "stop")
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

# Search for references to variables (not function) named in 'pattern'
# return TRUE if any were found, FALE otherwise
referencesNonfunctionNames = function(expr, pattern) {
  startfrom = 1
  if (is.call(expr)) {
    if (!is.recursive(expr)) {
      return(FALSE)
    }
    startfrom = 2
    if (is.recursive(expr[[1]])) {
      startfrom = 1
    } else if (length(expr) == 1) {
      return(FALSE)
    }
  }
  if (is.recursive(expr)) {
    for (idx in seq(startfrom, length(expr))) {
      if (referencesNonfunctionNames(expr[[idx]], pattern)) {
        return(TRUE)
      }
    }
  } else if (is.symbol(expr) && as.character(expr) %in% pattern) {
    return(TRUE)
  }
  return(FALSE)
}

# create a function with expressions 'expr' in the environment 'env'.
# the function gets the argument list 'required.arglist.
# if 'expr' is actually a function, we just check that it has at least all the
# arguments in 'required.arglist' (or that is has ellipses), that all
# arguments have the same default values as required.arglist, and that
# arguments of the function that are not 'required' have a default value so
# there won't be an error when the function gets called later.
makeFunction = function(expr, required.arglist, env = parent.frame()) {
  if (is.recursive(expr) && identical(expr[[1]], quote(`{`))) {
    # we have a headless list of expressions
    # so we build our own function
    args = as.pairlist(required.arglist)
    newfun = eval(call("function", args, expr), envir = env)
  } else {
    newfun = eval(expr, envir = env)
    assertFunction(newfun)
    if (!"..." %in% names(formals(newfun))) {
      # with a vararg function we tentatively trust the function
      # handles its arguments

      assertFunction(newfun, args = names(required.arglist))
    }
    for (arg in names(formals(newfun))) {
      if (arg == "...") {
        # can't say anything about ellipses
        next
      }
      if (identical(formals(newfun)[[arg]], substitute())) {
        # the argument has no default, so we only care that this it will always be given
        if (!arg %in% names(required.arglist)) {
          stopf("Bad argument %s.\nNeed a function with arguments %s.", arg, collapse(names(required.arglist)), sep = ", ")
        }
        next
      }
      if (!arg %in% names(required.arglist)) {
        # the argument is not required, but it has a default; that is fine.
        next
      }
      if (!identical(formals(newfun)[[arg]], required.arglist[[arg]])) {
        if (identical(formals(newfun)[[arg]], substitute())) {
          funargstr = "no default"
          funarg = "no default"
        } else {
          funarg = eval(formals(newfun)[[arg]], envir = env)
          funargstr = sprintf("default %s", collapse(funarg))
        }
        if (identical(required.arglist[[arg]], substitute())) {
          reqarg = "no default"
        } else {
          reqarg = required.arglist[[arg]]
        }
        # there is a chance that they evaluate to the same value, so we check again
        if (!identical(funarg, reqarg) && (!length(funarg) == 1 || !length(reqarg) == 1 || !is.na(funarg) || !is.na(reqarg))) {
          stopf("Given function parameter %s has %s, but required default is %s.",
                arg, funargstr, collapse(reqarg))
        }
      }
    }
  }
  newfun
}

# capture the environment of the call to 'fun'
captureEnvWrapper = function(fun) {
  envcapture = quote({ assign(".ENV", environment(), envir = parent.frame()) ; 0 })
  envcapture[[3]] = body(fun)
  body(fun) = envcapture
  environment(fun) = new.env(parent = environment(fun))
  fun
}

requireCPOPackages = function(cpo) {
  requirePackages(cpo$packages, why = stri_paste("CPO", cpo$bare.name, sep = " "), default.method = "load")
}



# TO-DO:

# 'cat_P' class prevalence
# 'interaction'
# 'formula'
# QR
# spacialSign

# kernelPCA (kernlab) kpca
# ICA

# converting features to nums:
#  - 'as.numeric'
#  -

# ordered -> factor
# ordered -> as.numeric
# contr.poly, etc.
# knn impute


# binning -> to ordereds, to numbers

# cpoDummyEncode: referenzlevel waehlen ; 'ref.cat'
# cpoNAIndicators
# cpoRecodeNums (mit konstante, mit maximum, ...)
# cpoMergeSmallLevels
# listCPO() nicer print


# cpoDropConst

# rename attachCPO --> makeCPOWrapper
# unwrapCPOLearner


# properties.data nicht an den user geben

# cpo crossval

# sources:
#  https://topepo.github.io/recipes/reference/index.html
#  https://stats.idre.ucla.edu/r/library/r-library-contrast-coding-systems-for-categorical-variables/

#- exporting / fixing / hiding hyperparameters needs to handle requirements.
#- cpoCbind: prevent copies
#- check shapeinfo when reattaching retrafos
#- meta-CPOs: difficulties with 'properties.needed', 'properties.adding:
#  - properties.needed is the union of all properties.needed, properties and properties.adding are intersections
#  - optionally set to less strict properties: needed is intersection, properties and adding are unions; properties.needed must be ignored internally then.
#  - cpoApply must have all properties, all properties.adding, must ignore properties.needed

# test todo
# convertNamesToItems, ItemsToNames
# on.par.out.of.bounds setting
# task types checked

# news
# colApplyCPO
# cpoRangeScale
# impact encoding: level -> probability fuer jede klasse
# 'cat_B' ('bayesian' logit) x_catB = logit(P[y==target|x]) - logit(P[y==target]
