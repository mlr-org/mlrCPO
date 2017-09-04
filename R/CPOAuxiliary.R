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
  }

  nullToNullcpo(res)
}

#' @export
retrafo.WrappedModel = function(data) {
  NULLCPO
}

#' @export
`retrafo<-.default` = function(data, value) {
  if (!is.null(value)) {
    assert(is.retrafo(value))
  }
  if (!any(c("data.frame", "Task") %in% class(data))) {
    warningf("argument is neither a Task nor data.frame.\n%s\n%s",
      "are you sure you are applying it to the input or",
      "result of a %>>% transformation?")
  }
  nullcpoToNull(value)
  attr(data, "retrafo") = value
  data
}

#' @export
`retrafo<-.WrappedModel` = function(data, value) {
  stop("Cannot change retrafo of a model!")
}

#' @title Check CPORetrafo
#'
#' @description
#' Check whether the given object is a \code{CPORetrafo} object.
#'
#' @param x\cr
#'   The object to check.
#'
#' @return \code{TRUE} if \code{x} has class \code{CPORetrafo}, \code{FALSE} otherwise.
#'
#' @export
is.retrafo = function(x) {  # nolint
  "CPORetrafo" %in% class(x)
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
  }
  nullToNullcpo(res)
}

#' @export
inverter.WrappedModel = function(data) {
  stop("Cannot get inverter of a model!")
}

#' @export
`inverter<-.default` = function(data, value) {
  if (!is.null(value)) {
    assert(is.inverter(value))
  }
  if (!any(c("data.frame", "Task") %in% class(data))) {
    warningf("argument is neither a Task nor data.frame.\n%s\n%s",
      "are you sure you are applying it to the input or",
      "result of a %>>% transformation?")
  }
  value = nullcpoToNull(value)
  attr(data, "inverter") = value
  data
}

#' @export
`inverter<-.WrappedModel` = function(data, value) {
  stop("Cannot change inverter of a model!")
}

#' @title Check CPOInverter
#'
#' @description
#' Check whether the given object is a \code{CPOInverter} object.
#'
#' @param x\cr
#'   The object to check.
#'
#' @return \code{TRUE} if \code{x} has class \code{CPOInverter}, \code{FALSE} otherwise.
#'
#' @export
is.inverter = function(x) {  # nolint
  "CPOInverter" %in% class(x)
}

##################################
### General Generic Functions  ###
##################################

#' @export
setHyperPars2.CPOTrained = function(learner, par.vals = list()) {
  stopf("Cannot change parameter values of retrafo / inverter object\n%s\n%s\n",
    "To create a retrafo / inverter with a specific state use makeRetrafoFromState.",
    "Get the state of an existing retrafo / inverter using getRetrafoState.")
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
getParamSet.CPOTrained = function(x) {
  stop("Cannot get param set of compound retrafo. Use as.list to get individual elements")
}

#' @export
getHyperPars.CPOTrained = function(learner, for.fun = c("train", "predict", "both")) {
  stop("Cannot get parameters of compound retrafo. Use as.list to get individual elements")
}

#' @export
setCPOId.default = function(cpo, id) {
  stop("setCPOId for object not defined.")
}

#' @export
getCPOName.CPOTrained = function(cpo) {
  paste(sapply(as.list(cpo), getCPOName), collapse = " => ")
}

#' @export
invertCPO.CPO = function(inverter, prediction, predict.type) {
  stop("Cannot invert prediction with a CPO object; need a CPOTrained object.")
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

noMissingAssert = function(paramlist) {
  lapply(names(paramlist), function(x) {
    if (identical(paramlist[[x]], substitute())) {
      stopf("Parameter %s missing, with no default.", x)
    }
  })
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
  requirePackages(cpo$packages, why = stri_paste("CPO", cpo$name, sep = " "), default.method = "load")
}

# BBmisc::coalesce is dangerous, instead this sensible alternative is used
firstNonNull = function(...) {
  dots = match.call(expand.dots = FALSE)$...
  for (arg in dots) {
    val = eval.parent(arg)
    if (!is.null(val)) {
      return(val)
    }
  }
  NULL
}

# check global flag whether to strictly check property compliance
isPropertyStrict = function() {
  TRUE
}

getMlrOption = function(name, default = NULL) {
  getOption(stri_paste("mlr.", name), default)
}

getTaskWeights = function(task) {
  task$weights
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
# retrafoless
# simple makeCPO, makeCPORetrafoless, makeCPOTagetOp

# news
# colApplyCPO
# cpoRangeScale
# impact encoding: level -> probability fuer jede klasse
# 'cat_B' ('bayesian' logit) x_catB = logit(P[y==target|x]) - logit(P[y==target]
