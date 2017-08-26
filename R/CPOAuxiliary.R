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
  if ("WrappedModel" %in% class(data)) {
    stop("Cannot add retrafo to an mlr model.")
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

#' @title Invert Target Preprocessing
#'
#' @description
#' Invert the transformation, done on the target column(s)
#' of a data set, after prediction.
#'
#' Use either a retrafo object, or an inverter retrieved with
#' \code{\link{inverter}} from a data object that was fed through a retrafo
#' chain with \code{\link{tagInvert}} set to \code{TRUE}.
#'
#' If the retrafo object used had no target-bound transformations,
#' this is mostly a no-op, except that it possibly changes the task description
#' of the prediction.
#'
#' @param inverter [\code{CPORetrafo}]\cr
#'   The retrafo or inverter to apply
#' @param prediction [\code{\link{Prediction}} | \code{matrix} | \code{data.frame}]\cr
#'   The prediction to invert
#' @return A transformed \code{\link{Prediction}} if a prediction was given,
#'   or a \code{data.frame}. The 'truth' column(s) of the prediction will be dropped.
#'
#' @export
invert = function(inverter, prediction, predict.type = "response") {
  assertClass(inverter, "CPORetrafo")

  have.prediction = "Prediction" %in% class(prediction)
  if (have.prediction) {
    preddf = prediction$data
    probs = grepl("^prob(\\..*)$", names(preddf))
    if (length(probs)) {
      preddf = preddf[probs]
    } else if ("se" %in% names(preddf)) {
      preddf = preddf[c("response", "se")]
    } else {
      preddf = preddf$response
    }
    assert(is.data.frame(preddf))
  } else {
    preddf = prediction
  }
  preddf = sanitizePrediction(preddf)

  if (is.nullcpo(inverter) || length(inverter$predict.type) > 2) {  # predict.type is the identity
    cat("(Inversion was a no-op.)\n")
    # we check this here and not earlier because the user might rely on inverter()
    # to check his data for consistency
    prediction
  }

  inverted = invertCPO(inverter, preddf, predict.type)
  invdata = inverted$new.prediction
  assert(all(grepl("^se$|^(prob|response)(\\..*)?$", names(invdata))))
  if (is.null(inverted$new.td)) {
    assert("retrafo" %in% getCPOKind(inverter))  # only hybrid retrafos should return a NULL td

    outputtype = intersect(getCPOProperties(inverter)$properties, cpo.tasktypes)
    assert(length(outputtype) == 1)  # hybrid retrafos should always have one, otherwise it is a bug.

    tdconstructor = get(sprintf("make%sTaskDesc", stri_trans_totitle(outputtype)), mode = "function")

    tdname = "[CPO CONSTRUCTED]"

    inverted$new.td = switch(outputtype,
      classif = {
        levels = ifelse(predict.type == "prob", colnames(invdata), levels(invdata))
        makeClassifTaskDesc(tdname, data.frame(target = factor(character(0), levels = levels)), "df.features", NULL, NULL, levels[1])
      },
      cluster = makeClusterTaskDesc(tdname, data.frame(), NULL, NULL),
      regr = makeRegrTaskDesc(tdname, data.frame(target = numeric(0)), "df.features", NULL, NULL),
      multilabel = makeMultilabelTaskDesc(tdname, as.data.frame(invdata)[integer(0), ], colnames(invdata), NULL, NULL),
      surv = makeSurvTaskDesc(tdname, data.frame(target1 = numeric(0), target2 = numeric(0)), c("target1", "target2"), NULL, NULL, "rcens"),
      # assuming rcens since nothing else ever gets used.
      stop("unknown outputtype"))
  }
  if (have.prediction) {
    makePrediction(inverted$new.td, row.names = rownames(invdata), id = prediction$data$id,
      truth = inverted$new.truth, predict.type = predict.type, predict.threshold = NULL, y = invdata, time = prediction$time,
      error = prediction$error, dump = prediction$dump)
  } else {
    invdata
  }
}

# data is either a data.frame or a matrix, and will be turned into
# a uniform format.
sanitizePrediction = function(data) {
  if (is.data.frame(data)) {
    if (length(unique(sapply(data, function(x) class(x)[1]))) != 1) {
      stop("Prediction had columns of multiple modes.")
    }
    if (ncol(data) > 1) {
      data = as.matrix(data)
    } else {
      data = data[[1]]
    }
  }
  if (is.matrix(data) && ncol(data) == 1) {
    data = data[, 1, drop = TRUE]
  }
  if (is.logical(data) && !is.matrix(data)) {
    data = matrix(data, ncol = 1)
  }
  if (!is.logical(data) && !is.numeric(data) && !is.factor(data)) {
    stop("Data did not conform to any possible prediction: Was not numeric, factorial, or logical")
  }
  data
}

inferPredictionTypePossibilities = function(data) {
  # the canonical data layout, after sanitizePrediction
  # regr response: numeric vector
  # regr se: numeric 2-column matrix
  # cluster response: integer vector
  # cluster prob: numeric matrix. This could also be a 1-D matrix but will be returned as numeric vector
  # classif response: logical vector
  # classif prob: numeric matrix > 1 column, except for oneclass possibly (numeric vector)
  # surv response: numeric vector
  # surv prob: assuming a numeric matrix > 1 column, but doesn't seem to currently exist
  # multiclass response: logical matrix > 1 column
  # multiclass prob: matrix > 1 column

  data = sanitizePrediction(data)
  if (is.matrix(data)) {
    if (mode(data) == "logical") {
      return("multilabel")
    }
    return(c("cluster", "classif", "multilabel", "surv", if (ncol(data) == 2) "regr"))
  }

  if (is.factor(data)) {
    "classif"
  } else if (!is.numeric(data)) {
    stop("Data did not conform to any possible prediction: Was not numeric or factorial")
  } else {
    areWhole = function(x, tol = .Machine$double.eps^0.25)  all(abs(x - round(x)) < tol)
    c(if (areWhole(data)) "cluster", "surv", "regr")
  }
}

# if 'typepossibilities' has one element, this will also return one element EXCEPT FOR CLASSIF, CLUSTER
getPredResponseType = function(data, typepossibilities) {
  assertSubset(typepossibilities, cpo.tasktypes, empty.ok = FALSE)
  errout = function() stopf("Data did not conform to any of the possible prediction types %s", collapse(typepossibilities))
  data = sanitizePrediction(data)
  if (is.matrix(data)) {
    if (mode(data) == "logical") {
      if (!"multilabel" %in% typepossibilities) errout()
      return("response")
    }
    if (ncol(data) == 2) {
      if (identical(typepossibilities, "regr")) {
        return("se")
      }
      return(c("se", "prob"))
    }
    if ("regr" %in% typepossibilities) errout()
    return("prob")
  }

  if (is.factor(data)) {
    if (!"classif" %in% typepossibilities) errout()
    return("response")
  }
  if (!numeric(data)) errout()
  areWhole = function(x, tol = .Machine$double.eps^0.25)  all(abs(x - round(x)) < tol)
  if (!areWhole(data) && !"surv" %in% typepossibilities && !"regr" %in% typepossibilities) errout()
  c("response", if (any(c("classif", "cluster") %in% typepossibilities)) "prob")
}

# 'prediction' is whatever type the prediction usually has (depending on type). must return
# a list (new.prediction, new.td, new.truth)
#
# new.td & new.truth may be NULL if no target change occurred.
invertCPO = function(inverter, prediction, predict.type) {
  UseMethod("invertCPO")
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
getRetrafoState.CPORetrafo = function(retrafo.object) {
  stop("Cannot get state of compound retrafo. Use as.list to get individual elements")
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
