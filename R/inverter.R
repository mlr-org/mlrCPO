
#' @title Invert Target Preprocessing
#'
#' @description
#' Invert the transformation, done on the target column(s)
#' of a data set, after prediction.
#'
#' Use either a \code{\link{CPORetrafo}} object with invert capability (see \code{\link{getCPOTrainedCapability}},
#' or a \code{\link{CPOInverter} retrieved with
#' \code{\link{inverter}} from a data object that was fed through a retrafo
#' chain.
#'
#' If a \code{\link{CPORetrafo}} object is used that contains no target-bound transformations
#' (i.e. has \dQuote{invert} capability 0}, this is a no-op (except for dropping the \sQuote{truth} column).
#'
#' @param inverter [\code{CPOInverter}]\cr
#'   The retrafo or inverter to apply
#' @param prediction [\code{\link[mlr]{Prediction}} | \code{matrix} | \code{data.frame}]\cr
#'   The prediction to invert
#' @param predict.type [\code{character(1)}]\cr
#'   The equivalent to the \code{predict.type} property of a \code{\link[mlr:makeLearner]{Learner}}] object,
#'   control what kind of prediction to perform. One of \dQuote{response}, \dQuote{se},
#'   \dQuote{prob}. Default is \dQuote{response}. Care must be taken that the \code{prediction} was generated
#'   with a prediction type that fits this, i.e. it must be of type \code{getCPOPredictType(inverter)[predict.type]}.
#' @return [\code{\link[mlr]{Prediction}} | \code{data.frame}]. A transformed \code{\link{Prediction}} if a prediction was given,
#'   or a \code{data.frame}. If a \code{CPORetrafo} object is used, the \sQuote{truth} column(s) of the prediction will be dropped.
#'
#' @export
invert = function(inverter, prediction, predict.type = "response") {
  if ("Prediction" %in% class(prediction)) {
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
  UseMethod("invert")
}

#' @export
invert.CPO = function(inverter, prediction, predict.type = "response") {
  stop("Cannot invert prediction with a CPO object; need a CPOTrained object.")
}

#' @export
invert.CPOTrained = function(inverter, prediction, predict.type = "response") {
  cap = getCPOTrainedCapability(inverter)["invert"]
  assertSubset(cap, -1:1)
  if (cap == -1) {
    stopf("Inverting with CPORetrafo %s not possible, since it was created with data-dependent inverters\nUse a CPOInverter object instead\n%s",
      getCPOName(inverter), "Retrieve a CPOInverter using the inverter() function.")
  } else if (cap == 0) {
    message("(Inversion was a no-op.)")
    # we check this here and not earlier because the user might rely on inverter()
    # to check his data for consistency
    return(prediction)
  }
  assertChoice(inverter$convertto, cpo.tasktypes)
  prediction = sanitizePrediction(preddf, inverter$convertto, predict.type)

}

#' @export
invert.CPOInverter = function(inverter, prediction, predict.type = "response") {

  inverted = invertCPO(inverter, prediction, predict.type)
  invdata = inverted$new.prediction
  assert(all(grepl("^se$|^(prob|response)(\\..*)?$", names(invdata))))
  if (is.null(inverted$new.td)) {
#    TODO assert("retrafo" %in% getCPOKind(inverter))  # only hybrid retrafos should return a NULL td

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
      surv = makeSurvTaskDesc(tdname, data.frame(target1 = numeric(0), target2 = numeric(0)), c("target1", "target2"), NULL, NULL),
      # assuming rcens since nothing else ever gets used.
      stop("unknown outputtype"))
  }
  if ("Prediction" %in% class(prediction)) {
    makePrediction(inverted$new.td, row.names = rownames(invdata), id = prediction$data$id,
      truth = inverted$new.truth, predict.type = predict.type, predict.threshold = NULL, y = invdata, time = prediction$time,
      error = prediction$error, dump = prediction$dump)
  } else {
    invdata
  }
}

#' @title Check CPOInverter
#'
#' @description
#' Check whether the given object is a \code{CPOInverter} object.
#'
#' @param x [any]\cr
#'   The object to check.
#'
#' @return \code{TRUE} if \code{x} has class \code{CPOInverter}, \code{FALSE} otherwise.
#'
#' @export
is.inverter = function(x) {  # nolint
  "CPOInverter" %in% class(x)
}

# Invert the (learner supplied) prediction.
#
# This is used internally and takes the 'prediction' as generated by a Learner; it is whatever type the
# prediction usually has (depending on type).
#
# @param inverter [CPOInverter] the inverter
# @param prediction [any] prediction, as usually given by a Learner
# @param predict.type [character(1)] "response", "se", or "prob"
# @return [list] list(new.prediction, new.td, new.truth)
#   new.td & new.truth may be NULL if no target change occurred.
invertCPO = function(inverter, prediction, predict.type) {
  UseMethod("invertCPO")
}


# INVERTER main function
# - errors out if not the right kind
# - does superficial test whether the input format is compatible with what to expect for the task type
# - applies the re-transformation
# - check result for plausibility
invertCPO.CPOInverter = function(inverter, prediction, predict.type) {
  assertString(predict.type)
  cpo = inverter$cpo
  if ("invert" %in% inverter$kind) {
    # make sure some things that should always be true are actually true
    assertString(cpo$convertfrom)
    assertString(cpo$convertto)
    assert(!"retrafo" %in% inverter$kind || cpo$stateless)  # for data caching inverters, no hybrids are created
    assert(("retrafo" %in% inverter$kind) == is.null(inverter$inverter.indata))

    if (!predict.type %in% names(inverter$predict.type)) {
      stop("Inverter %s cannot convert to requested predict.type %s", getCPOName(inverter), predict.type)
    }
    input.predict.type = inverter$predict.type[predict.type]
    assertString(input.predict.type)

    output.predict.type = ifelse(is.null(inverter$prev.retrafo), predict.type, inverter$prev.retrafo$predict.type[predict.type])
    assertString(output.predict.type)
    assertSubset(output.predict.type, names(cpo$predict.type))
    assert(cpo$predict.type[output.predict.type] == input.predict.type)

    prediction = validateSupposedPredictionFormat(prediction, cpo$convertto, input.predict.type, predict.type, "input", inverter)
    args = list(target = prediction, predict.type = output.predict.type)
    assertChoice(cpo$control.type, c("functional", "object"))
    if (cpo$control.type == "functional") {
      result = do.call(cpo$state, args)
    } else {
      args = insert(args, cpo$par.vals)
      if (!cpo$stateless) {
        args$control = inverter$state
      }
      result = do.call(cpo$retrafo, args)
    }


    result = sanitizePrediction(result)
    result = validateSupposedPredictionFormat(result, cpo$convertfrom, output.predict.type, predict.type, "output", inverter)
  }
  if (is.null(inverter$prev.retrafo)) {
    return(list(new.prediction = prediction, new.td = inverter$indatatd, new.truth = inverter$truth))
  }
  invertCPO(inverter$prev.retrafo, prediction, predict.type)
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

# Put the prediction 'data' into a canonical format.
#
# This is used when a new prediction comes in from outside, i.e. when invertCPO
# is called. The return value of a Learner is always checked with
# 'checkPredictLearnerOutput'.
#
# the canonical data layout, after sanitizePrediction:
# regr response: numeric vector
# regr se: numeric 2-column matrix
# cluster response: integer vector
# cluster prob: numeric matrix.
# classif response: factor vector
# classif prob: numeric matrix > 1 column
# surv response: numeric vector
# surv prob: not currently supported
# multilabel response: logical matrix > 1 column
# multilabel prob: matrix > 1 column
#
# @param data [data.frame | matrix | atomic] the input data
# @param type [character(1)] one of 'regr', 'cluster', 'classif', 'surv', 'multilabel'
# @param predict.type [character(1)] one of 'response', 'prob', 'se'.
sanitizePrediction = function(data, type, predict.type) {
  assertChoice(type, cpo.tasktypes)
  assertChoice(predict.type, cpo.predict.types)
  if (is.data.frame(data)) {
    if (length(unique(sapply(data, function(x) class(x)[1]))) != 1) {
      stop("Prediction had columns of multiple modes.")
    }
    if (ncol(data) > 1) {
      if (type == "classif" && predict.type == "response") {
        # give special error message here, since otherwise the error message
        # we give further down is cryptic.
        stop("'classif' response prediction needs single factor, but got data.frame with multiple columns.")
      }
      data = as.matrix(data)
    } else {
      data = data[[1]]
    }
  }
  if (!is.matrix(data) && !is.factor(data) && !is.numeric(data) && !is.logical(data)) {
    stop("data was not in any valid prediction format")
  }
  if (predict.type == "prob") {
    if (type == "surv") {
      stop("'surv' prediction has no predict.type 'prob'")
    }
    if (!is.numeric(data)) {
      stop("predict.type 'prob' must have numeric data")
    }
    return(as.matrix(data))
  } else if (predict.type == "se") {
    if (type != "regr") {
      stopf("predict.type 'se' only valid for 'regr' prediction, but prediction was '%s'.", type)
    }
    if (!is.matrix(data) || !is.numeric(data) || !ncol(data) == 2) {
      stop("'regr' 'se' prediction must be a numeric matrix with two columns")
    }
    return(data)
  } else if (type == "multilabel") {
    # the only other matrix
    if (!is.logical(data)) {
      stop("'multilabel' response prediction must be logical")
    }
    return(as.matrix(data))
  }

  # if data is a matrix, we allow it if it is a 1-column matrix
  if (is.matrix(data)) {
    if (ncol(matrix) > 1) {
      stopf("'%s' response prediction must be a vector, but is matrix with %s columns.",
        type, ncol(matrix))
    }
    data = data[, 1, drop = TRUE]
  }

  if (type == "cluster") {
    if (!is.numeric(data) || any(as.integer(data) != data)) {
      stop("'cluster' response prediction must be an integer vector")
    }
    data = as.integer(data)
  } else if (type == "classif") {
    if (!is.factor(data)) {
      stop("'classif' response prediction must be a factor")
    }
  } else {
    # regr, surv
    if (!is.numeric(data)) {
      stopf("'%s' response prediction must be a numeric vector", type)
    }
  }
  # remove all other hairs that `data` may have
  c(data)  # nolint
}
