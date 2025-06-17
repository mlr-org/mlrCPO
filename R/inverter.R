
#' @title Invert Target Preprocessing
#'
#' @description
#' Invert the transformation, done on the target column(s)
#' of a data set, after prediction.
#'
#' Use either a \code{\link{CPORetrafo}} object with invert capability (see \code{\link{getCPOTrainedCapability}},
#' or a \code{\link{CPOInverter}} retrieved with
#' \code{\link{inverter}} from a data object that was fed through a retrafo
#' chain.
#'
#' If a \code{\link{CPORetrafo}} object is used that contains no target-bound transformations
#' (i.e. has \dQuote{invert} capability 0), this is a no-op.
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
#' @return [\code{\link[mlr]{Prediction}} | \code{data.frame}]. A transformed \code{\link[mlr]{Prediction}} if a prediction was given,
#'   or a \code{data.frame}. If the first object in the chain is a \code{CPORetrafo} object, the \sQuote{truth} column(s) of the
#'   prediction will be dropped.
#'
#' @export
invert = function(inverter, prediction, predict.type = "response") {
  assertChoice(predict.type, cpo.predict.types)
  UseMethod("invert")
}

#' @export
predict.CPOTrained = function(object, data, predict.type = "response", ...) {
  assert(length(list(...)) == 0)
  invert(object, data, predict.type = predict.type)
}

#' @export
invert.CPO = function(inverter, prediction, predict.type = "response") {
  stop("Cannot invert prediction with a CPO object; need a CPOTrained object.")
}

#' @export
invert.CPOConstructor = function(inverter, prediction, predict.type = "response") {
  stop("Cannot invert prediction with a CPOConstructor object; need a CPOTrained object.")
}

#' @export
invert.CPOTrained = function(inverter, prediction, predict.type = "response") {
  cap = getCPOTrainedCapability(inverter)["invert"]
  assertSubset(cap, -1:1)
  if (cap == -1) {
    stopf("Inverting with CPORetrafo %s not possible, since it was created with data-dependent inverters\nUse a CPOInverter object instead\n%s",
      getCPOName(inverter), "Retrieve a CPOInverter using the inverter() function.")
  } else if (cap == 0) {
    # message("(Inversion was a no-op.)")
    # we check this here and not earlier because the user might rely on inverter()
    # to check his data for consistency
    return(prediction)
  }
  if (predict.type %nin% names(inverter$predict.type)) {
    stopf("CPOInverter %s cannot invert to get predict.type %s.",
      getCPOName(inverter), predict.type)
  }

  needed.predict.type = inverter$predict.type[predict.type]

  if ("Prediction" %in% class(prediction)) {
    preddf = prediction$data
    if (needed.predict.type == "response") {
      if (!any(grepl("^response", names(preddf)))) {
        stopf("Trying to predict %s, and need response prediction for that, but no response found in Prediction.",
          predict.type)
      }
      preddf = preddf[grep("^response", names(preddf))]
    } else if (needed.predict.type == "se") {
      if (!all(c("response", "se") %in% names(preddf))) {
        stopf("Trying to predict %s, and need response and se prediction for that, but columns 'response' and/or 'se' not found in Prediction.",
          predict.type)
      }
      preddf = preddf[c("response", "se")]
    } else {
      probs = grepl("^prob(\\..*)$", names(preddf))
      if (length(probs)) {
        preddf = preddf[probs]
      } else {
        # no 'prop.' found, but this apparently happens sometimes
        # with 1-column-prob predictions. See if we can rescue this.
        preddf = dropNamed(preddf, c("id", "response"))
        if (ncol(preddf) > 1) {
          stopf("Trying to predict %s, and need prob prediction for that, but neither found columns 'prob.*', nor a single column besides 'id' and 'response'.",
            predict.type)
        }
      }
    }
    assert(is.data.frame(preddf))
  } else {
    preddf = prediction
  }

  assert(identical(intersect(getCPOProperties(inverter)$handling, cpo.tasktypes), inverter$convertfrom))
  assertChoice(inverter$convertto, cpo.tasktypes)
  sanpred = sanitizePrediction(preddf, inverter$convertto, needed.predict.type)

  inverted = invertCPO(inverter$element, sanpred, predict.type)

  if ("Prediction" %nin% class(prediction)) {
    return(inverted$new.prediction)
  }

  invdata = inverted$new.prediction

  if (is.null(inverted$new.td)) {
    # last inverter was a CPORetrafo, so we don't have a TD

    tdname = "[CPO CONSTRUCTED]"

    inverted$new.td = switch(inverter$convertfrom,
      classif = {
        levels = if (predict.type == "prob") colnames(invdata) else levels(invdata)
        makeClassifTaskDesc(tdname, data.frame(target = factor(character(0), levels = levels)), "target", NULL, NULL,
          if (length(levels) == 2) levels[1] else NA, FALSE)
      },
      cluster = makeClusterTaskDesc(tdname, data.frame(), NULL, NULL, FALSE),
      regr = makeRegrTaskDesc(tdname, data.frame(target = numeric(0)), "target", NULL, NULL, FALSE),
      multilabel = makeMultilabelTaskDesc(tdname, as.data.frame(invdata)[integer(0), ], colnames(invdata), NULL, NULL, FALSE),
      surv = makeSurvTaskDesc(tdname, data.frame(target1 = numeric(0), target2 = numeric(0)), c("target1", "target2"),
        NULL, NULL, FALSE),
      stop("unknown outputtype"))
  }
  makePrediction(inverted$new.td, row.names = rownames(prediction$data), id = prediction$data$id,
    truth = inverted$new.truth, predict.type = predict.type, predict.threshold = NULL, y = invdata, time = prediction$time,
    error = prediction$error, dump = prediction$dump)
}

# Invert the (learner supplied) prediction.
#
# This is used internally and takes the 'prediction' as generated by a Learner; it is whatever type the
# prediction usually has (depending on type).
#
# INVERTER main function
# - errors out if not the right kind
# - does superficial test whether the input format is compatible with what to expect for the task type
# - applies the re-transformation
# - check result for plausibility
#
# @param inverter [InverterElement] the inverter
# @param prediction [any] prediction, as usually given by a Learner
# @param predict.type [character(1)] "response", "se", or "prob"
# @return [list] list(new.prediction, new.td, new.truth)
#   new.td & new.truth may be NULL if no target change occurred.
invertCPO = function(inverter, prediction, predict.type) {
  assertChoice(predict.type, cpo.predict.types)
  cpo = inverter$cpo
  assert(inverter$capability["invert"] %in% 0:1)
  if (inverter$capability["invert"] != 1) {
    assertClass(inverter, "RetrafoElement")
    if (is.null(inverter$prev.retrafo.elt)) {
      return(list(new.prediction = prediction, new.td = inverter$task.desc, new.truth = inverter$truth))
    }
    return(invertCPO(inverter$prev.retrafo.elt, prediction, predict.type))
  }

  assertString(cpo$convertfrom)
  assertString(cpo$convertto)

  if (!predict.type %in% names(inverter$prev.predict.type)) {
    stop("Inverters preceding %s cannot convert to requested predict.type %s", getCPOName(cpo), predict.type)
  }

  output.predict.type = unname(inverter$prev.predict.type[predict.type])
  assert(output.predict.type %in% names(cpo$predict.type))

  if (inherits(inverter, "InverterElement")) {
    state = inverter$state
  } else {
    assert("state.invert" %in% names(inverter),
      "cpo.trafo.orig" %in% names(cpo$trafo.funs) && is.null(cpo$trafo.funs$cpo.trafo.orig))
    state = inverter$state.invert
  }

  args = list(target = prediction, predict.type = output.predict.type, state = state)

  prediction = do.call(cpo$trafo.funs$cpo.invert, insert(getBareHyperPars(cpo), args))
  prediction = sanitizePrediction(prediction, cpo$convertfrom, output.predict.type, inverter$task.desc)

  if (is.null(inverter$prev.retrafo.elt)) {
    return(list(new.prediction = prediction, new.td = inverter$task.desc, new.truth = inverter$truth))
  }
  invertCPO(inverter$prev.retrafo.elt, prediction, predict.type)
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
# @param new.td [TaskDesc | NULL] TaskDesc of input task, if available; used for class levels only.
sanitizePrediction = function(data, type, predict.type, new.td = NULL) {
  assertChoice(type, cpo.tasktypes)
  assertChoice(predict.type, cpo.predict.types)
  if (is.data.frame(data)) {
    if (!length(data)) {
      stop("Prediction was empty.")
    }
    if (length(unique(vcapply(data, function(x) class(x)[1]))) != 1) {
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
    if (ncol(data) > 1) {
      stopf("'%s' response prediction must be a vector, but is matrix with %s columns.",
        type, ncol(data))
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
    if (!is.null(new.td)) {
      if (!testSubset(levels(data), new.td$class.levels)) {
        stop("'classif' response prediction must have factor levels as determined by input Task TaskDesc.")
      }
      data = factor(data, levels = new.td$class.levels)
    }
  } else {
    # regr, surv
    if (!is.numeric(data)) {
      stopf("'%s' response prediction must be a numeric vector", type)
    }
  }

  data
}
