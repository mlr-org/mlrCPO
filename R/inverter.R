
#' @title Invert Target Preprocessing
#'
#' @description
#' Invert the transformation, done on the target column(s)
#' of a data set, after prediction.
#'
#' Use either a retrafo object, or an inverter retrieved with
#' \code{\link{inverter}} from a data object that was fed through a retrafo
#' chain.
#'
#' If the retrafo object used had no target-bound transformations,
#' this is mostly a no-op, except that it possibly changes the task description
#' of the prediction.
#'
#' @param inverter [\code{CPOInverter}]\cr
#'   The retrafo or inverter to apply
#' @param prediction [\code{\link{Prediction}} | \code{matrix} | \code{data.frame}]\cr
#'   The prediction to invert
#' @return A transformed \code{\link{Prediction}} if a prediction was given,
#'   or a \code{data.frame}. The 'truth' column(s) of the prediction will be dropped.
#'
#' @export
invert = function(inverter, prediction, predict.type = "response") {
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
  prediction = sanitizePrediction(preddf)
  UseMethod("invert", inverter, prediction = prediction)
}

#' @export
invertCPO.CPO = function(inverter, prediction, predict.type) {
  stop("Cannot invert prediction with a CPO object; need a CPOTrained object.")
}

#' @export
invert.CPORetrafo = function(inverter, prediction, predict.type = "response") {
  message("(Inversion was a no-op.)")
  # we check this here and not earlier because the user might rely on inverter()
  # to check his data for consistency
  prediction
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
  if (have.prediction) {
    makePrediction(inverted$new.td, row.names = rownames(invdata), id = prediction$data$id,
      truth = inverted$new.truth, predict.type = predict.type, predict.threshold = NULL, y = invdata, time = prediction$time,
      error = prediction$error, dump = prediction$dump)
  } else {
    invdata
  }
}

#' @export
invert.CPORetrafoHybrid = invert.CPOInverter  # nolint

#' @export
invert.CPORetrafoOnly = function(inverter, prediction, predict.type = "response") {
  stopf("Inverting with CPO %s not possible, since it contains data-dependent inverters\nUse a CPOInverter object instead\n%s",
    getCPOName(inverter), "Retrieve a CPOInverter using the inverter() function.")
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



# INVERTER main function
# - errors out if not the right kind
# - does superficial test whether the input format is compatible with what to expect for the task type
# - applies the re-transformation
# - check result for plausibility
invertCPO.CPOInverter = function(inverter, prediction, predict.type) {
  assertString(predict.type)
  cpo = inverter$cpo
  if ("inverter" %in% inverter$kind) {
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

# type: the Task type that the prediction should conform to
# predict.type: what predict.type should prediction conform to?
# ultimate.predict.type: for output: what is the ultimate type we want?
validateSupposedPredictionFormat = function(prediction, type, predict.type, ultimate.predict.type, direction = c("input", "output"), inverter) {
  direction = match.arg(direction)
  name = inverter$cpo$name
  if (!type %in% inferPredictionTypePossibilities(prediction)) {
    # data format in 'prediction' is not compatible with what this CPO is supposed to have converted to
    stopf("Prediction %s of CPO Inverter %s is not compatible with supposed type %s",
      direction, name, type)
  }
  if (!predict.type %in% getPredResponseType(prediction, type)) {
    if (direction == "input") {
      stopf("To make a %s prediction, %s needs input of predict.type %s %s, but input seems incompatible with this.",
        ultimate.predict.type, getCPOName(inverter), type, predict.type)
    } else {
      stopf("Return of %s inverter did not conform with necessary predict.type %s.", name, predict.type)
    }
  }

  if (predict.type == "prob" && !is.matrix(prediction)) {
    assert(is.atomic(prediction))  # we should have filtered out data frames before this.
    if (type %in% c("classif", "cluster")) {  # if there is one cluster / one class, convert to a 1-D matrix
      prediction = matrix(prediction, ncol = 1)
    } else {
      stop("%s of inverter %s was not a matrix even though supposed prediction type is %s prob.", stri_trans_totitle(direction), name, type)
    }
  }
  prediction
}

