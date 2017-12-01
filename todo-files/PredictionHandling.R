# Some functions that were useful once, for handling predictions.



# Currently unused: Inver prediction type from data type
#
# @param data [any] the data, as returned by inverter / Learner
# @return [character]. Possible
inferPredictionTypePossibilities = function(data) {
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
    # Data did not conform to any possible prediction: Was not numeric or factorial
    character(0)
  } else {
    areWhole = function(x, tol = .Machine$double.eps^0.25)  all(abs(x - round(x)) < tol)
    c(if (areWhole(data) && all(data >= 0)) "cluster", "surv", "regr")
  }
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

