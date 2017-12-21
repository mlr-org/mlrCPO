

# a constructorconstructor for cpoRegrResiduals, wrapped using makeFauxCPOConstructor
# documentation below
makeCPORegrResiduals = function(learner, predict.se = FALSE, crr.train.residuals = "plain", crr.resampling = cv5) {

  assertFlag(predict.se)
  assertChoice(crr.train.residuals, c("resample", "oob", "plain"))
  if (crr.train.residuals == "resample") {
    assert(checkClass(crr.resampling, "ResampleInstance"),
      checkClass(crr.resampling, "ResampleDesc"))
  }
  learner = checkLearner(learner, "regr", c(if (predict.se) "se", if (crr.train.residuals == "oob") "oobpreds"))

  learner = setPredictType(learner, if (predict.se) "se" else "response")

  forbidden.pars = c(reserved.params, "crr.train.residuals", "crr.resampling")

  addnl.params = getParamSet(learner)
  addnl.params$pars = dropNamed(addnl.params$pars, forbidden.pars)  # prevent clashes
  addnl.params$pars = lapply(addnl.params$pars, function(x) {
    # this is necessary so the CPO does not complain about unset hyperparameters.
    # TODO there should be a better way.
    x$requires = FALSE
    x
  })
  addnl.params = c(pSSLrn(crr.train.residuals = "plain": discrete[list("resample", "oob", "plain")], crr.resampling: untyped),
    addnl.params)

  par.vals = getHyperPars(learner)
  par.vals = dropNamed(par.vals, forbidden.pars)
  par.vals = c(list(crr.train.residuals = crr.train.residuals, crr.resampling = crr.resampling), par.vals)

  # average out-of-resample-fold prediction / se
  # average se is calculated as root-mean-squared
  # rr [ResampleResult]
  # what [character(1)] "response" or "se"
  predMatFromRR = function(rr, what) {
    rows = rr$task.desc$size
    plist = getRRPredictionList(rr)
    sapply(plist$test, function(p) {
      out = rep(NA_real_, rows)
      out[p$data$id] = p$data[[what]]
      out
    })
  }

  # subtract the prediction from task to get to residuals
  # task [Task]
  # prediction.data [data.frame] the result of predict(...)$data
  taskSubtractPrediction = function(task, prediction.data) {
    tdata = getTaskData(task)
    tname = getTaskTargetNames(task)
    tdata[[tname]] %-=% prediction.data$response
    changeData(task, tdata)
  }

  control = NULL  # pacify static R code check
  data = NULL
  target = NULL
  predict.type = NULL

  makeCPOExtendedTargetOp("regr.residuals", addnl.params, par.vals,
    dataformat = "task",
    properties.data = intersect(cpo.dataproperties, getLearnerProperties(learner)),
    properties.target = "regr",
    predict.type.map = c(response = "response", se = if (predict.se) "se"),
    cpo.trafo = function(data, target, crr.train.residuals, crr.resampling, ...) {
      pars = list(...)  # avoid possible name clash through partial matching with par.vals parameter of setHyperPars
      control = train(setHyperPars(learner, par.vals = pars), data)
      control.invert = dropNamed(predict(control, data)$data, c("id", "truth"))

      if (crr.train.residuals == "oob") {
        if ("oobpreds" %nin% getLearnerProperties(learner)) {
          stop("for crr.resampling == 'oob' the Learner needs property 'oobpreds'.")
        }
        if (predict.se) {
          # since 'se' models don't support oobpreds
          # TODO: can go away when mlr-org/mlr#2116 is fixed
          model = train(setHyperPars(setPredictType(learner, "response"), par.vals = pars), data)
        } else {
          model = control
        }
        newresponse = getOOBPreds(model, data)$data$response
        control.invert$response[!is.na(newresponse)] = newresponse[!is.na(newresponse)]
      } else if (crr.train.residuals == "resample") {
        assert(checkClass(crr.resampling, "ResampleInstance"),
          checkClass(crr.resampling, "ResampleDesc"))
        if ("ResampleDesc" %in% class(crr.resampling)) {
          assertString(crr.resampling$predict)
          crr.resampling$predict = "test"
        } else {
          assertString(crr.resampling$desc$predict)
          crr.resampling$desc$predict = "test"
        }
        rr = resample(learner, data, crr.resampling, keep.pred = TRUE, show.info = FALSE)
        if (predict.se) {
          pmat = predMatFromRR(rr, "response")
          precmat = 1 / predMatFromRR(rr, "se")^2
          for (row in seq_along(control.invert$response)) {
            wmean = stats::weighted.mean(pmat[row, , drop = TRUE], precmat[row, , drop = TRUE], na.rm = TRUE)
            if (is.na(wmean)) {
              next
            }
            control.invert$response[row] = wmean
            control.invert$se[row] = 1 / sqrt(mean(precmat[row, , drop = TRUE], na.rm = TRUE))
          }
        } else {
          pmat = predMatFromRR(rr, "response")
          newresponse = apply(pmat, 1, mean, na.rm = TRUE)
          control.invert$response[!is.na(newresponse)] = newresponse[!is.na(newresponse)]
        }
      }
      taskSubtractPrediction(data, control.invert)
    },
    cpo.retrafo = {
      control.invert = dropNamed(predict(control, newdata = data)$data, c("id", "truth"))
      if (!is.null(target)) {
        taskSubtractPrediction(target, control.invert)
      }
    },
    cpo.invert = {
      inlen = if (predict.type == "se") nrow(target) else length(target)
      if (inlen != nrow(control.invert)) {
        stopf("cpoRegrResiduals prediction to be inverted has different length from original task used for retrafo.")
      }
      if (predict.type == "response") {
        target + control.invert$response
      } else {
        cbind(target[, 1, drop = TRUE] + control.invert$response, sqrt(target[, 2, drop = TRUE]^2 + control.invert$se^2))
      }
    })
}

#' @title Train a Model on a Task and Return the Residual Task
#'
#' @template cpo_doc_intro
#'
#' @description
#' Given a regression learner, this \code{\link{CPO}} fits the learner to a
#' regression \code{\link[mlr]{Task}} and replaces the regression target with
#' the residuals--the differences of the target values and the model's predictions--of the model.
#'
#' For inversion, the predictions of the model for the prediction data are
#' added to the predictions to be inverted.
#'
#' If \code{predict.se} is \code{TRUE}, \code{property.type == "se"} inversion can also
#' be performed. In that case, the \code{se} of the incoming prediction and the \code{se}
#' of the internal model are assumed to be independently distributed, and the resulting
#' \code{se} is the pythagorean sum of the \code{se}s.
#' @param learner [\code{character(1)} | \code{\link[mlr:makeLearner]{Learner}}]\cr
#'   A regression \code{\link[mlr:makeLearner]{Learner}}, or a \code{character(1)} identifying a
#'   \code{\link[mlr:makeLearner]{Learner}} to be constructed.
#' @param predict.se [\code{logical(1)}]\cr
#'   Whether to fit the model with \dQuote{se} predict type. This enables the resulting
#'   \code{\link{CPOInverter}} to be used for \code{property.type == "se"} inversion.
#'   Default is \code{FALSE}.
#' @param crr.train.residuals [\code{character(1)}]\cr
#'   What residuals to use for training (i.e. initial transformation). One of \dQuote{resample}, \dQuote{oob},
#'   \dQuote{plain}. If \dQuote{resample} is given, the out-of-resampling-fold predictions are used when resampling
#'   according to the \code{resampling} parameter. If \dQuote{oob} is used, the \code{\link[mlr:makeLearner]{Learner}} must
#'   have the \dQuote{oobpreds} property; the out-of-bag predictions are then used. If \code{train.residuals} is
#'   \dQuote{plain}, the simple regression residuals are used. \dQuote{plain} may offer slightly worse performance
#'   than the alternatives, but few \code{mlr} \code{\link[mlr:makeLearner]{Learners}} support \dQuote{oobpreds}, and
#'   \dQuote{resample} can come at a considerable run time penalty. Default is \dQuote{plain}.
#' @param crr.resampling [\code{\link[mlr:makeResampleDesc]{ResampleDesc}} | \code{\link[mlr:makeResampleInstance]{ResampleInstance}}]\cr
#'   What resampling to use when \code{train.residuals} is \dQuote{resample}; otherwise has no effect.
#'   The \code{$predict} slot of the resample description will be ignored and set to \code{test}.
#'   If a data point is predicted by multiple resampling folds, the average residual is used. If a data
#'   point is not predicted by any resampling fold, the \dQuote{plain} residual is used for that one.
#'   Default is \code{cv5}.
#' @section CPOTrained State:
#' The \code{CPORetrafo} state's \code{$control} slot is the \code{\link[mlr:makeWrappedModel]{WrappedModel}}
#' created when training the \code{learner} on the given data.
#'
#' The \code{CPOInverter} state's \code{$control} slot is a \code{data.frame} of the \dQuote{response} and
#' (if \code{predict.se} is \code{TRUE}) \dQuote{se} columns of the prediction done by the model on the data.
#'
#' @template cpo_doc_outro
#' @export
cpoRegrResiduals = makeFauxCPOConstructor(makeCPORegrResiduals, "regr.residuals", "target")  # nolint

registerCPO(cpoRegrResiduals(learner = "regr.lm"), "target", "residual fitting", "Replace a regression target by regression residuals.")
