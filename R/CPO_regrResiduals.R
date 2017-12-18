

# a constructorconstructor for cpoRegrResiduals, wrapped using makeFauxCPOConstructor
# documentation below
makeCPORegrResiduals = function(learner, predict.se = FALSE) {

  checkFlag(predict.se)
  learner = checkLearner(learner, "regr", if (predict.se) "se")

  learner = setPredictType(learner, if (predict.se) "se" else "response")

  addnl.params = getParamSet(learner)
  addnl.params$pars = dropNamed(addnl.params, reserved.params)  # prevent clashes
  addnl.params$pars = lapply(addnl.params$pars, function(x) {
    # this is necessary so the CPO does not complain about unset hyperparameters.
    # TODO there should be a better way.
    x$requires = FALSE
    x
  })
  par.vals = getHyperPars(learner)
  par.vals = dropNamed(par.vals, reserved.params)

  makeCPOTargetOp("regr.residuals", addnl.params, par.vals,
    dataformat = "task",
    properties.data = intersect(cpo.dataproperties, getLearnerProperties(learner)),
    cpo.train = function(data, target, ...) {
      train(setHyperPars(learner, ...), data)
    },
    cpo.retrafo = {
      target[[1]] %-=% predict(control, newdata = data)$data$response
    },
    cpo.train.invert = {
      dropNamed(predict(control, newdata = data)$data, c("id", "truth"))
    },
    cpo.invert = {
      inlen = if (predict.type == "se") nrow(target) else length(target)
      if (inlen != nrow(control.invert)) {
        stopf("cpoRegrResiduals prediction to be inverted has different length from original task used for retrafo.")
      }
      if (predict.type == "response") {
        target + control.invert$response
      } else {
        target + as.matrix(control.invert[c("response", "se")])
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
#' @param learner [\code{character(1)} | \code{\link[mlr]{Learner}}]\cr
#'   A regression \code{\link[mlr]{Learner}}, or a \code{character(1)} identifying a
#'   \code{\link[mlr]{Learner}} to be constructed.
#' @param predict.se [\code{logical(1)}]\cr
#'   Whether to fit the model with \dQuote{se} predict type. This enables the resulting
#'   \code{\link{CPOInverter}} to be used for \code{property.type == "se"} inversion.
#'   Default is \code{FALSE}.
#' @export
cpoRegrResiduals = makeFauxCPOConstructor(makeCPORegrResiduals, "regr.residuals", "target")

registerCPO(cpoRegrResiduals(learner = "regr.lm"), "target", "residual fitting", "Replace a regression target by regression residuals.")
