

#' @title Use the \dQuote{se} \code{predict.type} for \dQuote{response} Prediction
#'
#' @template cpo_doc_intro
#'
#' @description
#' Some \code{\link[mlr:makeLearner]{Learners}} may give better \dQuote{response} prediction
#' if their \dQuote{se} \code{predict.type} is used, especially when a \code{\link{cpoApplyFunRegrTarget}}is used
#' on it. This \code{\link{CPO}} performs no transformation of the data, but instructs
#' the underlying \code{\link[mlr:makeLearner]{Learner}} to do \dQuote{se} prediction
#' when \dQuote{response} prediction is requested (the default) and drops the \code{se}
#' column.
#'
#' @template cpo_doc_outro
#' @export
cpoResponseFromSE = makeCPOTargetOp("response.from.se",  # nolint
  properties.target = "regr",
  predict.type.map = c(response = "se", se = "se"),
  constant.invert = TRUE,
  cpo.train = NULL, cpo.train.invert = NULL,
  cpo.retrafo = { target },
  cpo.invert = {
    if (predict.type == "response") {
      target[, 1, drop = TRUE]
    } else {
      target
    }
  })

registerCPO(cpoResponseFromSE, "tools", "predict.type", "Use the 'se' predict.type for 'response' prediction.")
