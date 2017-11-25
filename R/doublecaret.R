# doublecaret.R -- the %>>% ("double caret") operator.

##################################
### %>>% Operator              ###
##################################

#' @title CPO Composition / Attachment / Application Operator.
#'
#' @description
#' This operator \dQuote{pipes} data from the source into the target object.
#'
#' If both objects are a \code{\link{CPO}} object, or both are a \code{\link{CPOTrained}} object,
#' they will be composed. A new object, representing the operation of performing both object's operations in succession,
#' will be created, which can be handled like a new CPO or CPOTrained object. See \code{\link{composeCPO}}.
#'
#' If the source object is a \code{\link[base]{data.frame}} or a \code{link[mlr]{Task}}, the
#' transformation operation will be applied to this data, and the same resulting
#' data will be returned. See \code{\link{applyCPO}}.
#'
#' If the target object is a \code{\link[mlr:makeLearner]{Learner}}, the CPO will be attached to
#' this learner. The same operation will be performed during the \code{\link[mlr]{train}} and
#' \code{\link[stats]{predict}} phase; the behaviour during the predict phase may furthermore
#' be depend on the training data. See \code{\link{attachCPO}}.
#'
#' Note that you can not link a \code{data.frame} or \code{\link[mlr]{Task}} directly
#' to a \code{\link[mlr:makeLearner]{Learner}}, since this operation is not algebraically associative
#' with the composition of CPOs. Use \code{\link[mlr]{train}} for this.
#'
#' The \code{\%<<\%} operator is synonymous with \code{\%>>\%} with source and target argument swapped.
#'
#' @param cpo1 [\code{\link[base]{data.frame}} | \code{\link[mlr]{Task}} | \code{\link{CPO}} | \code{\link{CPOTrained}}]\cr
#'   The source object.
#' @param cpo2 [\code{\link{CPO}} | \code{\link{CPOTrained}} | \code{\link[mlr:makeLearner]{Learner}}]\cr
#'   The target object.
#'
#' @family operators
#' @family retrafo related
#' @family inverter related
#' @family CPO lifecycle related
#'
#' @examples
#' # PCA-rotate pid.task
#' rotated.pid.task = pid.task %>>% cpoScale() %>>% cpoPca()
#'
#' # Centering / Scaling *after* PCA
#' newPCA = cpoPca() %>>% cpoScale()
#'
#' # Attach the above to learner
#' pcaLogreg = neoPCA %>>% makeLearner("classif.logreg")
#'
#' @export
`%>>%` = function(cpo1, cpo2) {
  UseMethod("%>>%")
}

#' @rdname grapes-greater-than-greater-than-grapes
#' @export
`%<<%` = function(cpo2, cpo1) {
  cpo1 %>>% cpo2
}

#' @export
`%>>%.default` = function(cpo1, cpo2) {
  stopf("%%>>%% not defined for objects of class c(%s)", paste0('"', class(cpo1), '"', collapse = ", "))
}

#' @export
`%>>%.data.frame` = function(cpo1, cpo2) {
  `%>>%.Task`(cpo1, cpo2)
}

#' @export
`%>>%.Task` = function(cpo1, cpo2) {
  if (is.nullcpo(cpo2)) {
    cpo1
  } else if ("Learner" %in% class(cpo2)) {
    stopf("%s\n%s\n%s\n%s",
      "Cannot pipe data into learner!",
      "If you called 'data %>>% preproc %>>% learner', you probably meant",
      "train(preproc %>>% learner, data). Note that this is different from",
      "'train(learner, data %>>% preproc), which is usually not what you want.")
  } else if (any(c("CPOTrained", "CPO") %in% class(cpo2))) {
    applyCPO(cpo2, cpo1)
  } else if ("CPOConstructor" %in% class(cpo2)) {
    stop("Cannot compose CPO Constructors.\nDid you forget to construct the CPO?")
  } else {
    stopf("Cannot compose data with object of class c(%s)", paste0('"', class(cpo2), '"', collapse = ", "))
  }
}

#' @export
`%>>%.CPOConstructor` = function(cpo1, cpo2) {
  stop("Cannot compose CPO Constructors.\nDid you forget to construct the CPO?")
}

#' @export
`%>>%.CPO` = function(cpo1, cpo2) {
  if (is.nullcpo(cpo2)) {
    cpo1
  } else if ("CPO" %in% class(cpo2)) {
    # compose two CPOs
    composeCPO(cpo1, cpo2)
  } else if ("Learner" %in% class(cpo2)) {
    # wrap around learner
    attachCPO(cpo1, cpo2)
  } else if ("CPOConstructor" %in% class(cpo2)) {
    stop("Cannot compose CPO Constructors.\nDid you forget to construct the CPO?")
  } else {
    stopf("Cannot compose CPO with object of class c(%s)", paste0('"', class(cpo2), '"', collapse = ", "))
  }
}

#' @export
`%>>%.CPOTrained` = function(cpo1, cpo2) {
  if (is.nullcpo(cpo2)) {
    cpo1
  } else if ("CPOTrained" %in% class(cpo2)) {
    composeCPO(cpo1, cpo2)
  } else if ("WrappedModel" %in% class(cpo2)) {
    stop("Attaching CPO Retrafo to a model is not implemented.")
  } else if ("CPO" %in% class(cpo2) || "CPOConstructor" %in% class(cpo2)) {
    stop("Cannot compose CPO Retrafo with CPO.")
  } else {
    stopf("Cannot compose CPO Retrafo with object of class c(%s)", paste0('"', class(cpo2), '"', collapse = ", "))
  }
}
