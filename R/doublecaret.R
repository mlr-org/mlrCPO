# doublecaret.R -- the %>>% ("double caret") operator.

##################################
### %>>% Operator              ###
##################################

#' @title CPO Composition / Attachment operator
#'
#' @description
#' This operator \dQuote{pipes} data from the source into the target object.
#'
#' If both objects are a \link{CPO} object, they will be composed. A new object,
#' representing the operation of performing both object's operations in succession,
#' will be created, which can be handled like a new \link{CPO} object.
#'
#' If the left object is a \code{data.frame} or a \code{link{Task}}, the
#' transformation operation will be applied to this data, and the same resulting
#' data will be returned.
#'
#' If the right object is a \code{\link{Learner}}, the CPO will be attached to
#' this learner. The same operation will be performed during the \dQuote{train} and
#' \dQuote{predict} phase; the behaviour during the predict phase may furthermore
#' be depend on the training data.
#'
#' Note that you can not link a \code{data.frame} or \code{\link{Task}} directly
#' to a \code{\link{Learner}}, since this operation is not algebraically associative
#' with the composition of CPOs. Use \code{\link{train}} for this.
#'
#' @param cpo1 [\code{data.frame} | \code{\link{Task}} | \code{\link{CPO}}]\cr
#'   The source object.
#' @param cpo2 [\code{\link{CPO}} | \code{\link{Learner}}]\cr
#'   The target object.
#'
#' @family CPO
#' @examples
#' # PCA-rotate pid.task
#' rotated.pid.task = pid.task %>>% cpoPca()
#'
#' # Centering / Scaling *after* PCA
#' neoPCA = cpoPca(center = FALSE, scale = FALSE, id = "pca") %>>% cpoScale()
#'
#' # Attach the above to learner
#' pcaLogreg = neoPCA %>>% makeLearner("classif.logreg")
#'
#' @export
`%>>%` = function(cpo1, cpo2) {
  UseMethod("%>>%")
}

#' @title CPO Composition / Attachment operator
#'
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
  } else if (any(c("CPOConstructed", "CPO") %in% class(cpo2))) {
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
`%>>%.CPOConstructed` = function(cpo1, cpo2) {
  if (is.nullcpo(cpo2)) {
    cpo1
  } else if ("CPOConstructed" %in% class(cpo2)) {
    composeCPO(cpo1, cpo2)
  } else if ("WrappedModel" %in% class(cpo2)) {
    stop("Attaching CPO Retrafo to a model is not implemented.")
  } else if ("CPO" %in% class(cpo2) || "CPOConstructor" %in% class(cpo2)) {
    stop("Cannot compose CPO Retrafo with CPO.")
  } else {
    stopf("Cannot compose CPO Retrafo with object of class c(%s)", paste0('"', class(cpo2), '"', collapse = ", "))
  }
}
