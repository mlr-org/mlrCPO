# NULLCPO.R -- all the operations provided by and for the NULLCPO object.
# These are mostly generics that provide the proper behaviour of NULLCPO.
# If a generic concerns itself mostly with NULLCPO, it should probably
# go here, and *not* in other files that it might also touch (e.g. printing).

#' @title CPO Composition Neutral Element.
#'
#' @description
#' \code{NULLCPO} is the neutral element of \code{\link{CPO}} and \code{\link{CPOTrained}} composition when using
#' \code{\link{\%>>\%}} or \code{\link{composeCPO}}. It is furthermore no effect when attached to a \code{\link[mlr:makeLearner]{Learner}}
#' using \code{\link{attachCPO}} (or \code{\link{\%>>\%}}), or when applied to data using \code{\link{applyCPO}}, \code{\link{invert}},
#' or \code{\link[stats]{predict}} (or, again, \code{\link{\%>>\%}}).
#'
#' \code{NULLCPO} works as a stand-in for certain operations that have an "empty" return value:
#' It is returned when \code{\link{retrafo}} and \code{\link{inverter}} are applied to an object that has no retrafo or inverter
#' associated with it, and by \code{\link{pipeCPO}} when applied to an empty list.
#'
#' \code{NULLCPO} can be checked using \code{\link{is.nullcpo}}, and converted from or to \code{NULL} using \code{\link{nullToNullcpo}} and
#' \code{\link{nullcpoToNull}}. Otherwise it behaves very similarly to other \code{\link{CPO}} or \code{\link{CPOTrained}} objects.
#'
#' @family retrafo related
#' @family inverter related
#' @family CPO lifecycle related
#' @family NULLCPO related
#' @export
NULLCPO = makeS3Obj(c("NULLCPO", "CPOPrimitive", "CPORetrafo", "CPOInverter", "CPOTrained", "CPO"))  # nolint


#' @title Check for NULLCPO.
#'
#' @description
#' Check whether the given object is a \code{\link{NULLCPO}}.
#'
#' @param x [any]\cr
#'   The object to check
#' @return [\code{logical(1)}]. \code{TRUE} if \code{x} is a \code{NULLCPO}, \code{FALSE} otherwise.
#' @family NULLCPO related
#' @export
is.nullcpo = function(x) {  # nolint
  "NULLCPO" %in% class(x)
}


#' @title NULLCPO to NULL.
#'
#' @description
#' Convert \code{\link{NULLCPO}} to \code{NULL}, leave other values intact.
#'
#' @template arg_cpo
#' @return [\code{\link{CPO}} | \code{NULL}]. \code{NULL} if \code{cpo} is \code{NULLCPO}, \code{cpo} otherwise.
#' @family NULLCPO related
#' @export
nullcpoToNull = function(cpo) {
  if (is.nullcpo(cpo)) {
    NULL
  } else {
    cpo
  }
}


#' @title NULL to NULLCPO.
#'
#' @description
#' Convert \code{NULL} to \code{\link{NULLCPO}}, leave other values intact.
#'
#' @param cpo [\code{\link{CPO}} | \code{NULL}]\cr
#'   The CPO.
#' @return [\code{\link{CPO}}]. \code{\link{NULLCPO}} if \code{cpo} is \code{NULL}, \code{cpo} otherwise.
#' @family NULLCPO related
#' @export
nullToNullcpo = function(cpo) {
  if (is.null(cpo)) {
    NULLCPO
  } else {
    cpo
  }
}


#' @export
`%>>%.NULLCPO` = function(cpo1, cpo2) {
  if (any(c("Learner", "CPO", "CPOTrained") %in% class(cpo2))) {
    cpo2
  } else {
    NextMethod()
  }
}

#' @export
predict.NULLCPO = function(object, data, ...) {
  assert(length(list(...)) == 0)
  data
}

#' @export
getCPOTrainedState.NULLCPO = function(retrafo.object) {
  NULL
}

#' @export
getParamSet.NULLCPO = function(x) {
  makeParamSet()
}

#' @export
getHyperPars.NULLCPO = function(learner, for.fun = c("train", "predict", "both")) {
  setNames(list(), character(0))
}

#' @export
setCPOId.NULLCPO = function(cpo, id) {
  stop("Cannot set ID of NULLCPO.")
}

#' @export
getCPOId.NULLCPO = function(cpo) {
  NULL
}

#' @export
getCPOAffect.NULLCPO = function(cpo, drop.defaults = TRUE) {
  list()
}

#' @export
getCPOName.NULLCPO = function(cpo) {
  "NULLCPO"
}

#' @export
getCPOClass.NULLCPO = function(cpo) {
  "NULLCPO"
}

#' @export
getCPOTrainedCapability.NULLCPO = function(cpo) {
  c(retrafo = 0L, invert = 0L)
}

#' @export
getCPOOperatingType.NULLCPO = function(cpo) {
  character(0)
}

#' @export
getOriginalCPO.NULLCPO = function(cpo) {
  NULLCPO
}

#' @export
getOriginalCPOConstructor.NULLCPO = function(cpo) {
  stop("No CPOConstructor for NULLCPO.")
}

#' @export
composeCPO.NULLCPO = function(cpo1, cpo2) {
  cpo2
}

#' @export
attachCPO.NULLCPO = function(cpo, learner) {
  learner
}

#' @export
applyCPO.NULLCPO = function(cpo, task) {
  task
}

invertCPO.NULLCPO = function(inverter, prediction, predict.type) {
  list(new.prediction = prediction, new.td = NULL, new.truth = NULL)
}

#' @export
invert.NULLCPO = function(inverter, prediction, predict.type = "response") {
  message("(Inversion was a no-op.)")
  prediction
}

#' @export
as.list.NULLCPO = function(x, ...) {
  list()
}

#' @export
print.NULLCPO = function(x, ...) {
  cat("NULLCPO\n")
}
