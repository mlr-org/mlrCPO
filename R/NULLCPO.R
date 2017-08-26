
#' @title CPO composition neutral element
#'
#' @description
#' FIXME to come
#'
#' @family CPO
#' @export
NULLCPO = makeS3Obj(c("NULLCPO", "CPOPrimitive", "CPORetrafo", "CPO"))  # nolint

#' @export
is.nullcpo = function(cpo) {  # nolint
  "NULLCPO" %in% class(cpo)
}

#' @export
`%>>%.NULLCPO` = function(cpo1, cpo2) {
  if (any(c("Learner", "CPO", "CPORetrafo") %in% class(cpo2))) {
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
getRetrafoState.NULLCPO = function(retrafo.object) {
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
getCPOBound.NULLCPO = function(cpo) {
  character(0)
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

#' @export
invertCPO.NULLCPO = function(inverter, prediction, predict.type) {
  list(new.prediction = prediction, new.td = NULL, new.truth = NULL)
}

#' @export
as.list.NULLCPO = function(x, ...) {
  list()
}

#' @export
print.NULLCPO = function(x, ...) {
  cat("NULLCPO\n")
}
