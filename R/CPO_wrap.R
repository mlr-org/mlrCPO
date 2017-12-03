
#' @title CPO Wrapper
#'
#' @template cpo_doc_intro
#'
#' @description
#' Applies the \code{\link{CPO}} that is given to the \code{\link{CPO}} hyperparameter.
#'
#' \code{cpoWrap} only wraps Feature Operation CPOs,
#' \code{cpoWrapRetrafoless} only wraps Retrafoless CPOs.
#'
#' Target Operation CPOs currently cannot be wrapped, sorry.
#'
#' @param cpo [\code{\link{CPO}}]\cr
#'   The CPO to wrap.
#' @template cpo_doc_outro
#' @family special CPOs
#' @export
cpoWrap = makeCPOExtendedTrafo("wrap", makeParamSet(makeUntypedLearnerParam("cpo")), dataformat = "task",  # nolint
  properties.adding = paste0(cpo.dataproperties, ".sometimes"), properties.needed = paste0(cpo.dataproperties, ".sometimes"),
  cpo.trafo = {
    optype = getCPOOperatingType(cpo)
    if (any(c("target", "retrafoless") %in% optype)) {
      stopf("cpoWrap can only use Feature Operation CPOs, but got CPO with OperatingType(s) '%s'",
        collapse(optype, "', '"))
    }
    control = retrafo({res = data %>>% cpo}) ; res }, cpo.retrafo = { data %>>% control })
registerCPO(cpoWrap, "meta", "wrap", "Apply a freely chosen Feature Operation CPO, without exporting its hyperparameters.")

#' @rdname cpoWrap
#' @export
cpoWrapRetrafoless = makeCPORetrafoless("wrap", makeParamSet(makeUntypedLearnerParam("cpo")), dataformat = "task",  # nolint
  properties.adding = paste0(cpo.dataproperties, ".sometimes"), properties.needed = paste0(cpo.dataproperties, ".sometimes"),
  cpo.trafo = {
    optype = getCPOOperatingType(cpo)
    if (any(c("target", "feature") %in% optype)) {
      stopf("cpoWrapRetrafoless can only use Retrafoless CPOs, but got CPO with OperatingType(s) '%s'",
        collapse(optype, "', '"))
    }
    control = retrafo({res = data %>>% cpo}) ; res })
registerCPO(cpoWrapRetrafoless, "meta", "wrap", "Apply a freely chosen Retrafoless CPO, without exporting its hyperparameters.")

