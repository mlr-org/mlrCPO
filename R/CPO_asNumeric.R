
#' @title Convert All Features to Numerics
#'
#' @template cpo_doc_intro
#'
#' @description
#' Converts all \code{feature} columns to (integer) \code{numeric} columns by
#' applying \code{\link[base:numeric]{as.numeric}} to them.
#'
#' @template cpo_doc_outro
#' @export
cpoAsNumeric = makeCPO("as.numeric", properties.adding = c("factors", "ordered"), properties.needed = "numerics",  # nolint
  dataformat = "factor", cpo.train = NULL, cpo.retrafo = function(data) {
      as.data.frame(lapply(data, as.numeric), row.names = rownames(data)) })
registerCPO(cpoAsNumeric, "data", "feature conversion", "Convert all Features to Numerics using as.numeric.")
