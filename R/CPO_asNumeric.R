
#' @title Convert All Features to Numerics
#'
#' @template cpo_description
#'
#' @template arg_cpo_id
#' @family CPO
#' @export
cpoAsNumeric = makeCPOExtended("as.numeric", .properties.adding = c("factors", "ordered"), .properties.needed = "numerics",  # nolint
  .trafo.type = "stateless", .dataformat = "factor", cpo.trafo = function(data, target) {
    as.data.frame(lapply(data, as.numeric), row.names = rownames(data)) }, cpo.retrafo = function(data) {
      as.data.frame(lapply(data, as.numeric), row.names = rownames(data)) })
registerCPO(cpoAsNumeric, "data", "feature conversion", "Convert all Features to Numerics using as.numeric.")
