
#' @title Scale Rows to Unit Length
#'
#' @description
#' Normalizes the data row-wise. This is a natural
#' generalization of the "sign" function to higher
#' dimensions.
#'
#' @template cpo_description
#'
#' @param length [\code{numeric(1)}]\cr
#'   Length to scale rows to. Default is 1.
#'
#' @template arg_cpo_id
#' @family CPO
#' @export
cpoSpatialSign = makeCPOExtended("spatial.sign", length = 1: numeric[0, ~.], .dataformat = "numeric", .trafo.type = "stateless",  # nolint
  .properties = c("numerics", "factors", "ordered"),  # no missings
  cpo.trafo = {
    t(apply(as.matrix(data), 1, function(x) {
      len = sqrt(sum(x ^ 2))
      if (!identical(len, 0)) {
        x = x / len * length
      }
      x
    }))
  }, cpo.retrafo = NULL)
registerCPO(cpoSpatialSign, "data", "numeric data preprocessing", "Scale numeric rows to given length.")
