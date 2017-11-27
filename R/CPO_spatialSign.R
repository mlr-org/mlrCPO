
#' @title Scale Rows to Unit Length
#'
#' @template cpo_doc_intro
#'
#' @description
#' Normalizes the data row-wise. This is a natural
#' generalization of the "sign" function to higher
#' dimensions.
#'
#' @param length [\code{numeric(1)}]\cr
#'   Length to scale rows to. Default is 1.
#' @template cpo_doc_outro
#' @export
cpoSpatialSign = makeCPO("spatial.sign",  # nolint
  pSS(length = 1: numeric[0, ~.]),
  dataformat = "numeric",
  properties.data = c("numerics", "factors", "ordered"),  # no missings
  cpo.train = NULL,
  cpo.retrafo = {
    t(apply(as.matrix(data), 1, function(x) {
      len = sqrt(sum(x ^ 2))
      if (!identical(len, 0)) {
        x = x / len * length
      }
      x
    }))
  })
registerCPO(cpoSpatialSign, "data", "numeric data preprocessing", "Scale numeric rows to given length.")
