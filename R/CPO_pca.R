#' @title Construct a CPO for PCA preprocessing
#'
#' @template cpo_description
#'
#' @description
#' Note that data is neither scaled nor centered. Often
#' this needs to be done for PCA, in which case, \code{\link{cpoScale}}
#' should be used in addition (and before) \code{cpoPca}.
#'
#' @template arg_cpo_id
#' @family CPO
#' @export
cpoPca = makeCPOExtended("pca", .dataformat = "numeric", cpo.trafo = {  # nolint
  pcr = prcomp(as.matrix(data), center = FALSE, scale. = FALSE)
  data = as.data.frame(pcr$x)
  control = list(rotation = pcr$rotation)
  data
}, cpo.retrafo = {
  as.data.frame(as.matrix(data) %*% control$rotation)
})
registerCPO(cpoPca, "data", "numeric data preprocessing", "Perform Principal Component Analysis (PCA) using stats::prcomp.")
