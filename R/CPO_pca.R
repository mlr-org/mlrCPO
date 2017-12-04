#' @title Construct a CPO for PCA Preprocessing
#'
#' @template cpo_doc_intro
#'
#' @description
#' Note that data is neither scaled nor centered. Often
#' this needs to be done for PCA, in which case, \code{\link{cpoScale}}
#' should be used in addition (and before) \code{cpoPca}.
#'
#' @template cpo_doc_outro
#' @export
cpoPca = makeCPOExtendedTrafo("pca", dataformat = "numeric",  # nolint
  cpo.trafo = {
    pcr = prcomp(as.matrix(data), center = FALSE, scale. = FALSE)
    control = list(rotation = pcr$rotation)
    pcr$x
  },
  cpo.retrafo = {
    as.matrix(data) %*% control$rotation
  })
registerCPO(cpoPca, "data", "numeric data preprocessing", "Perform Principal Component Analysis (PCA) using stats::prcomp.")
