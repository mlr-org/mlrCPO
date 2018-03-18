#' @title Construct a CPO for PCA Preprocessing
#'
#' @template cpo_doc_intro
#'
#' @description
#' Performs principal component analysis using \code{\link[stats]{prcomp}}.
#'
#' @param center [\code{logical(1)}]\cr
#'   Whether to center columns before PCA. Default is \code{TRUE}.
#' @param scale [\code{logical(1)}]\cr
#'   Whether to scale columns to unit variance before PCA.
#'   Default is \code{FALSE}.
#' @param tol [\code{numeric(1)} | \code{NULL}]\cr
#'   Magnitude below which components are omitted. Default is
#'   \code{NULL}: all columns returned. Sensible settings are
#'   \code{tol = 0}, \code{tol = sqrt(.Machine$double.eps)}.
#' @param rank [\code{numeric(1)} | \code{NULL}]\cr
#'   Maximal number of components to return. Default is \code{NULL},
#'   no limit.
#'
#' @section CPOTrained State:
#' The state's \code{$control} slot is a list with the \code{$rotation} matrix,
#' the \code{$scale} vector and the \code{$center} vector
#' as returned by \code{\link[stats]{prcomp}}.
#'
#' @template cpo_doc_outro
#' @export
cpoPca = makeCPOExtendedTrafo("pca",  # nolint
  pSS(center = TRUE: logical, scale = FALSE: logical,
    tol = NULL: numeric[0, ] [[special.vals = list(NULL)]],
    rank = NULL: integer[1, ] [[special.vals = list(NULL)]]),
  dataformat = "numeric",
  export = c("center", "scale"),
  cpo.trafo = {
    if (!ncol(data)) {
      emat = matrix(data = numeric(0), nrow = 0, ncol = 0)
      control = list(rotation = emat, scale = numeric(0), center = numeric(0))
      return(data)
    }
    pcr = prcomp(as.matrix(data), center = center, scale. = scale, tol = tol, rank = rank)
    control = pcr[c("rotation", "scale", "center")]
    pcr$x
  },
  cpo.retrafo = {
    scale(as.matrix(data), center = control$center, scale = control$scale) %*% control$rotation
  })
registerCPO(cpoPca, "data", "numeric data preprocessing", "Perform Principal Component Analysis (PCA) using stats::prcomp.")
