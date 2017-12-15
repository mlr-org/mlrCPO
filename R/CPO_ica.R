
#' @title Construct a CPO for ICA Preprocessing
#'
#' @template cpo_doc_intro
#'
#' @description
#' Use the \code{\link[fastICA]{fastICA}} function implementing the
#' \dQuote{FastICA algorithm}. See the documentation there.
#'
#' @param n.comp [\code{numeric(1)} | \code{NULL}]\cr
#'   Number of components to extract. Default is \code{NULL}, which sets it
#'   to the number of available numeric columns.
#' @param alg.typ [\code{character(1)}]\cr
#'   Algorithm type. One of \dQuote{parallel} (default) or \dQuote{deflation}.
#' @param fun [\code{character(1)}]\cr
#'   One of \dQuote{logcosh} (default) or \dQuote{exp}.
#' @param alpha [\code{numeric(1)}]\cr
#'   In range [1, 2], Used for negentropy calculation when \code{fun} is \dQuote{logcosh}.
#'   Default is 1.0.
#' @param method [\code{character(1)}]\cr
#'   Internal calculation method. \dQuote{C} (default) or \dQuote{R}.
#' @param maxit [\code{numeric(1)}]\cr
#'   Maximum number of iterations. Default is 200.
#' @param tol [\code{numeric(1)}]\cr
#'   Tolerance for convergence, default is \code{1e-4}.
#' @param verbose [\code{logical(1)}]\cr
#'   Default is \code{FALSE}.
#'
#' @section CPOTrained State:
#' The state contains a \code{$control} slot with the \code{$K},
#' \code{$W} and \code{$A} slots of the \code{\link[fastICA]{fastICA}} call,
#' as well as a \code{$center} slot indicating the row-wise center of the
#' training data that will be subtracted before rotation.
#'
#'
#' @template cpo_doc_outro
#' @export
cpoIca = makeCPOExtendedTrafo("ica",  # nolint
  pSS(n.comp = NULL: integer[1, ] [[special.vals = list(NULL)]],
    alg.typ = "parallel": discrete[parallel, deflation],
    fun = "logcosh": discrete[logcosh, exp],
    alpha = 1.0: numeric[1, 2] [[requires = quote(fun == "logcosh")]],
    method = "C": discrete[C, R],
    maxit = 200: integer[1, ],
    tol = 1e-4: numeric[~0, ],
    verbose = FALSE: logical),
  dataformat = "numeric", packages = "fastICA",
  export = c("n.comp", "alg.typ", "fun", "alpha"),
  cpo.trafo = {
    if (!ncol(data)) {
      emat = matrix(data = numeric(0), nrow = 0, ncol = 0)
      control = list(K = emat, W = emat, A = emat, center = numeric(0))
      return(data)
    }
    if (is.null(n.comp)) {
      n.comp = ncol(data)
    }
    control = fastICA::fastICA(data, n.comp = n.comp, alg.typ = alg.typ, fun = fun, alpha = alpha,
      method = method, maxit = maxit, tol = tol, verbose = verbose)
    ret = control$S
    control$S = NULL
    control$X = NULL
    control$center = vnapply(data, mean)
    ret
  },
  cpo.retrafo = {
    scale(as.matrix(data), scale = FALSE, center = control$center) %*% (control$K %*% control$W)
  })
registerCPO(cpoIca, "data", "numeric data preprocessing", "Perform the fastICA algorithm of the fastICA package.")
