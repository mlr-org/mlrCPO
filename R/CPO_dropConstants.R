#' @title Drop Constant or Near-Constant Features
#'
#' @template cpo_doc_intro
#'
#' @description
#' Drop all columns that are either constant, or close to constant for numerics,
#' and columns that have only one value for factors or ordered columns.
#'
#' @param rel.tol [\code{numeric(1)}]\cr
#'   Relative tolerance within which to consider a feature constant.
#'   Set to 0 to disregard relative tolerance.
#'   Default is \code{1e-8}.
#' @param abs.tol [\code{numeric(1)}]\cr
#'   Absolute tolerance within which to consider a feature constant.
#'   Set to 0 to disregard absolute tolerance.
#'   Default is \code{1e-8}.
#' @param ignore.na [\code{logical(1)}]\cr
#'   Whether to ignore \code{NA} and \code{NaN} values. If this is
#'   \code{TRUE}, values that are \code{NA} or \code{NaN} will not
#'   be counted as different from any other value. If this is
#'   \code{FALSE}, columns with \code{NA} or \code{NaN} in them will
#'   only count as constant if they are entirely made up of \code{NA},
#'   or entirely made up of \code{NaN}.
#'   Default is \code{FALSE}.
#' @template cpo_doc_outro
#' @export
cpoDropConstants = makeCPO("dropconst",  # nolint
  pSS(rel.tol = 1e-8: numeric[0, ~.], abs.tol = 1e-8: numeric[0, ~.],
    ignore.na = FALSE: logical),
  dataformat = "df.features",
  cpo.train = {
    vlapply(data, function(col) {
      if (ignore.na) {
        col = col[!(is.na(col) | is.nan(col))]
      }
      if (is.numeric(col)) {
        if (all(col == Inf) || all(col == -Inf) || all(is.nan(col)) || all(is.na(col))) {
          return(FALSE)
        }
        if (any(col == Inf) || any(col == -Inf) || any(is.nan(col)) || any(is.na(col))) {
          return(TRUE)
        }
        cmean = mean(col)
        return(!(all(abs(col - cmean) <= abs.tol) || all(abs((col - cmean) / cmean) <= rel.tol)))
      }
      if (all(is.na(col))) {
        return(FALSE)
      }
      if (any(is.na(col))) {
        return(TRUE)
      }
      return(!all(col == col[1]))
    })
  },
  cpo.retrafo = {
    data[control]
  })
registerCPO(cpoDropConstants, "data", "cleanup", "Drop constant or near-constant Features.")
