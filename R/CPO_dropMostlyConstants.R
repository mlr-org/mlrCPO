#' @title Drop Constant or Near-Constant Features
#'
#' @template cpo_doc_intro
#'
#' @description
#' Drop all columns that are mostly constant: Constant within tolerance
#' with numerics, and
#' and columns that have only one value for factors or ordered columns.
#'
#' This CPO can also filter \dQuote{mostly} constant Features:
#' ones where at most a fraction of \code{ratio} samples differ from the
#' mode value.
#'
#' @param ratio [\code{numeric(1)}]\cr
#'   Minimum ratio of values which must be different from the mode value in
#'   order to keep a feature in the task. Default is 0, which means only
#'   constant features with exactly one observed level are removed.
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
cpoDropMostlyConstants = makeCPO("dropmostlyconst",  # nolint
  pSS(ratio = 0: numeric[0, ~1], rel.tol = 1e-8: numeric[0, ~.],
    abs.tol = 1e-8: numeric[0, ~.], ignore.na = FALSE: logical),
  dataformat = "df.features",
  cpo.train = {
    !vlapply(data, is.constant.enough,
      ratio = ratio, rel.tol = rel.tol, abs.tol = abs.tol,
      ignore.na = ignore.na)
  },
  cpo.retrafo = {
    data[control]
  })

registerCPO(cpoDropMostlyConstants, "data", "cleanup", "Drop mostly constant or mostly near-constant Features")


is.constant.enough = function(x, ratio, rel.tol, abs.tol, ignore.na) {
  x[is.nan(x)] = NA
  if (ignore.na) {
    x = x[!is.na(x)]
  }
  if (!length(x)) return(TRUE)
  required.size = length(x) - floor(length(x) * ratio)
  if (required.size <= 1) return(TRUE)

  if (is.numeric(x)) {
    # consider non-finite values first (as if they are distinct)
    # note that 'NA' is not 'finite' and not 'infinite'.
    x.nonf = x[!is.finite(x)]
    if (any(table(x.nonf, useNA = "ifany") >= required.size)) return(TRUE)

    # now consider finite values: sort them and see if items that are
    # 'required.size - 1' steps away from each other differ by at most 'abs.tol' or 'rel.tol'
    x = sort(x[is.finite(x)])
    if (length(x) < required.size) return(FALSE)

    fst = x[seq.int(1, length(x) - required.size + 1)]
    lst = x[seq.int(required.size, length(x))]

    # check both abs and rel difference
    any(abs(fst - lst) <= abs.tol) ||
      any(2 * abs(fst - lst) / (abs(fst) + abs(lst)) <= rel.tol)
  } else {
    tbl = table(x, useNA = "ifany")
    # just check if any category is larger than the required category size
    max(tbl) >= required.size
  }
}
