#' @title Range Scaling CPO
#'
#' @template cpo_doc_intro
#'
#' @description
#' Linearly transform data columns so they are
#' between \code{lower} and \code{upper}. If
#' \code{lower} is greater than \code{upper},
#' this will reverse the ordering of input data.
#' \code{NA}, \code{Inf} are ignored.
#'
#' @param lower [\code{numeric(1)}]\cr
#'   Target value of smallest item of input data.
#'   Default is 0.
#' @param upper [\code{numeric(1)}]\cr
#'   Target value of greatest item of input data.
#'   Default is 1.
#' @template cpo_doc_outro
#' @export
cpoScaleRange = makeCPO("range.scale",  # nolint
  pSS(lower = 0: numeric[~., ~.], upper = 1: numeric[~., ~.]),
  dataformat = "numeric",
  cpo.train = {
    lapply(data, function(x) {
      rng = range(x, na.rm = TRUE, finite = TRUE)
      # linear transformation to get minimum to 'lower' and maximum to 'upper:
      # x' = a + x * b
      # where b is (upper - lower) / (max(x) - min(x))
      # and   a is -min(x) * b + lower
      b = (upper - lower) / (rng[2] - rng[1])
      a = -rng[1] * b + lower
      c(a, b)
    })
  },
  cpo.retrafo = {
    for (i in seq_along(data)) {
      trafo = control[[i]]
      data[[i]] = trafo[1] + data[[i]] * trafo[2]
    }
    data
  })
registerCPO(cpoScaleRange, "data", "numeric data preprocessing", "Scale numeric columns to lie in a given range.")
