#' @title Range Scaling CPO
#'
#' @description
#' Linearly transform data columns so they are
#' between \code{lower} and \code{upper}. If
#' \code{lower} is greater than \code{upper},
#' this will reverse the ordering of input data.
#' \code{NA}, \code{Inf} are ignored.
#'
#' @template cpo_description
#'
#' @param lower [\code{numeric(1)}]\cr
#'   Target value of smallest item of input data.
#'   Default is 0.
#' @param upper [\code{numeric(1)}]\cr
#'   Target value of greatest item of input data.
#'   Default is 1.
#' @template arg_cpo_id
#' @family CPO
#' @export
cpoScaleRange = makeCPOExtended("range.scale", lower = 0: numeric[~., ~.], upper = 1: numeric[~., ~.],  # nolint
  .dataformat = "numeric",
  cpo.trafo = {
    ranges = lapply(data, function(x) {
      rng = range(x, na.rm = TRUE, finite = TRUE)
      # linear transformation to get minimum to 'lower' and maximum to 'upper:
      # x' = a + x * b
      # where b is (upper - lower) / (max(x) - min(x))
      # and   a is -min(x) * b + lower
      b = (upper - lower) / (rng[2] - rng[1])
      a = -rng[1] * b + lower
      c(a, b)
    })
    cpo.retrafo = function(data) {
      for (i in seq_along(data)) {
        trafo = ranges[[i]]
        data[[i]] = trafo[1] + data[[i]] * trafo[2]
      }
      data
    }
    cpo.retrafo(data)
  }, cpo.retrafo = NULL)
registerCPO(cpoScaleRange, "data", "numeric data preprocessing", "Scale numeric columns to lie in a given range.")
