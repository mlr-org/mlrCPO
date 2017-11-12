

#' @title Max Abs Scaling CPO
#'
#' @description
#' Scale the numeric data columns so their maximum absolute value
#' is \code{maxabs}, if possible. \code{NA}, \code{Inf} are ignored, and features that are constant 0
#'   are not scaled.
#'
#' @template cpo_description
#'
#' @param maxabs [\code{numeric(1)}]\cr
#'   The maximum absolute value for each column after transformation. Default is 1.
#' @template arg_cpo_id
#' @family CPO
#' @export
cpoScaleMaxAbs = makeCPOExtended("maxabs.scale", maxabs = 1: numeric[0, ~.],  # nolint
  .dataformat = "numeric", .trafo.type = "trafo.returns.control",
  cpo.trafo = {
    scaling = lapply(data, function(x) {
      s = max(abs(range(x, na.rm = TRUE, finite = TRUE)))
      if (s == 0) {
        s = 1
      }
      s
    })
    function(data) {
      for (i in seq_along(data)) {
        data[[i]] = data[[i]] / scaling[[i]] * maxabs
      }
      data
    }
  }, cpo.retrafo = NULL)
registerCPO(cpoScaleMaxAbs, "data", "numeric data preprocessing", "Scale numeric columns to get a specific maximum absolute value.")
