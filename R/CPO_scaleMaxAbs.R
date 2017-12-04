#' @title Max Abs Scaling CPO
#'
#' @template cpo_doc_intro
#'
#' @description
#' Scale the numeric data columns so their maximum absolute value
#' is \code{maxabs}, if possible. \code{NA}, \code{Inf} are ignored, and features that are constant 0
#'   are not scaled.
#'
#' @param maxabs [\code{numeric(1)}]\cr
#'   The maximum absolute value for each column after transformation. Default is 1.
#' @template cpo_doc_outro
#' @export
cpoScaleMaxAbs = makeCPO("maxabs.scale",  # nolint
  pSS(maxabs = 1: numeric[0, ~.]),
  dataformat = "numeric",
  cpo.train = {
    lapply(data, function(x) {
      s = max(abs(range(x, na.rm = TRUE, finite = TRUE)))
      if (s == 0) {
        s = 1
      }
      s
    })
  },
  cpo.retrafo = {
    for (i in seq_along(data)) {
      data[[i]] = data[[i]] / control[[i]] * maxabs
    }
    data
  })
registerCPO(cpoScaleMaxAbs, "data", "numeric data preprocessing", "Scale numeric columns to get a specific maximum absolute value.")
