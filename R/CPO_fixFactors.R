
#' @title Clean Up Factorial Features
#'
#' @template cpo_doc_intro
#'
#' @description
#' Prevent common pitfalls when using factorial data, by making factorial data have the
#' same levels in training and prediction, and by dropping factor levels that do not
#' occur in training data.
#'
#' @param drop.unused.levels
#'   Factor levels of data that have no instances in the data are dropped. If
#'   \dQuote{fix.factors.prediction} is false, this can lead to training data having
#'   different factor levels than prediction data. Default is \code{TRUE}.
#' @param fix.factors.prediction
#'   Factor levels are kept the same in training and prediction. This is
#'   recommended. Default is \code{TRUE}.
#' @template cpo_doc_outro
#' @export
cpoFixFactors = makeCPOExtendedTrafo("fixfactors",  # nolint
  pSS(drop.unused.levels = TRUE: logical, fix.factors.prediction = TRUE: logical),
  dataformat = "df.features",
  cpo.trafo = {
    if (drop.unused.levels) {
      data = droplevels(data)
    }
    control = Filter(function(x) !is.null(x), lapply(data, levels))
    data
  },
  cpo.retrafo = {
    if (fix.factors.prediction) {
      data = fixFactors(data, control)
    } else if (drop.unused.levels) {
      data = droplevels(data)
    }
    data
  })
registerCPO(cpoFixFactors, "data", "cleanup", "Clean up Factorial Features.")


