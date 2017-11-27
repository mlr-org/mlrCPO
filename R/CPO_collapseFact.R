
#' @title Compine Rare Factors
#'
#' @template cpo_doc_intro
#'
#' @description
#' Combine rare factor levels into a single factor level.
#'
#' @param max.collapsed.class.prevalence [\code{numeric(1)}]\cr
#'   Maximum prevalence of newly created collapsed factor level.
#'   Default is \code{0.1}.
#' @template cpo_doc_outro
#' @export
cpoCollapseFact = makeCPO("collapse.fact",  # nolint
  pSS(max.collapsed.class.prevalence = 0.1: numeric[0, ~1]),
  dataformat = "factor",
  cpo.train = {
    sapply(data, function(d) {
      if (all(is.na(d))) {
        return(levels(d))
      }
      fractions = cumsum(sort(table(d))) / sum(!is.na(d))
      collapse = names(fractions)[fractions < max.collapsed.class.prevalence]
      if (length(collapse) > 1) {
        nocollapse = setdiff(levels(d), collapse)
        lvls = list(collapsed = collapse)
        insert(lvls, stats::setNames(as.list(nocollapse), nocollapse))
      } else {
        levels(d)
      }
    }, simplify = FALSE)
  },
  cpo.retrafo = {
    for (n in names(data)) {
      levels(data[[n]]) = control[[n]]
    }
    data
  })
registerCPO(cpoCollapseFact, "data", "factor data preprocessing", "Combine rare factors.")
