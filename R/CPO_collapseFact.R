
#' @title Compine Rare Factors
#'
#' @template cpo_description
#'
#' @param max.collapsed.class.prevalence [\code{numeric(1)}]\cr
#'   Maximum prevalence of newly created collapsed factor level.
#'   Default is \code{0.1}.
#'
#' @template arg_cpo_id
#' @family CPO
#' @export
cpoCollapseFact = makeCPOExtended("collapse.fact",  # nolint
  max.collapsed.class.prevalence = 0.1: numeric[0, ~1],
  .dataformat = "factor",
  cpo.trafo = {
    newlevels = sapply(data, function(d) {
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
    cpo.retrafo = function(data) {
      for (n in names(data)) {
        levels(data[[n]]) = newlevels[[n]]
      }
      data
    }
    cpo.retrafo(data)
  }, cpo.retrafo = NULL)
registerCPO(cpoCollapseFact, "data", "factor data preprocessing", "Combine rare factors.")
