#' @title Convert Data into Factors indicating Missing Data
#'
#' @description
#' Convert a data.frame into a data.frame with the same column names,
#' but with columns of factors indicating whether data was missing or not.
#'
#' This is most useful in combination with \code{\link{cpoCbind}}.
#'
#' @template cpo_description
#'
#' @param force.dummies [\code{logical(1)}]\cr
#'   Whether to create dummy columns even for data that is not missing.
#'   This can be useful if missing data is expected during test in columns
#'   where it did not occur during training.
#'
#' @template arg_cpo_id
#' @family CPO
#' @export
cpoMissingIndicators = makeCPOExtended("missingindicators", force.dummies = FALSE: logical,  # nolint
  .dataformat = "df.features",
  .properties.needed = "factors",
  .properties.adding = c("numerics", "ordered", "missings"),
  cpo.trafo = {
    dummycols = sapply(data, function(x) any(is.na(x)))
    cpo.retrafo = function(data) {
      data = data[force.dummies | dummycols]
      for (d in names(data)) {
        data[[d]] = as.factor(is.na(data[[d]]))
      }
      data
    }
    cpo.retrafo(data)
  }, cpo.retrafo = NULL)
registerCPO(cpoMissingIndicators, "tools", "imputation", "Generate factorial columns indicating whether data was NA.")

