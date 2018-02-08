#' @title Convert Data into Factors Indicating Missing Data
#'
#' @template cpo_doc_intro
#'
#' @description
#' Convert a data.frame into a data.frame with the same column names,
#' but with columns of factors indicating whether data was missing or not.
#'
#' This is most useful in combination with \code{\link{cpoCbind}}.
#'
#' @param force.dummies [\code{logical(1)}]\cr
#'   Whether to create dummy columns even for data that is not missing.
#'   This can be useful if missing data is expected during test in columns
#'   where it did not occur during training.
#'
#' @template cpo_doc_outro
#' @export
cpoMissingIndicators = makeCPO("missingindicators",  # nolint
  pSS(force.dummies = FALSE: logical),
  dataformat = "df.features",
  properties.needed = "factors",
  properties.adding = c("numerics", "ordered", "missings"),
  cpo.train = {
    vlapply(data, function(x) any(is.na(x)))
  },
  cpo.retrafo = {
    data = data[force.dummies | control]
    for (d in names(data)) {
      data[[d]] = factor(is.na(data[[d]]), levels = c("FALSE", "TRUE"))
    }
    data
  })
registerCPO(cpoMissingIndicators, "tools", "imputation", "Generate factorial columns indicating whether data was NA.")

