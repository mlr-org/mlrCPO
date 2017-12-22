

#' @title Perform SMOTE Oversampling for Binary Classification
#'
#' @template cpo_doc_intro
#'
#' @description
#' Uses \code{mlr}'s \code{\link[mlr]{smote}} function to perform
#' \dQuote{Synthetic Minority Oversampling TEchnique} sample generation
#' to handle class imbalance in binary tasks.
#'
#' See the \code{\link[mlr]{smote}} documentation for details.
#'
#' @param rate [\code{numeric(1)} | \code{NULL}]\cr
#'   Upsampling factor, between 1 and \code{Inf}. Default is
#'   \code{NULL}, which sets this to the ratio \code{<majority prevalence>}
#'   \code{/} \code{<minority prevalence>}
#' @param nn [\code{integer(1)}]\cr
#'   Number of nearest neighbours to consider. Defaults to 5.
#' @param standardize [\code{integer(1)}]\cr
#'   Standardize feature values. Default is \code{TRUE}.
#' @param alt.logic [\code{integer(1)}]\cr
#'   Use alternative logic for minority selection. Default is \code{FALSE}.
#'
#' @template cpo_doc_outro
#' @export
cpoSmote = makeCPORetrafoless("sw",  # nolint
  pSS(rate = NULL: numeric[1, ~.] [[special.vals = list(NULL)]],
    nn = 5: integer[1, ], standardize = TRUE: logical, alt.logic = FALSE: logical),
  dataformat = "task",
  properties.target = c("classif", "twoclass"),
  cpo.trafo = {
    if (is.null(rate)) {
      cdist = table(getTaskData(data, target.extra = TRUE)$target)
      assert(length(cdist) == 2)
      rate = max(cdist) / min(cdist)
    }
    smote(data, rate = rate, nn = nn, standardize = standardize, alt.logic = alt.logic)
  })
registerCPO(cpoSmote(), "subsampling", "binary classif", "synthetic minority oversampling technique")
