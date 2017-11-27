#' @title Create a "model matrix" from the data given a formula
#'
#' @template cpo_doc_intro
#'
#' This uses the \dQuote{stats} function \code{model.matrix} to create
#' (numerical) data from the given data, using the provided formula.
#'
#' @template cpo_description
#'
#' @param formula [\code{formula}]\cr
#'   Formula to use. Higher order interactions can be created using constructs
#'   like \code{~. ^ 2}.
#' @template cpo_doc_outro
#' @export
cpoModelMatrix = makeCPO("model.matrix", fix.factors = TRUE,  # nolint
  par.set = makeParamSet(makeUntypedLearnerParam("formula")), .dataformat = "df.features",
  properties.adding = c("factors", "ordered"), properties.needed = "numerics",
  cpo.train = NULL,
  cpo.retrafo = {
    as.data.frame(model.matrix(formula, data = data))
  })
registerCPO(cpoModelMatrix, "data", "general", "TODO")  # TODO

