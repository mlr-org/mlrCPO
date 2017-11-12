
#' @title Create a "model matrix" from the data given a formula
#'
#' This uses the \dQuote{stats} function \code{model.matrix} to create
#' (numerical) data from the given data, using the provided formula.
#'
#' @template cpo_description
#'
#' @param formula [\code{formula}]\cr
#'   Formula to use. Higher order interactions can be created using constructs
#'   like \code{~. ^ 2}.
#'
#' @template arg_cpo_id
#' @family CPO
#' @export
cpoModelMatrix = makeCPOExtended("model.matrix", .fix.factors = TRUE, .trafo.type = "stateless",  # nolint
  .par.set = makeParamSet(makeUntypedLearnerParam("formula")), .dataformat = "df.features",
  .properties.adding = c("factors", "ordered"), .properties.needed = "numerics",
  cpo.trafo = {
    as.data.frame(model.matrix(formula, data = data))
  }, cpo.retrafo = NULL)
registerCPO(cpoSelect, "data", "general", ".")

