#' @title Construct a CPO for scaling / centering
#'
#' @template cpo_description
#'
#' @param center [\code{logical(1)}]\cr
#'   Whether to center the data.
#'   Default is \code{TRUE}.
#' @param scale [\code{logical(1)}]\cr
#'   Whether to scale the data.
#'   Default is \code{TRUE}.
#' @template arg_cpo_id
#' @family CPO
#' @export
cpoScale = makeCPOExtended("scale", center = TRUE: logical, scale = TRUE: logical, .dataformat = "numeric", cpo.trafo = {  # nolint
  result = scale(as.matrix(data), center = center, scale = scale)
  control = list(center = firstNonNull(attr(result, "scaled:center"), FALSE), scale = firstNonNull(attr(result, "scaled:scale"), FALSE))
  result
}, cpo.retrafo = {
  scale(as.matrix(data), center = control$center, scale = control$scale)
})
registerCPO(cpoScale, "data", "numeric data preprocessing", "Center and / or scale the data using base::scale.")
