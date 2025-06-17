#' @return [\code{\link{CPO}}].
#' @family CPOs
#'
#' @section General CPO info:
#' This function creates a CPO object, which can be applied to
#' \code{\link[mlr]{Task}}s, \code{data.frame}s, \code{link[mlr]{Learner}}s
#' and other CPO objects using the \code{\link{\%>>\%}} operator.
#'
#' The parameters of this object can be changed after creation
#' using the function \code{\link[mlr]{setHyperPars}}. The other
#' hyper-parameter manipulating functins, \code{\link[mlr]{getHyperPars}}
#' and \code{\link[ParamHelpers]{getParamSet}} similarly work as one expects.
#'
#' If the \dQuote{id} parameter is given, the hyperparameters
#' will have this id as aprefix; this will, however, not change
#' the parameters of the creator function.
#' @section Calling a \code{\link{CPOConstructor}}:
#' CPO constructor functions are called with optional values of parameters, and additional \dQuote{special} optional values.
#' The special optional values are the \code{id} parameter, and the \code{affect.*} parameters. The \code{affect.*} parameters
#' enable the user to control which subset of a given dataset is affected. If no \code{affect.*} parameters are given, all
#' data features are affected by default.
#'
#' @inheritParams cpoTemplate
