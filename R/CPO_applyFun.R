#' @include makeCPO.R auxiliary.R

#' @title Apply a Function Element-Wise
#'
#' @description
#' The function must either vectorize over the given data,
#' or will be applied to each data element on its own.
#'
#' It must not change the type of the data, i.e. numeric
#' data must remain numeric etc.
#'
#' If the function can only handle a subset of the given columns,
#' e.g. only a certain type, use \code{affect.*} arguments.
#'
#' @template cpo_description
#'
#' @param fun [\code{function}]\cr
#'   The function to apply. Must take one argument. If
#'   \code{vectorize} is \code{TRUE}, the argument is the
#'   whole column, \code{fun} must vectorize over it;
#'   otherwise, the function gets called once for
#'   every data item, and both the function argument and
#'   the return value must have length 1.
#' @param vectorize [\code{logical(1)}]\cr
#'   Whether to call \code{fun} once for each column, or
#'   once for each element. If \code{fun} vectorizes,
#'   it is recommended to have this set to \code{TRUE}
#'   for better performance. Default is \code{TRUE}.
#' @template arg_cpo_id
#' @family CPO
#' @export
cpoApplyFun = makeCPOExtended("fun.apply",  # nolint
  .par.set = makeParamSet(
      makeFunctionLearnerParam("fun"),
      makeLogicalLearnerParam("vectorize", default = TRUE)),
  .dataformat = "df.features", cpo.trafo = {
    if (vectorize) {
      fun2 = fun
    } else {
      fun2 = function(col) {
        sapply(col, function(x) {
          ret = fun(x)
          if (length(ret) != 1) {
            stop("cpoApplyFun 'fun' did not return a result with length 1")
          }
        })
      }
    }
    cpo.retrafo = function(data) {
      as.data.frame(lapply(data, fun2))
    }
    cpo.retrafo(data)
  }, cpo.retrafo = NULL)
registerCPO(cpoApplyFun, "data", "general data preprocessing", "Apply an arbitrary function column-wise.")

