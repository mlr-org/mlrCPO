


# A CPO that takes a list of expressions and turns them into columns of new data
#
# Since the 'expr' and 'superceding.env' is not very user-friendly, this is wrapped
# by `cpoMakeCols`, see there.
barecpoMakeCols = makeCPO("new.cols", pSS(expr: untyped, superceding.env: untyped, make.factors: logical, add.cols: logical),  # nolint
  dataformat = "df.all",
  properties.adding = c("numerics.sometimes", "factors.sometimes", "ordered.sometimes"),
  properties.needed = c("numerics.sometimes", "factors.sometimes", "ordered.sometimes"),
  cpo.train = { target },
  cpo.retrafo = {
    target = control
    if (length({badcols = intersect(names(expr), target)})) {
      stopf("Columns %s can not be created, since they clash with the Task's target names", collapse(badcols, ", "))
    }
    if (add.cols && length({badcols = intersect(names(expr), colnames(data))})) {
      stopf("Columns %s can not be created, since they clash with the Task's feature names", collapse(badcols, ", "))
    }
    reserved = "..CPO..RESERVED.."
    if (length(target)) {
      reserved = target[1]
    }
    expr[[reserved]] = seq_len(nrow(data))
    ret = data.frame(lapply(expr, function(ex) {
      val = eval(ex, data, superceding.env)
      if (make.factors && (is.character(val) || is.logical(val))) {
        factor(val)
      } else {
        val
      }
    }), stringsAsFactors = FALSE)
    ret = dropNamed(ret, reserved)
    if (add.cols) {
      cbind(data, ret)
    } else {
      ret
    }
  })


#' @title Create Columns from Expressions
#'
#' @template cpo_doc_intro
#'
#' @description
#' Create columns from expressions and the incoming data.
#'
#' When \code{cpoMakeCols} or \code{cpoAddCols} are called as
#' \code{cpoMakeCols( <newcolname> = <expression>, ... )}, a new column
#' with the name \code{<newcolname} containing the result of
#' \code{<expression>} is created. The
#' expressions need to be vectorising R expressions
#' and may refer to any feature columns in the data (excluding the
#' target) and any other values. The names should be valid \code{data.frame}
#' column names and may not clash with the target column name.
#'
#' \code{cpoMakeCols} replaces existing cols by the newly created
#' ones, \code{cpoAddCols} adds them to the data already present.
#'
#' @param ... [any]\cr
#'   Expressions of the form \code{colname = expr}. See Examples.
#' @param .make.factors [\code{logical(1)}]\cr
#'   Whether to turn resulting \code{logical} and \code{character}
#'   columns into \code{factor} columns (which are preferred
#'   by \code{mlr}). Default is \code{TRUE}.
#'
#' @section CPOTrained State:
#' The created state is empty.
#'
#' @examples
#' res = pid.task %>>% cpoAddCols(gpi = glucose * pressure * insulin, pm = pregnant * mass)
#' head(getTaskData(res))
#' @template cpo_doc_outro
#' @export
cpoMakeCols = function(..., .make.factors = TRUE) {
  mc = match.call()
  mc[[1]] = alist
  expr = eval.parent(mc)
  expr$.make.factors = NULL
  superceding.env = parent.frame()

  barecpoMakeCols(expr = expr, superceding.env = superceding.env, make.factors = .make.factors, add.cols = FALSE, export = character(0))
}
cpoMakeCols = wrapFauxCPOConstructor(cpoMakeCols)  # nolint
registerCPO(cpoMakeCols, "data", "features", "Replace columns by columns generated from expressions")

#' @rdname cpoMakeCols
#' @export
cpoAddCols = function(..., .make.factors = TRUE) {
  mc = match.call()
  mc[[1]] = alist
  expr = eval.parent(mc)
  superceding.env = parent.frame()

  barecpoMakeCols(expr = expr, superceding.env = superceding.env, make.factors = .make.factors, add.cols = TRUE, export = character(0))
}
cpoAddCols = wrapFauxCPOConstructor(cpoAddCols)  # nolint
registerCPO(cpoAddCols, "data", "features", "Add columns generated from expressions")
