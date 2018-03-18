#' @include makeCPO.R auxiliary.R fauxCPOConstructor.R

#' @title Apply a Function Element-Wise
#'
#' @template cpo_doc_intro
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
#' @param fun [\code{function}]\cr
#'   The function to apply. If
#'   \code{vectorize} is \code{TRUE}, the argument is a vector of the
#'   whole column, \code{fun} must vectorize over it and return
#'   a vector of the same length;
#'   otherwise, the function gets called once for
#'   every data item, and both the function argument and
#'   the return value must have length 1.
#'
#'   The function must take one or two arguments. If it takes
#'   two arguments, the second argument will be \code{param}.
#' @param param [any]\cr
#'   Optional argument to be given to \code{fun}. If \code{fun}
#'   only takes one argument, this is ignored. Default is \code{NULL}.
#' @param vectorize [\code{logical(1)}]\cr
#'   Whether to call \code{fun} once for each column, or
#'   once for each element. If \code{fun} vectorizes,
#'   it is recommended to have this set to \code{TRUE}
#'   for better performance. Default is \code{TRUE}.
#' @param make.factors [\code{logical(1)}]\cr
#'   Whether to turn resulting \code{logical} and \code{character}
#'   columns into \code{factor} columns (which are preferred by
#'   \code{mlr}). Default is \code{TRUE}.
#'
#' @section CPOTrained State:
#' The created state is empty.
#'
#' @template cpo_doc_outro
#' @export
cpoApplyFun = makeCPO("fun.apply",  # nolint
  pSS(fun: funct, param = NULL: untyped, vectorize = TRUE: logical, make.factors = TRUE: logical),
  export = "param",
  properties.adding = paste0(cpo.dataproperties, ".sometimes"),
  properties.needed = paste0(cpo.dataproperties, ".sometimes"),
  dataformat = "df.features",
  cpo.train = NULL,
  cpo.retrafo = {
    fun = augmentFun(fun, 1, param)
    if (!vectorize) {
      fun = vectorizeFun(fun, 1, "cpoApplyFun")
    }
    outerfun = function(col) {
      result = fun(col)
      if (length(result) != nrow(data)) {
        stop("cpoApplyFun 'fun' return value had the wrong length.")
      }
      if (!is.atomic(result)) {
        stop("cpoApplyFun 'fun' did not return values that simplified to an atomic vector.")
      }
      if (make.factors && (is.character(result) || is.logical(result))) {
        result = factor(result)
      }
      result
    }
    as.data.frame(lapply(data, outerfun), stringsAsFactors = FALSE, row.names = rownames(data))
  })


#' @title Transform a Regression Target Variable
#'
#' @template cpo_doc_intro
#'
#' @description
#' Apply a given function to the target column of a regression \code{\link{Task}}.
#'
#' @section Details:
#'   When both \code{mean} and \code{se} prediction is available, it may be possible to
#'   make more accurate mean inversion than for the \code{response} \code{predict.type},
#'   using integrals or approximations like the \emph{delta method}. In such cases it may be
#'   advisable to prepend this \code{\link{CPO}} with the \code{\link{cpoResponseFromSE}}
#'   \code{\link{CPO}}.
#'
#'   Note when \code{trafo} or \code{invert.response} take more than one argument, the
#'   second argument will be set to the value of \code{param}. This may lead to unexpected
#'   results when using functions with rarely used parameters, e.g. \code{\link[base:Log]{log}}.
#'   In these cases, it may be necessary to wrap the function:
#'   \code{trafo = function(x) log(x)}.
#'
#' @param trafo [\code{function}]\cr
#'   A function transforming the target column. If \code{vectorize} is \code{TRUE},
#'   the argument is a vector of the whole column, \code{trafo} must vectorize over it
#'   and return a vector of the same length; otherwise, the function gets called once
#'   for every data item, and both the function argument and the return value
#'   must have length 1.
#'
#'   The function must take one or two arguments. If it takes two arguments, the second argument
#'   will be \code{param}.
#' @param invert.response [\code{function}]\cr
#'   If a model is trained on data that was transformed by \code{trafo}, this function
#'   should invert a prediction made by this model back to the space of the original data.
#'   In most cases, this will be the inverse of \code{trafo}, so that \code{invert.response(trafo(x)) == x}.
#'
#'   Similarly to \code{trafo}, this function takes / produces single elements or the whole
#'   column, depending on \code{vectorize}. The return value should be a \code{numeric}
#'   in both cases.
#'
#'   This can also be \code{NULL}, in which case using this \code{\link{CPO}} for
#'   \code{\link{invert}} with \code{predict.type = "response"} is not possible.
#'
#'   Default is \code{NULL}.
#' @param invert.se [\code{function}]\cr
#'   Similarly to \code{invert.response}, this is a function that inverts a \code{"se"}
#'   prediction made after training on \code{trafo}'d data. This function should take
#'   at least two arguments, \code{mean} and \code{se}, and return a numeric vector of length
#'   2 if \code{vectorize} is \code{FALSE}, or a \code{data.frame} or \code{matrix} with
#'   two numeric columns if \code{vectorize} is \code{TRUE}. The function may also take a third
#'   argument, which will be set to \code{param}.
#'
#'   \code{invert.se} may also be \code{NULL}, in which case \dQuote{se} inversion is done
#'   by numeric integration using Gauss-Hermite quadrature.
#'
#'   Default is \code{NULL}.
#' @param param [any]\cr
#'   Optional argument to be given to \code{trafo} and / or \code{invert}. If both of
#'   them only take one argument, this is ignored. Default is \code{NULL}.
#' @param vectorize [\code{logical(1)}]\cr
#'   Whether to call \code{trafo}, \code{invert.response} and \code{invert.se} once
#'   with the whole data column (or response \emph{and} se column if \code{predict.type == "se"}),
#'   or once for each element. If the functions vectorize, it is recommended to have this
#'   set to \code{TRUE} for better performance. Default is \code{TRUE}.
#' @param gauss.points [\code{numeric(1)}]\cr
#'   Number of points at which to evaluate \code{invert.response} for Gauss-Hermite quadrature integration.
#'   Only used if \code{invert.se} is \code{NULL}. Default is \code{23}.
#' @template cpo_doc_outro
#' @export
cpoApplyFunRegrTarget = makeCPOTargetOp("fun.apply.regr.target",  # nolint
  pSS(trafo: funct,
    invert.response = NULL: funct [[special.vals = list(NULL)]],
    invert.se = NULL: funct [[special.vals = list(NULL)]],
    param = NULL: untyped, vectorize = TRUE: logical,
    gauss.points = 23: integer[3, ]),
  properties.target = "regr", predict.type.map = c(response = "response", se = "se"),
  export = "param",
  constant.invert = TRUE,
  cpo.train = NULL, cpo.train.invert = NULL,
  cpo.retrafo = {
    trafo = augmentFun(trafo, 1, param)
    if (!vectorize) {
      trafo = vectorizeFun(trafo, 1, "cpoApplyFunRegrTarget")
    }
    if (!is.null(invert.response)) augmentFun(invert.response, 1, param)  # just for checking
    if (!is.null(invert.se)) augmentFun(invert.se, 2, param)  # just for checking
    targettrans = trafo(target[[1]])
    if (!is.numeric(targettrans)) {
      stop("cpoApplyFunRegrTarget trafo did not return a numeric.\n To convert between Task types, use makeCPOTargetOp.")
    }
    if (length(targettrans) != nrow(target)) {
      stop("trafo may not change length")
    }
    target[[1]] = targettrans
    target
  },
  cpo.invert = {
    if (predict.type == "se") {
      inlength = nrow(target)
      if (is.null(invert.se)) {
        if (is.null(invert.response)) {
          stop("cpoApplyFunRegrTarget: cannot predict 'se', since invert.response or invert.se must be non-NULL")
        }
        invert.response = augmentFun(invert.response, 1, param)
        invert.se = function(mu, sigma, param) {
          invertNormalMuSigma(invert.response, mu, sigma, gauss.points, vectorize)
        }
      } else {
        invert.se = augmentFun(invert.se, 2, param)
      }
      meancol = target[, 1, drop = TRUE]
      secol = target[, 2, drop = TRUE]
      if (!vectorize) {
        invert.se = vectorizeFun(invert.se, 2, "cpoApplyFunRegrTarget")
        target = t(invert.se(meancol, secol))
      } else {
        target = invert.se(meancol, secol)
        if (is.data.frame(target)) {
          if (!all(vlapply(target, is.numeric))) {
            stop("invert.se returned data.frame that had non-numeric columns")
          }
          target = as.matrix(target)
        }
      }
      if (!is.matrix(target) || ncol(target) != 2) {
        stop("invert.se returned data needs to have two columns")
      }
      if (!is.numeric(target)) {
        stop("cpoAppluFunRegrTarget invert.se did not return a numeric.")
      }
      if (nrow(target) != inlength) {
        stop("invert.se output had length different from input length.")
      }
    } else {
      assert(predict.type == "response")
      if (is.null(invert.response)) {
        stop("cpoApplyFunRegrTarget cannot predict 'response', since invert.response was NULL")
      }
      inlength = length(target)

      invert.response = augmentFun(invert.response, 1, param)
      if (!vectorize) {
        invert.response = vectorizeFun(invert.response, 1, "cpoApplyFunRegrTarget")
      }
      target = invert.response(target)
      if (!is.numeric(target)) {
        stop("cpoAppluFunRegrTarget invert.response did not return a numeric.")
      }
      if (length(target) != inlength) {
        stop("invert.response output length different from input length.")
      }
    }
    target
  })

# check function and possibly curry 'param'
#
# If 'fun' takes fewer than minparam params, throw an error.
# If it takes more, or has "...", return a function that
# automatically calls 'fun' with 'param'.
#
# @param fun [function] the function to check / augment
# @param minparams [numeric(1)] min number of params
# @param param [any] the param to augment the function by.
augmentFun = function(fun, minparams, param) {
  force(fun)
  fun.name = as.character(substitute(fun))
  if (is.primitive(fun)) {
    # formals() works different on primitive functions like e.g. 'as.character'
    forms = formals(args(fun))
  } else {
    forms = formals(fun)
  }
  hasdots = "..." %in% names(forms)
  if (!length(forms) || (length(forms) < minparams && !hasdots)) {
    stopf("%s must take at least %s arguments", fun.name, minparams)
  }
  if (hasdots || length(forms) > minparams) {
    function(...) fun(..., param)
  } else {
    fun
  }
}

# sapply the fun to column(s), check the return
#
# Return a function that vectorizes over any number of columns
# and returns a data.frame with `numreturns` return values.
vectorizeFun = function(fun, numreturns, cponame) {
  force(fun)
  fun.name = as.character(substitute(fun))
  function(...) {
    mapply(function(...) {
      ret = fun(...)
      if (length(ret) != numreturns) {
        stopf("%s '%s' did not return a result with length %s", cponame, fun.name, numreturns)
      }
      if (!is.atomic(ret)) {
        stopf("%s '%s' did not return values that simplified to an atomic vector.", cponame, fun.name)
      }
      ret
    }, ...)
  }
}

#' @title Log-Transform a Regression Target Variable.
#'
#' @template cpo_doc_intro
#'
#' @description
#' Log-transforms the regression \code{\link[mlr:Task]{Task}}'s target variable.
#'
#' If \code{predict.type} is \dQuote{response} for inversion, the model's prediction is
#' exponentiated.
#'
#' If \code{predict.type} = \dQuote{se} prediction is performed, the model's prediction
#' is taken as the parameters of a lognormal random variable; the inverted prediction is then
#' \code{mean = exp(mean + se^2 / 2)}, \code{se = sqrt((exp(se^2) - 1) * exp(2 * mean + se^2))}.
#'
#' It is therefore recommended to use \dQuote{se} prediction, possibly with the help of
#' \code{\link{cpoResponseFromSE}}.
#'
#' @template cpo_doc_outro
#' @export
cpoLogTrafoRegr = function(id) {
  cpo = cpoApplyFunRegrTarget(trafo = log, invert.response = exp,
    invert.se = function(mean, se) { cbind(mean = exp(mean + se^2 / 2), se = sqrt((exp(se^2) - 1) * exp(2 * mean + se^2))) },
    param = exp(1),
    export = "export.none")
  if (!missing(id)) {
    cpo = setCPOId(id)
  }
  cpo
}
cpoLogTrafoRegr = wrapFauxCPOConstructor(cpoLogTrafoRegr)  # nolint



registerCPO(cpoApplyFun(fun = identity), "data", "general data preprocessing", "Apply an arbitrary function column-wise.")
registerCPO(cpoApplyFunRegrTarget(trafo = identity), "target", "general target transformation", "Apply an arbitrary function to a regression target.")
registerCPO(cpoLogTrafoRegr(), "target", "target transformation", "Log-transform a regression target.")

