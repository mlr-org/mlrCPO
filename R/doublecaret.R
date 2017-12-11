# doublecaret.R -- the %>>% ("double caret") operator.

##################################
### %>>% Operator              ###
##################################

#' @title CPO Composition / Attachment / Application Operator
#'
#' @description
#' This operator \dQuote{pipes} data from the source into the sink object.
#'
#' If both objects are a \code{\link{CPO}} object, or both are a \code{\link{CPOTrained}} object,
#' they will be composed. A new object, representing the operation of performing both object's operations in succession,
#' will be created, which can be handled like a new CPO or CPOTrained object. See \code{\link{composeCPO}}.
#'
#' If the source object is a \code{\link[base]{data.frame}} or a \code{link[mlr]{Task}}, the
#' transformation operation will be applied to this data, and the same resulting
#' data will be returned. See \code{\link{applyCPO}}.
#'
#' If the sink object is a \code{\link[mlr:makeLearner]{Learner}}, the CPO will be attached to
#' this learner. The same operation will be performed during the \code{\link[mlr]{train}} and
#' \code{\link[stats]{predict}} phase; the behaviour during the predict phase may furthermore
#' be depend on the training data. See \code{\link{attachCPO}}.
#'
#' Note that you can not link a \code{data.frame} or \code{\link[mlr]{Task}} directly
#' to a \code{\link[mlr:makeLearner]{Learner}}, since this operation is not algebraically associative
#' with the composition of CPOs. Use \code{\link[mlr]{train}} for this.
#'
#' The \code{\%<<\%} operator is synonymous with \code{\%>>\%} with source and sink argument swapped.
#'
#' The \code{\%>|\%} and \code{\%|<\%} operators perform piping followed by application of \code{\link{retrafo}}.
#' The \code{\%>|\%} evaluates the expression to its right before the expression to its left, so it may be
#' used in the most natural way without parentheses:
#'
#' \code{data \%>|\% cpo1 \%>>\% cpo2}
#'
#' is the same as
#'
#' \code{retrafo(data \%>>\% cpo1 \%>>\% cpo2)}.
#'
#'
#' The \code{\%<>>\%} and \code{\%<<<\%} operators perform the piping operation and assign the result
#' to the left hand variable. This way it is possible to apply a \code{\link{CPO}}, or to
#' attach a \code{\link{CPO}} to a \code{\link[mlr:makeLearner]{Learner}}, and just keep the resulting
#' object. The assignment operators evaluate their right hand side before their left hand side, so
#' it is possible to build long chains that end up writing to the leftmost variable. Therefore the expression
#'
#' \code{data \%<>>\% cpo1 \%<>>\% cpo2 \%>>\% cpo3}
#'
#' is the same as
#'
#' \preformatted{cpo1 = cpo1 \%>>\% cpo2 \%>>\% cpo3
#' data = data \%>>\% cpo1}
#'
#' @param cpo1 [\code{\link[base]{data.frame}} | \code{\link[mlr]{Task}} | \code{\link{CPO}} | \code{\link{CPOTrained}}]\cr
#'   The source object.
#' @param cpo2 [\code{\link{CPO}} | \code{\link{CPOTrained}} | \code{\link[mlr:makeLearner]{Learner}}]\cr
#'   The sink object.
#' @return [\code{\link[base]{data.frame}} | \code{\link[mlr]{Task}} | \code{\link{CPO}} | \code{\link{CPOTrained}}].
#'
#' @family operators
#' @family retrafo related
#' @family inverter related
#' @family CPO lifecycle related
#'
#' @examples
#' # PCA-rotate pid.task
#' rotated.pid.task = pid.task %>>% cpoScale() %>>% cpoPca()
#'
#' # Centering / Scaling *after* PCA
#' newPCA = cpoPca() %>>% cpoScale()
#'
#' # Attach the above to learner
#' pcaLogreg = newPCA %>>% makeLearner("classif.logreg")
#'
#' # append cpoAsNumeric to newPCA
#' newPCA %<>>% cpoAsNumeric()
#' print(newPCA)
#'
#' # prepend cpoAsNumeric to pcaLogreg
#' pcaLogreg %<<<% cpoAsNumeric()
#'
#' @export
`%>>%` = function(cpo1, cpo2) {
  # deferAssignmentOperator call so that %<>>% etc. is evaluated last.
  # it rewrites `%>>%` etc. operators to `internal%>>%` functions and
  # calls the rewritten statement.
  return(eval.parent(deferAssignmentOperator(substitute(cpo1 %>>% cpo2))))
  UseMethod("%>>%")  # for roxygen2 to recognize %>>% as an S3 generic
}

# %>>% is rewritten to internal%>>%
#' @title Internally Used \code{\%>>\%} Operators
#'
#' @description
#' These functions are used internally as replacements of the
#' \code{\link{\%>>\%}} operators. This replacement is necessary
#' to enable right-associativity of some operators.
#' @inheritParams %>>%
#' @return [\code{\link[base]{data.frame}} | \code{\link[mlr]{Task}} | \code{\link{CPO}} | \code{\link{CPOTrained}}].
#' @keywords internal
#' @export
`internal%>>%` = function(cpo1, cpo2) {
  UseMethod("%>>%")
}

#' @rdname grapes-greater-than-greater-than-grapes
#' @export
`%<<%` = function(cpo2, cpo1) {
  eval.parent(deferAssignmentOperator(substitute(cpo2 %<<% cpo1)))
}

# %<<% is rewritten to internal%<<%
#' @rdname internal-grapes-greater-than-greater-than-grapes
#' @export
`internal%<<%` = function(cpo2, cpo1) {
  `internal%>>%`(cpo1, cpo2)
}

#' @rdname grapes-greater-than-greater-than-grapes
#' @export
`%<>>%` = function(cpo1, cpo2) {
  eval.parent(deferAssignmentOperator(substitute(cpo1 %<>>% cpo2)))
}

# %<>>% is rewritten to internal%<>>%
#' @rdname internal-grapes-greater-than-greater-than-grapes
#' @export
`internal%<>>%` = function(cpo1, cpo2) {
  if (identical(substitute(cpo1), quote(NULLCPO))) {
    stop("Cowardly refusing to assign to NULLCPO")
  }
  eval.parent(substitute({ cpo1 = `internal%>>%`(cpo1, cpo2) }))
}

#' @rdname grapes-greater-than-greater-than-grapes
#' @export
`%<<<%` = function(cpo2, cpo1) {
  eval.parent(deferAssignmentOperator(substitute(cpo2 %<<<% cpo1)))
}

# %<<<% is rewritten to internal%<<<%
#' @rdname internal-grapes-greater-than-greater-than-grapes
#' @export
`internal%<<<%` = function(cpo2, cpo1) {
  if (identical(substitute(cpo2), quote(NULLCPO))) {
    stop("Cowardly refusing to assign to NULLCPO")
  }
  eval.parent(substitute({ cpo2 = `internal%>>%`(cpo1, cpo2) }))
}

#' @rdname grapes-greater-than-greater-than-grapes
#' @export
`%>|%` = function(cpo1, cpo2) {
  eval.parent(deferAssignmentOperator(substitute(cpo1 %>|% cpo2)))
}

# %>|% is rewritten to internal%>|%
#' @rdname internal-grapes-greater-than-greater-than-grapes
#' @export
`internal%>|%` = function(cpo1, cpo2) {
  retrafo(`internal%>>%`(cpo1, cpo2))
}

#' @rdname grapes-greater-than-greater-than-grapes
#' @export
`%|<%` = function(cpo2, cpo1) {
  eval.parent(deferAssignmentOperator(substitute(cpo2 %|<% cpo1)))
}

# %|<% is rewritten to internal%|<%
#' @rdname internal-grapes-greater-than-greater-than-grapes
#' @export
`internal%|<%` = function(cpo2, cpo1) {
  retrafo(`internal%>>%`(cpo1, cpo2))
}

#' @export
`%>>%.default` = function(cpo1, cpo2) {
  stopf("%%>>%% not defined for objects of class c(%s)", paste0('"', class(cpo1), '"', collapse = ", "))
}

#' @export
`%>>%.data.frame` = function(cpo1, cpo2) {
  res = `%>>%.Task`(cpo1, cpo2)
  if ("Task" %in% class(res)) {
    message("data.frame was converted to Task by Target Operating CPO.")
  }
  res
}

#' @export
`%>>%.Task` = function(cpo1, cpo2) {
  if (is.nullcpo(cpo2)) {
    cpo1
  } else if ("Learner" %in% class(cpo2)) {
    stopf("%s\n%s\n%s\n%s",
      "Cannot pipe data into learner!",
      "If you called 'data %>>% preproc %>>% learner', you probably meant",
      "train(preproc %>>% learner, data). Note that this is different from",
      "'train(learner, data %>>% preproc), which is usually not what you want.")
  } else if (any(c("CPOTrained", "CPO") %in% class(cpo2))) {
    applyCPO(cpo2, cpo1)
  } else if ("CPOConstructor" %in% class(cpo2)) {
    stop("Cannot compose CPO Constructors.\nDid you forget to construct the CPO?")
  } else {
    stopf("Cannot compose data with object of class c(%s)", paste0('"', class(cpo2), '"', collapse = ", "))
  }
}

#' @export
`%>>%.CPOConstructor` = function(cpo1, cpo2) {
  stop("Cannot compose CPO Constructors.\nDid you forget to construct the CPO?")
}

#' @export
`%>>%.CPO` = function(cpo1, cpo2) {
  if (is.nullcpo(cpo2)) {
    cpo1
  } else if ("CPO" %in% class(cpo2)) {
    # compose two CPOs
    composeCPO(cpo1, cpo2)
  } else if ("Learner" %in% class(cpo2)) {
    # wrap around learner
    attachCPO(cpo1, cpo2)
  } else if ("CPOConstructor" %in% class(cpo2)) {
    stop("Cannot compose CPO Constructors.\nDid you forget to construct the CPO?")
  } else {
    stopf("Cannot compose CPO with object of class c(%s)", paste0('"', class(cpo2), '"', collapse = ", "))
  }
}

#' @export
`%>>%.CPOTrained` = function(cpo1, cpo2) {
  if (is.nullcpo(cpo2)) {
    cpo1
  } else if ("CPOTrained" %in% class(cpo2)) {
    composeCPO(cpo1, cpo2)
  } else if ("WrappedModel" %in% class(cpo2)) {
    stop("Attaching CPO Retrafo to a model is not implemented.")
  } else if ("CPO" %in% class(cpo2) || "CPOConstructor" %in% class(cpo2)) {
    stop("Cannot compose CPO Retrafo with CPO.")
  } else {
    stopf("Cannot compose CPO Retrafo with object of class c(%s)", paste0('"', class(cpo2), '"', collapse = ", "))
  }
}

# Rewrite all %**% operators to the right of the leftmost %<**% (assignment) operator
#
# Recursively collect the right hand side of each found operator.
# As soon as an assignment operator is found, a
# list(internal_operator, left_hand_side, right_hand_side)
# is returned.
#
# right_hand_side is a newly created syntax tree that corresponds
# to everything to the right of the %<>>% operator (as long as its precedence is equal
# or above %%-operator precedence).
#
# left_hand_side is just the first operand of %<>>%, since that
# *already* corresponds to everything of equal or above precedence to the left
# of the operator.
#
# Some variables are defined outside of this function for performance, otherwise
# the quotes are parsed again on every recursive iteration.
#
# @param expr [language] the expression to parse
# @return [list]. list(operator, lhs, rhs)
defas.assignment.ops = list(quote(`%<>>%`), quote(`%<<<%`), quote(`%>|%`))
defas.assignment.repl = list(quote(`internal%<>>%`), quote(`internal%<<<%`), quote(`internal%>|%`))
defas.deferred.ops = list(quote(`%>>%`), quote(`%<<%`), quote(`%|<%`))
defas.deferred.repl = list(quote(`internal%>>%`), quote(`internal%<<%`), quote(`internal%|<%`))
defas.triolist = list(NULL, NULL, NULL)
defas.recurse.rewrite = function(expr) {
  ret = defas.triolist
  if (!is.call(expr)) {
    ret[[3]] = expr
    return(ret)
  }
  for (i in seq_along(defas.deferred.ops)) {
    if (identical(expr[[1]], defas.deferred.ops[[i]])) {
      # %>>%
      ret = defas.recurse.rewrite(expr[[2]])

      expr[[1]] = defas.deferred.repl[[i]]
      expr[[2]] = ret[[3]]
      ret[[3]] = expr
      return(ret)
    }
  }
  for (i in seq_along(defas.assignment.ops)) {
    if (identical(expr[[1]], defas.assignment.ops[[i]])) {
      ret[[1]] = defas.assignment.repl[[i]]
      ret[[2]] = expr[[2]]
      ret[[3]] = expr[[3]]
      return(ret)
    }
  }
  ret[[3]] = expr
  ret
}

# Rewrite expr to make %<>>% and %>>>% right-associative
#
# This iterates through expr, rewrites %**% to internal%**%,
# and makes %<>>% assignment operators evaluate their right hand
# arguments first by rearranging the syntax tree.
#
# defas.recurse.rewrite is iteratively called until no more
# %<>>% assignment operators are found.
#
# @param expr [language] the expression to rearrange / rewrite
# @return [language]. The translated expression, which can be
#   eval'd.
deferAssignmentOperator = function(expr) {
  prev.split = defas.recurse.rewrite(expr)
  while (!is.null(prev.split[[1]])) {
    split = defas.recurse.rewrite(prev.split[[2]])
    prev.split[[2]] = split[[3]]
    split[[3]] = as.call(prev.split)
    prev.split = split
  }
  as.call(prev.split[[3]])
}


