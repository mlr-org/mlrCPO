#' @title CPO: Composable Preprocessing Operators
#'
#' @description
#' FIXME to come
#'
#' @family CPO
#' @name CPO
NULL

##################################
### Auxiliaries                ###
##################################

# deparseJoin: deparse, but work with longer than 500 char expressions, mostly.
# Note that this is a heuristic for user messages only, the result can not be
# parsed again!
# @param what [language] expression to deparse
# @param sep [character(1)] separator between "lines" when deparse is longer than 500 chars and would otherwise be split
# @return [character(1)] string form of expression
deparseJoin = function(what, sep = " ") {
  collapse(deparse(what, 500), sep = sep)
}

# Search for references to variables (not function) named in 'pattern'
# return TRUE if any were found, FALE otherwise.
# This is necessary since the R namespace for functions is different from
# the namespace for values. This is why
# > c = 1
# > c(c, c)
# works as expected.
# @param expr [language] the expression to check for occurrence of 'pattern'
# @param pattern [character] one or multiple variable names to look for
# @return [logical(1)] whether 'pattern' was found
referencesNonfunctionNames = function(expr, pattern) {
  startfrom = 1
  if (is.call(expr)) {
    if (!is.recursive(expr)) {
      return(FALSE)
    }
    startfrom = 2
    if (is.recursive(expr[[1]])) {
      startfrom = 1
    } else if (length(expr) == 1) {
      return(FALSE)
    }
  }
  if (is.recursive(expr)) {
    for (idx in seq(startfrom, length(expr))) {
      if (referencesNonfunctionNames(expr[[idx]], pattern)) {
        return(TRUE)
      }
    }
  } else if (is.symbol(expr) && as.character(expr) %in% pattern) {
    return(TRUE)
  }
  return(FALSE)
}

# use BBmisc's "requirePackages" for the package(s) listed by the CPO as required.
# @param cpo [CPOPrimitive] the CPO of which the package to load
# @return NULL
requireCPOPackages = function(cpo) {
  requirePackages(cpo$packages, why = stri_paste("CPO", cpo$name, sep = " "), default.method = "load")
}

# BBmisc::coalesce is dangerous, instead this sensible alternative is used
# @param ... [any]
# @return the first entry of `...` that doesn't evaluate to NULL, or NULL
firstNonNull = function(...) {
  dots = match.call(expand.dots = FALSE)$...
  for (arg in dots) {
    val = eval.parent(arg)
    if (!is.null(val)) {
      return(val)
    }
  }
  NULL
}

# check global flag whether to enforce property compliance when combining CPOs or
# checking CPO input or output data.
# @return [logical(1)] whether to enforce property compliance
isPropertyStrict = function() {
  getMlrOption("cpo.property.strict", TRUE)
}

# get an option set for mlr. E.g. whether hyperparameter bounds checking is on or off.
# This re-implements mlr:::getMlrOption
# @param name [charcter(1)] name of the option, within the "mlr" namespace
# @param default [any] default value to return when option was not found
# @return [any] the value of the option, or `default` if the option is not set
getMlrOption = function(name, default = NULL) {
  getOption(stri_paste("mlr.", name), default)
}

# check whether the task has weights
# @param task [Task] the task to check
# @return [logical(1)] whether the task has weights
hasTaskWeights = function(task) {
  !is.null(task$weights)
}
