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
deparseJoin = function(what, sep = " ") {
  collapse(deparse(what, 500), sep = sep)
}

# Search for references to variables (not function) named in 'pattern'
# return TRUE if any were found, FALE otherwise
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
requireCPOPackages = function(cpo) {
  requirePackages(cpo$packages, why = stri_paste("CPO", cpo$name, sep = " "), default.method = "load")
}

# BBmisc::coalesce is dangerous, instead this sensible alternative is used
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

# check global flag whether to strictly check property compliance
isPropertyStrict = function() {
  TRUE
}

# get an option set for mlr. E.g. whether hyperparameter bounds checking is on or off.
getMlrOption = function(name, default = NULL) {
  getOption(stri_paste("mlr.", name), default)
}

# check whether the task has weights
hasTaskWeights = function(task) {
  !is.null(task$weights)
}
