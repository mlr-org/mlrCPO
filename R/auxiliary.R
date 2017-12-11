
#################################
# syntactic sugar               #
#################################

# increment by
#
# This is the C `+=` operator
`%+=%` = function(t, s) eval.parent(substitute({t = t + s}))

# decrement by
#
# This is the C `-=` operator
`%-=%` = function(t, m) eval.parent(substitute({t = t - m}))

# append
#
# X %c=% Y --> X = c(X, Y)
`%c=%` = function(t, a) eval.parent(substitute({t = c(t, a)}))

# union
#
# X %union=% Y --> X = union(X, Y)
`%union=%` = function(t, a) eval.parent(substitute({t = union(t, a)}))

# multireturn
#
# list(var1 = b, var2 = a, var3 = a + b, a) %<=% list(a = 1, b = 2, c = 3)
# -->
# var1 = 2 (b)
# var2 = 1 (a)
# var3 = 3 (a + b)
# a = 1 (a)
`%<=%` = function(a, b) {
  inexp = substitute(a)
  for (i in seq_len(length(inexp) - 1) + 1) {
    val = eval(inexp[[i]], b, enclos = parent.frame())
    assigntochr = names(inexp)[i]
    if (is.null(assigntochr) || assigntochr == "") {
      assignto = inexp[[i]]
    } else {
      assignto = asQuoted(assigntochr)
    }
    assignment = substitute({t = quote(v)}, list(t = assignto, v = val))
    eval.parent(assignment)
  }
}



#################################
# Printing                      #
#################################

# deparseJoin: deparse, but work with longer than 500 char expressions, mostly.
# Note that this is a heuristic for user messages only, the result can not be
# parsed again!
# @param what [language] expression to deparse
# @param sep [character(1)] separator between "lines" when deparse is longer than 500 chars and would otherwise be split
# @return [character(1)] string form of expression
deparseJoin = function(what, sep = " ") {
  collapse(deparse(what, 500), sep = sep)
}

# for pretty printing named vectors
# will not work for contents of `vec` that are longer than 60 characters.
namedVecToString = function(vec) {
  paste0("c(", collapse(paste(names(vec), vcapply(vec, deparse, control = NULL), sep = " = "), sep = ", "), ")")
}


#################################
# AST Manipulation              #
#################################

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
    if (identical(expr[[1]], quote(`function`))) {
      # variables are shadowed inside function definition
      pattern = setdiff(pattern, names(expr[[2]]))
    }
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

# Manipulate requirement expressions: rename all variables that are not function calls from
# one name to another.
#
# This is important e.g. when changing requirement expressions. and a parameter in the paramset
# is named e.g. 'c', because then c(1, 2, 3) would break. Therefore we
# go through the expressions and change only those requirements that
# are not function calls.
# @param expr [language] the expression to manipulate
# @param translate [list] named list of variable names to change
# @return [language] the expression `expr`, with the variables renamed according to `translate`.
renameNonfunctionNames = function(expr, translate) {
  startfrom = 1
  if (is.call(expr)) {
    if (!is.recursive(expr)) {
      return(expr)
    }
    startfrom = 2
    if (is.recursive(expr[[1]])) {
      startfrom = 1
    } else if (length(expr) == 1) {
      return(expr)
    }
  }
  if (is.recursive(expr)) {
    for (idx in seq(startfrom, length(expr))) {
      expr[[idx]] = renameNonfunctionNames(expr[[idx]], translate)
    }
  } else if (is.symbol(expr) && as.character(expr) %in% names(translate)) {
    expr = as.symbol(translate[[as.character(expr)]])
  }
  return(expr)
}

#################################
# Other                         #
#################################

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

# check global flag whether to enforce property compliance when combining CPOs or
# checking CPO input or output data.
# @return [logical(1)] whether to enforce property compliance
isPropertyStrict = function() {
  getMlrOption("cpo.property.strict", TRUE)
}
