
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
# Gauss-Hermite Quadrature      #
#################################


# Get the roots of the n'th order probabilist's Hermite polynomial
#
# Uses eigenvalues of the Jacobi matrix of which the (physicist's) Hermite polynomial is the characteristic
# polynomial.
#
# The function is wrapped in an environment for memoization
# @param n [numeric(1)] degree of Hermite polynomial, must be greater or equal 3.
# @return [numeric] positions of hermite polynomial roots
hermiteRoots = (function() {  # nolint
  ev = environment()
  function(n) {
    nname = as.character(n)
    ret = get0(nname, ev, inherits = FALSE, ifnotfound = NULL)
    if (!is.null(ret)) {
      return(ret)
    }
    ltrig = cbind(rbind(0, diag(sqrt(seq_len(n - 1) / 2))), 0)
    ret = eigen(ltrig, symmetric = TRUE, only.values = TRUE)$values * sqrt(2)
    assign(nname, ret, ev)
    ret
  }
})()

# Get the coefficients of the n'th order probabilist's Hermite polynomial
#
# Uses the recurrence relation H[n+1] = x H[n] - n H[n-1]
#
# Gives only the even or odd terms (since the others are 0)
# @param n [numeric(1)] degree of Hermite polynomial, must be greater or equal 3.
# @return [numeric] coefficients of Hermite polynomial
hermitePoly = function(n) {
  p0 = 1
  p1 = 1
  l = 2
  index = n + 1
  while (l < index) {
    p2 = c(0, p1) - c((l - 1) * p0, 0)
    p3 = p2 - c(l * p1, 0)
    l = l + 2
    p0 = p2
    p1 = p3
  }
  if (l == index) p3 else p2
}

# Evaluate the Hermite Polynomial of order n at position x
#
# Uses the coefficients generated by `hermitePoly`, respecting the fact
# that either only even or only odd coefficients are given (and the others are 0).
# @param n [numeric(1)] degree of Hermite polynomial, must be greater or equal 3.
# @param x [numeric(1)] position to evaluate
# @return [numeric(1)] the value of the Hermite polynomial at position `x`
evalHermitePoly = function(n, x) {
  hp = hermitePoly(n)
  offset = 2 - n %% 2
  sapply(x, function(x) sum(hp * x^(seq_along(hp) * 2 - offset)))  # nolint
}

# Get the Gauss-Hermite quadrature weights for order n
#
# The quadrature weights are `a_n/a_{n-1} <H_{n-1}, H_{n-1}> / (H_n' H_{n-1})`
# Where `a_n` is the highest order coefficient of the n'th polynomial (`1`, in this case),
# `<., .>` is the inner product, H_n' is the derivative of H_n.
#
# The inner product of <H_n, H_n> is `factorial(n)`, the derivative is given by the recurrence relation
# H_n' = n H_{n-1}.
#
# @param n [numeric(1)] degree of Hermite polynomial, must be greater or equal 3.
# @return [numeric] vector of length n of summation weights.
hermiteWeights = (function() {  # nolint
  ev = environment()
  function(n) {
    nname = as.character(n)
    ret = get0(nname, ev, inherits = FALSE, ifnotfound = NULL)
    if (!is.null(ret)) {
      return(ret)
    }
    roots = hermiteRoots(n)
    hvals = evalHermitePoly(n - 1, roots)
    ret = factorial(n - 1) / (n * hvals^2)
    assign(nname, ret, ev)
    ret
  }
})()

# Evaluate the expected value of a function when given a normally distributed argument
#
# @param fun [function] Function taking one numeric argument, returning a single numeric value
# @param n [numeric(1)] degree of Hermite polynomials to use for integration, must be greater or equal 3.
# @param mu [numeric(1)] mean of normal distribution
# @param sigma [numeric(1)] standard deviation of normal distribution.
# @return [numeric(1)] the expected value.
normExpVal = function(fun, n, mu = 0, sigma = 1) {
  roots = hermiteRoots(n)
  fvals = vnapply(roots, function(x) fun(x * sigma + mu))

  sum(fvals * hermiteWeights(n))
}

# Get Mean and Standard Deviation of a the function applied to a normally distributed value
#
# @param fun [function] Function taking one numeric argument, returning a single numeric value
# @param mu [numeric] mean of normal distribution, may be a vector of multiple values if `as.mat == TRUE`.
# @param sigma [numeric] standard deviation of normal distribution, must have the same length as `mu`.
# @param n [numeric(1)] degree of Hermite polynomials to use for integration, must be greater or equal 3.
# @param as.mat [logical(1)] whether to vectorize over `mu`, `sigma` and return a matrix. Otherwise, a numeric(2) is returned.
# @return [matrix | numeric(2)] If `as.mat == TRUE`, a matrix with two columns "response", "se", otherwise a numeric(2) named "response", "se", containing the transformed expected value and standard deviation.
invertNormalMuSigma = function(fun, mu, sigma, n = 23, as.mat = FALSE) {
  if (as.mat) {
    muinv = mapply(normExpVal, mu = mu, sigma = sigma, MoreArgs = list(fun = fun, n = n))
    seinv = sqrt(mapply(function(muinv, mu, sigma) normExpVal(function(x) (fun(x) - muinv)^2, n, mu, sigma),
      muinv = muinv, mu = mu, sigma = sigma))
    cbind(response = muinv, se = seinv)
  } else {
    muinv = normExpVal(fun, n, mu, sigma)
    seinv = sqrt(normExpVal(function(x) (fun(x) - muinv)^2, n, mu, sigma))
    c(response = muinv, se = seinv)
  }
}


#################################
# Other                         #
#################################

# use BBmisc's "requirePackages" for the package(s) listed by the CPO as required.
# @param cpo [CPOPrimitive] the CPO of which the package to load
# @return NULL
requireCPOPackages = function(cpo) {
  tryCatch({
    requirePackages(cpo$packages, why = stri_paste("CPO", cpo$name, sep = " "), default.method = "load")
  }, error = function(e) {
    if (dynGet("ISTESTING", ifnotfound = FALSE, minframe = 0)) {
      # if this happens in a test on CRAN, we forgive the absence of the package and just skip the test
      testthat::skip_on_cran()
    }
    stop(e)
  })
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


#' @title Add 'covr' coverage to CPOs
#'
#' @description
#' Use this if you want to check code coverage of \code{\link{CPO}}s using
#' \code{covr}. The functions inside \code{\link{CPO}}s is
#' originally not accessible to \code{covr}, so \code{covrTraceCPOs} needs to be called in
#' the \code{.onAttach} function. Note that putting it in \code{.onLoad} will \bold{not} work.
#'
#' Currently, for this to work, the mb706 fork of \code{covr} needs to be used. To install it,
#' call
#'
#' \code{devtools::install_github("mb706/covr")}
#'
#' To use it on Travis CI, add the line \code{- mb706/covr} under the \code{r_github_packages:}
#' category.
#'
#' This function comes at no runtime penalty: If the \code{R_COVR} environment variable is not
#' set to \dQuote{true}, then it only has an effect if \code{force} is \code{TRUE}.
#'
#' @param env [environment]\cr
#'   The environment to search for \code{\link{CPO}}s. Default is \code{parent.env(parent.frame())},
#'   which is the package namespace if called from \code{.onLoad}.
#' @param force [logical(1)]\cr
#'   Trace \code{\link{CPO}} functions even when \code{R_COVR} is not \dQuote{true}.
#'   Default is \code{FALSE}.
#' @return [invisible(NULL)].
#' @family helper functions
#' @export
covrTraceCPOs = function(env = parent.env(parent.frame()), force = FALSE) {
  assertFlag(force)
  if (!identical(Sys.getenv("R_COVR"), "true") && !force) {
    return(invisible(NULL))
  }

  trace_calls = get("trace_calls", mode = "function", envir = getNamespace("covr"))  # nolint

  fnames = c("cpo.trafo", "cpo.retrafo", "cpo.train.invert", "cpo.invert")

  cnames = Filter(function(x) "CPOConstructor" %in% class(get(x, envir = env)),
    ls(env, all.names = TRUE))

  for (objname in cnames) {
    catf("Tracing cpo %s", objname)
    cpo = get(objname, envir = env)
    cpo.name = environment(cpo)$cpo.name
    cenv = environment(cpo)
    if ("FauxCPOConstructor" %in% class(cpo)) {
      cenv$constructorconstructor = trace_calls(get("constructorconstructor", envir = cenv),
        paste0(cpo.name, "$constructor"))
      next
    }
    for (ifunname in c("cpo.trafo", "cpo.retrafo", "cpo.invert")) {
      ifun = cenv$trafo.funs[[ifunname]]
      if (is.null(ifun)) {
        next
      }
      ifenv = environment(ifun)
      for (fname in fnames) {
        if (!is.null({oldfun = ifenv[[fname]]})) {
          catf("interface fun %s environment %s is being replaced", ifunname, fname)
          ifenv[[fname]] = trace_calls(oldfun, paste0(cpo.name))
        }
      }
    }
  }
}
