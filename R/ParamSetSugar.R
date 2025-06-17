#' defined to avoid problems with the static type checker
#' @export
discrete = list

#' defined to avoid problems with the static type checker
#' @export
funct = list

#' defined to avoid problems with the static type checker
#' @export
untyped = list


#' @title Turn the argument list into a \code{ParamSet}
#'
#' @description
#'
#' \code{pSS}, short for \dQuote{ParamSet Sugar}, is a shorthand API for \code{\link[ParamHelpers]{makeParamSet}}
#' which enables entry of \code{\link[ParamHelpers]{ParamSet}}s in short form. It behaves similarly to
#' \code{\link[ParamHelpers]{makeParamSet}}, but instead of having to construct each parameter individually,
#' the parameters can be given in shorthand form with a convenient syntax, making use of R's
#' nonstandard evaluation.
#'
#' This makes definition of \code{\link[ParamHelpers]{ParamSet}}s shorter and more readable.
#'
#' The difference between \code{pSS} and \code{pSSLrn} is only in the default value of \code{.pss.learner.params}
#' being \code{FALSE} for the former and \code{TRUE} for the latter.
#'
#' @section Details:
#' The arguments are of the form
#'
#' \code{name = default: type range [^ dimension] [settings]}.
#'
#' \bold{\code{name}} is any valid R identifier name.
#'
#' \bold{\code{= default}} Determines the 'default' setting
#' in \code{makeXXXParam}. Note that this is different from an R function parameter
#' default value, in that it serves only as information to the user and does not set the
#' parameter to this value if it is not given. To define \sQuote{no default}, use \code{NA} or
#' leave the \dQuote{= default} part out. Leaving it out can cause problems when R's static
#' type checker verifies a package, so this is \emph{only} recommended for interactive sessions
#' and top-level applications. (To actually set a parameter default to NA, use \code{(NA)} in parentheses)
#'
#' \bold{\code{type}} is one of
#' \dQuote{integer}, \dQuote{numeric}, \dQuote{logical}, \dQuote{discrete}, \dQuote{funct}, \dQuote{character}, \dQuote{untyped}.
#' Each of these types leads to a \code{\link[ParamHelpers]{Param}} or \code{\link[ParamHelpers]{LearnerParam}} of the given type to be created.
#' Note that \dQuote{character} is not available if \sQuote{Learner}-parameters are created.
#'
#' \bold{\code{range}} is optional and only used for \emph{integer}, \emph{numeric}, and \emph{discrete} parameters.
#' For \dQuote{discrete}, it is either \code{[valuelist]} with \code{valuelist} evaluating to a list,
#' or of the form \code{[value1, value2, ...]}, creating a discrete parameter of character
#' or numeric values according to \code{value1},
#' \code{value2} etc. If \code{type} is one of \dQuote{integer} or \dQuote{numeric},
#' \code{range} is of the form \code{[lowBound, upBound]}, where \code{lowBound}
#' and \code{upBound} must either be numerical (or integer) values indicating the
#' lower and upper bound, or may be missing (indicating the absence of a bound). To indicate
#' an exclusive bound, prefix the values with a tilde (\dQuote{~}). For a \dQuote{numeric} variable, to
#' indicate an unbounded value which may not be infinite, you can use \code{~Inf} or \code{~-Inf},
#' or use tilde-dot (\dQuote{~.}).
#'
#' \bold{\code{^ dimension}} is optionally determining the dimension of a \sQuote{vector} parameter.
#' If it is absent, the result is a normal \code{\link[ParamHelpers]{Param}} or \code{\link[ParamHelpers]{LearnerParam}}, if it is present,
#' the result is a \code{Vector(Learner)Param}. Note that a one-dimensional \code{Vector(Learner)Param}
#' is distinct from a normal \code{(Learner)Param}.
#'
#' \bold{\code{settings}} may be a collection of further settings to supply to \code{makeXXXParam}
#' and is optional. To specify one or more settings, put in double square brackets (\code{[[}, \code{]]}),
#' and comma-separate settings if more than one is present.
#'
#' @param ... Parameters, see Details below.
#' @param .pss.learner.params [\code{logical}]\cr
#'   Whether to create \code{\link[ParamHelpers]{LearnerParam}} instead of \code{\link[ParamHelpers]{Param}} objects.
#'   Default is \code{TRUE} for \code{pSSLrn} and \code{FALSE} for \code{pSS}.
#' @param .pss.env [\code{environment}]\cr
#'   Which environment to use when evaluating expressions. Defaults to the calling
#'   function's frame.
#'
#' @examples
#' pSSLrn(a = NA: integer [~0, ]^2 [[requires = expression(b != 0)]],
#'        b = -10: numeric [~., 0],
#'        c: discrete [x, y, 1],
#'        d: logical,
#'        e: integer)
#'
#' # is equivalent to
#'
#' makeParamSet(
#'     makeIntegerVectorLearnerParam("a", len = 2, lower = 1,  # note exclusive bound
#'          upper = Inf, requires = expression(b != 0)),
#'     makeNumericLearnerParam("b", lower = -Inf, upper = 0,
#'          allow.inf = FALSE, default = -10),  # note infinite value is prohibited.
#'     makeDiscreteLearnerParam("c", values = list(x = "x", y = "y", `1` = 1)),
#'     makeLogicalLearnerParam("d"),
#'     makeIntegerLearnerParam("e"))
#'
#'
#' @export
pSS = function(..., .pss.learner.params = FALSE, .pss.env = parent.frame()) {
  promises = substitute(pSS(...))  # match.call doesn't work with indirect calls.
  promises[[1]] = NULL
  allparams = lapply(seq_along(promises), function(paridx) {
    thispar = promises[[paridx]]
    name = coalesce(names(promises)[paridx], "")
    parseSingleParameter(name, thispar, .pss.learner.params, .pss.env)
  })
  makeParamSet(params = allparams)
}

#' @rdname pSS
#' @export
pSSLrn = function(..., .pss.learner.params = TRUE, .pss.env = parent.frame()) {
  pSS(..., .pss.learner.params = .pss.learner.params, .pss.env = .pss.env)
}

### Auxiliary functions

# formerr: Give informational error about malformed parameter
formerr = function(pstring, pattern, ...) {
    stopf("Parameter '%s' must be of the form\n%s\n%s", pstring,
          "NAME = DEFAULT: TYPE [RANGE] [^ DIMENSION] [SETTINGS]",
          sprintf(pattern, ...))
}

# get the makeXXXParam function appropriate for the type and vector-ness
getConstructor = function(type, is.learner, is.vector) {
  normal.const = list(numeric = makeNumericParam,
                     integer = makeIntegerParam,
                     logical = makeLogicalParam,
                     discrete = makeDiscreteParam,
                     funct = makeFunctionParam,
                     character = makeCharacterParam,
                     untyped = makeUntypedParam)
  vector.const = list(numeric = makeNumericVectorParam,
                     integer = makeIntegerVectorParam,
                     logical = makeLogicalVectorParam,
                     discrete = makeDiscreteVectorParam,
                     character = makeCharacterVectorParam)
  normlrn.const = list(numeric = makeNumericLearnerParam,
                      integer = makeIntegerLearnerParam,
                      logical = makeLogicalLearnerParam,
                      discrete = makeDiscreteLearnerParam,
                      funct = makeFunctionLearnerParam,
#                      character = makeCharacterLearnerParam,
                      untyped = makeUntypedLearnerParam)
  vectlrn.const = list(numeric = makeNumericVectorLearnerParam,
                      integer = makeIntegerVectorLearnerParam,
                      logical = makeLogicalVectorLearnerParam,
                      discrete = makeDiscreteVectorLearnerParam)
#                      character = makeCharacterVectorLearnerParam,
    if (is.vector) {
    if (is.learner) {
      vectlrn.const[[type]]
    } else {
      vector.const[[type]]
    }
  } else {
    if (is.learner) {
      normlrn.const[[type]]
    } else {
      normal.const[[type]]
    }
  }
}


### parsing single parameter
# this function does the heavy lifting:
# it takes the name and expression of a given parameter and returns
# the constructed ParamSet.
# @param name [character(1)] the "name" of this item in the call object, this is the name of the parameter, if present.
# @param thispar [language] the rest of the expression in the call, to be parsed
# @param is.learner [logical(1)] whether to construct a LearnerParam
# @param pss.env [environment] the environment to use
# @return [Param | LearnerParam] the constructed parameter.
parseSingleParameter = function(name, thispar, is.learner, pss.env) {
  constructor.params = list()
  additional.settings = list()
  is.vector = FALSE
  pstring = deparseJoin(thispar)  # represent this parameter in warning / error messages
  if (name != "") {
    pstring = paste(name, "=", pstring)
  }

  if (length(thispar) < 3 || !identical(thispar[[1]], quote(`:`))) {
    formerr(pstring, "`:` was missing or at unexpected position.")
  }

  if (name == "") {  # no default, i.e. param is not of the form 'parname = default: type'
    constructor.params$id = as.character(thispar[[2]])
  } else {
    constructor.params$id = name
    if (!identical(thispar[[2]], NA)) {
      # the following uses single braces + list() to keep 'NULL' values
      constructor.params["default"] = list(eval(thispar[[2]], envir = pss.env))
    }
  }

  pdeco = thispar[[3]]  # pdeco: the part after the ':'

  # We now need to go up the table of operator precedences, see 'help(Syntax)'
  # We check the operators '^' (dimension), '[[' (additional settings), '[' (range)

  # ** dimension **
  if (is.recursive(pdeco) && identical(pdeco[[1]], quote(`^`))) {
    rl = parseDimension(pdeco, pstring, pss.env)
    pdeco = rl$pdeco  # replace pdeco with what's left after removing dimension part
    constructor.params$len = rl$len
    is.vector = TRUE
  }

  # ** settings **
  if (is.recursive(pdeco) && identical(pdeco[[1]], quote(`[[`))) {
    additional.settings = as.list(pdeco)
    additional.settings[[1]] = NULL  # delete `[[`
    additional.settings[[1]] = NULL  # delete part before `[[`
    additional.settings = lapply(additional.settings, function(x) eval(x, envir = pss.env))
    pdeco = pdeco[[2]]  # replace pdeco with what's left after removing settings part
  }

  # ** range (if any) **
  if (!is.recursive(pdeco)) {
    pdeco = list(pdeco)  # so we can access [[1]]
  }
  hasrange = identical(pdeco[[1]], quote(`[`))
  if (hasrange) {
    pdeco[[1]] = NULL
  }

  # from here on, pdeco[[1]] is the type, pdeco[[2]], ... is the range

  # ** actual parameter type **
  if (!is.name(pdeco[[1]])) {
    formerr(pstring, "Unknown parameter type %s", deparseJoin(pdeco[[1]]))
  }

  ptype = as.character(pdeco[[1]])

  if (is.null(getConstructor(ptype, is.learner, is.vector))) {
    if (ptype == "character") {
      formerr(pstring, "Parameter type 'character' not allowed for Learner params.")
    }
    formerr(pstring, "Unknown parameter type %s", ptype)
  }

  # ** check sanity of 'range' part **
  if (hasrange && ptype %nin% c("discrete", "numeric", "integer")) {
    formerr(pstring, "'%s' parameter may not have range parameter: %s", ptype, deparseJoin(pdeco))
  }
  if (!hasrange && length(pdeco) > 1) {
    formerr(pstring, "'%s' parameter with unexpected postfix: %s", pstring, deparseJoin(pdeco))
  }
  if (hasrange && length(pdeco) == 1) {
    formerr(pstring, "'%s' may not have empty range.", ptype)
  }

  # build the arguments to the make***Param constructor call in the 'constructor.params' variable
  if (ptype == "discrete") {
    if (!hasrange) {
      formerr(pstring, "discrete parameter must be of the form 'param: discrete[val1, val2]'\nRange suffix ([...] part) is missing.")
    }
    constructor.params$values = parseDiscrete(pdeco, pstring, pss.env)  # range is an enumeration of items, or a list
  }
  if (ptype %in% c("numeric", "integer")) {
    if (hasrange) {
      constructor.params = insert(constructor.params, parseNumeric(pdeco, ptype, pstring, pss.env)) # range is [lower, upper]
    } else if (ptype == "numeric") {
      constructor.params$allow.inf = TRUE
    }
  }
  constructor.params = insert(constructor.params, additional.settings)
  do.call(getConstructor(ptype, is.learner, is.vector), constructor.params, quote = TRUE)
}

### parameter parsing sub-functions

# parseDimension: parse the '^n' part indicating a vector
parseDimension = function(pdeco, pstring, pss.env) {
  if (is.recursive(pdeco[[3]]) && identical(pdeco[[3]][[1]], quote(`[[`))) { # settings
    exponent = pdeco[[3]][[2]]
    # remove '^..' part, but keep settings part
    new.pdeco = pdeco[[3]]
    new.pdeco[[2]] = pdeco[[2]]
    pdeco = new.pdeco
  } else {
    exponent = pdeco[[3]]
    pdeco = pdeco[[2]]  # remove '^...' part
  }
  len = eval(exponent, envir = pss.env)
  if (!is.atomic(len) || (!is.numeric(len) && !is.na(len))) {
    formerr(pstring, "`^` found, but exponent %s did not eval to numeric.", deparseJoin(exponent))
  }
  list(pdeco = pdeco, len = len)
}

# parseDiscrete: parse the range part of a discrete parameter
parseDiscrete = function(pdeco, pstring, pss.env) {
  if (length(pdeco) == 2) {
    # only one value given -> interpret it as expression that gives the values list
    return(eval(pdeco[[2]], envir = pss.env))
  } else if (all(names(pdeco) == "")) {
    # succession of names of the kind (a, b, c, 1, 2, 3) -> turn it into a list
    vallist = as.list(pdeco)
    vallist[[1]] = NULL
    vallist = lapply(vallist, function(item) {
      if (is.name(item)) {
        as.character(item)
      } else if (is.numeric(item)) {
        item
      } else {
        formerr(pstring, "value list %s invalid", deparseJoin(vallist))
      }
    })
    names(vallist) = sapply(vallist, as.character)
    return(vallist)
  } else {
    pdeco[[1]] = quote(list)
    return(eval(as.call(pdeco), envir = pss.env))
  }
}

# parseNumeric: parse the range part of a numeric / integer parameter
parseNumeric = function(pdeco, ptype, pstring, pss.env) {
  if (length(pdeco) != 3) {
    formerr(pstring, "invalid numeric / integer range")
  }
  quasi.inf = .Machine$double.xmax
  parse.bound = function(expr, lower) {
    if (is.recursive(expr) && identical(expr[[1]], quote(`~`))) {
      if (length(expr) == 2 &&  identical(expr[[2]], quote(`.`))) {
        value = ifelse(lower, -Inf, Inf)
      } else {
        value = eval(expr[[2]], envir = pss.env)
      }
      if (is.infinite(value)) {
        if (ptype == "integer") {
          formerr(pstring, '"~."-bounds (unbounded but excluding "Inf") are only allowed for "numeric" variables.')
        }
        if ((value < 0) == lower) {
          value = ifelse(lower, -quasi.inf, quasi.inf)
        }
      } else if (ptype == "integer") {
        value = value + ifelse(lower, 1, -1)
      } else {
        if (value == 0) {
          value = value + .Machine$double.xmin * ifelse(lower, 1, -1)
        } else {
          epsilon = ifelse(lower == (value > 0), .Machine$double.eps, -.Machine$double.neg.eps)
          value = value + epsilon * value
        }
      }
    } else if (identical(expr, substitute())) {
      if (length(expr) > 1) {
        formerr(pstring, "invalid numeric / integer range")
      }
      value = ifelse(lower, -Inf, Inf)
    } else {
      value = eval(expr, envir = pss.env)
    }
  }
  lower.bound = parse.bound(pdeco[[2]], TRUE)
  upper.bound = parse.bound(pdeco[[3]], FALSE)

  allow.inf = TRUE
  if (lower.bound == -quasi.inf || upper.bound == quasi.inf) {
    if (lower.bound != -Inf && upper.bound != Inf) {
      # no true 'Inf' occurs, so we can translate quasi.inf to Inf and
      # instead use the 'allow.inf' parameter
      if (lower.bound == -quasi.inf) {
        lower.bound = -Inf
      }
      if (upper.bound == quasi.inf) {
        upper.bound = Inf
      }
      allow.inf = FALSE
    }
  }
  rl = list(lower = lower.bound, upper = upper.bound)
  if (ptype == "numeric") {
    rl$allow.inf = allow.inf
  }
  rl
}


# deparseJoin: deparse, but work with longer than 500 char expressions, mostly.
# Note that this is a heuristic for user messages only, the result can not be
# parsed again!
deparseJoin = function(what, sep = " ") {
  collapse(deparse(what, 500), sep = sep)
}

