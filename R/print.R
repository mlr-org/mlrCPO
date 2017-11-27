
##################################
### Printing                   ###
##################################

#' @title Print CPO objects.
#'
#' @description
#' Prints a simple representation of a \code{\link{CPOConstructor}},
#' \code{\link{CPO}} or \code{\link{CPOTrained}}. If
#' \code{verbose} is \code{TRUE}, more information about the given objects
#' will be given. For \code{\link{CPOConstructor}}, that is the trafo and retrafo
#' functions, for \code{\link{CPO}}, the individual constituents of a compound
#' CPO will be printed.
#'
#' @param x [\code{\link{CPOConstructor}} | \code{\link{CPO}} | \code{\link{CPOTrained}}]\cr
#'   The \code{\link{CPOConstructor}} to print.
#' @param verbose [\code{logical(1)}]\cr
#'   Whether to print further information. Default is \code{FALSE}.
#' @return [\code{invisible(NULL)}].
#' @family CPOConstructor related
#' @export
print.CPOConstructor = function(x, verbose = FALSE, ...) {
  assertFlag(verbose)
  args = dropNamed(formals(x), environment(x)$reserved.params)
  argvals = sapply(args, function(y) if (identical(y, substitute())) "" else paste(" =", convertToShortString(y)))
  argstring = paste(names(args), argvals, collapse = ", ", sep = "")
  catf("<<CPO %s(%s)>>", getCPOName(x), argstring)
  if (verbose) {
    catf("CPO Trafo:")
    print(environment(x)$trafo.funs$cpo.trafo.orig)
    retrafo.fun = environment(x)$trafo.funs$cpo.retrafo.orig
    if (!is.null(retrafo.fun)) {
      catf("\nCPO Retrafo:")
      print(retrafo.fun)
    }
  }
}

# verbose print function for CPOs:
# calls nonverbose-print in return for all included primitive CPOs.
# @param x [CPO] the CPO to print
vprint = function(x) {
  chain = as.list(x)
  catf("Trafo chain of %d elements:", length(chain))
  is.first = TRUE
  for (retrafo in chain) {
    if (!is.first) {
      cat("  ====>\n")
    }
    is.first = FALSE
    print(retrafo)
    cat("\n")
    print(getParamSet(retrafo))
  }
}

#' @rdname print.CPOConstructor
#' @export
print.CPO = function(x, verbose = FALSE, ...) {
  if (verbose) {
    return(vprint(x))
  }
  isprim = "CPOPrimitive" %in% class(x)
  pv = if (isprim) getBareHyperPars(x) else getHyperPars(x)
  argstring = paste(names(pv), sapply(pv, convertToShortString), sep = " = ", collapse = ", ")
  template = ifelse("CPOPrimitive" %in% class(x), "%s(%s)", "(%s)(%s)")
  catf(template, x$debug.name, argstring, newline = FALSE)
  if (isprim && length({unexport = x$unexported.pars})) {
      catf("[not exp'd: %s]", paste(names(unexport), sapply(unexport, convertToShortString), sep = " = ", collapse = ", "), newline = FALSE)
  }
  if (isprim && length({affect = getCPOAffect(x)})) {
    catf(" [%s]", paste(names(affect), sapply(affect, convertToShortString), sep = " = ", collapse = ", "))
  } else {
    cat("\n")
  }
}

#' @rdname print.CPOConstructor
#' @family retrafo related
#' @family inverter related
#' @export
print.CPOTrained = function(x, ...) {
  first = TRUE
  object.type = getCPOObjectType(x)
  invcap = getCPOInvertCapability(x)
  pt = names(getCPOPredictType(x))
  if (length(pt) == 3) {
    assert(object.type == "CPORetrafo")
  }
  catf("CPO %s chain", collapse(stri_trans_totitle(invcap), sep = " / "), newline = FALSE)
  if (invcap %in% c("inverter", "hybrid")) {
    catf("(able to predict '%s')", collapse(pt, sep = "', '"))
  } else {
    cat("\n")
  }
  for (primitive in as.list(x)) {
    if (!first) {
      cat("=>")
    }
    first = FALSE
    pv = getHyperPars(primitive)
    argstring = paste(names(pv), sapply(pv, convertToShortString), sep = " = ", collapse = ", ")
    catf("[RETRAFO %s(%s)]", getCPOName(primitive), argstring, newline = FALSE)
  }
  cat("\n")
}

# ShapeInfo printing

# helper function for printing single shapeinfo row
# @param x [ShapeInfo] the ShapeInfo to print
catSI = function(x) {
  if (length(x$colnames)) {
    cat(collapse(sprintf("%s: %s", x$colnames, substr(x$coltypes, 1, 3)), sep = ", "))
  } else {
    cat("(empty)")
  }
}

# pretty-print shapeinfo; mostly useful for debugging, the user
# hardly ever gets to see this, except possibly when inspecting
# retrafo state.
#' @export
print.OutputShapeInfo = function(x, ...) {
  cat("<ShapeInfo (output)")
  if (all(c("colnames", "coltypes") %in% names(x))) {
    cat(" ")
    catSI(x)
    cat(">\n")
  } else {
    cat(">:\n")
    for (s in names(x)) {
      cat(s, ":\n", sep = "")
      print(x[[s]])
    }
  }
}

#' @export
print.InputShapeInfo = function(x, ...) {
  cat("<ShapeInfo (input) ")
  catSI(x)
  cat(">\n")
}

#' @export
print.ShapeInfo = function(x, ...) {
  cat("<ShapeInfo ")
  catSI(x)
  cat(">\n")
}
