
##################################
### Printing                   ###
##################################

#' @export
print.CPOConstructor = function(x, ...) {
  args = dropNamed(formals(x), environment(x)$reserved.params)
  argvals = sapply(args, function(y) if (identical(y, substitute())) "" else paste(" =", convertToShortString(y)))
  argstring = paste(names(args), argvals, collapse = ", ", sep = "")
  catf("<<CPO %s(%s)>>", getCPOName(x), argstring)
}

#' @export
print.CPO = function(x, ...) {
  isprim = "CPOPrimitive" %in% class(x)
  pv = if (isprim) getBareHyperPars(x) else getHyperPars(x)
  argstring = paste(names(pv), sapply(pv, convertToShortString), sep = " = ", collapse = ", ")
  template = ifelse("CPOPrimitive" %in% class(x), "%s(%s)", "(%s)(%s)")
  catf(template, getCPOName(x), argstring, newline = FALSE)
  if (isprim && length({unexport = x$unexported.args})) {
      catf("[not exp'd: %s]", paste(names(unexport), sapply(unexport, convertToShortString), sep = " = ", collapse = ", "), newline = FALSE)
  }
  if (isprim && length({affect = getCPOAffect(x)})) {
    catf(" [%s]", paste(names(affect), sapply(affect, convertToShortString), sep = " = ", collapse = ", "))
  } else {
    cat("\n")
  }
}

#' @export
print.DetailedCPO = function(x, ...) {
  chain = as.list(x)
  catf("Retrafo chain of %d elements:", length(chain))
  is.first = TRUE
  for (retrafo in chain) {
    if (!is.first) {
      cat("  ====>\n")
    }
    is.first = FALSE
    class(retrafo) = setdiff(class(retrafo), "DetailedCPO")
    print(retrafo)
    cat("\n")
    print(getParamSet(retrafo))
  }
}

#' @export
summary.CPO = function(object, ...) {
  if (!"DetailedCPO" %in% class(object)) {
    class(object) = c(head(class(object), -1), "DetailedCPO", "CPO")
  }
  object
}

#' @export
print.CPOTrained = function(x, ...) {
  first = TRUE
  kind = getCPOKind(x)
  pt = getCPOPredictType(x)
  if (length(pt) == 3) {
    assert("retrafo" %in% kind)
    kind = "retrafo"
  }
  catf("CPO %s chain", collapse(stri_trans_totitle(kind), sep = " / "), newline = FALSE)
  if ("inverter" %in% kind) {
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

# print single shapeinfo row
catSI = function(x) {
  if (length(x$colnames)) {
    cat(collapse(sprintf("%s: %s", x$colnames, substr(x$coltypes, 1, 3)), sep = ", "))
  } else {
    cat("(empty)")
  }
}

# pretty-print shapeinfo
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
