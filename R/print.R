
##################################
### Printing                   ###
##################################

#' @title Print CPO Objects
#'
#' @description
#' Prints a simple representation of a \code{\link{CPOConstructor}},
#' \code{\link{CPO}} or \code{\link{CPOTrained}}. If
#' \code{verbose} is \code{TRUE}, more information about the given objects
#' will be given. For \code{\link{CPOConstructor}}, that is the trafo and retrafo
#' functions, for \code{\link{CPO}}, the individual constituents of a compound
#' CPO will be printed.
#'
#' Verbose printing can also be done using the \code{!} operator. \code{!cpo} is equivalent to
#' \code{print(cpo, verbose = TRUE)}.
#'
#' @param x [\code{\link{CPOConstructor}} | \code{\link{CPO}} | \code{\link{CPOTrained}}]\cr
#'   The \code{\link{CPOConstructor}} to print.
#' @param verbose [\code{logical(1)}]\cr
#'   Whether to print further information. Default is \code{FALSE}.
#' @param ... [any]\cr
#'   Further arguments.
#' @return [\code{invisible(NULL)}].
#' @family CPOConstructor related
#' @export
print.CPOConstructor = function(x, verbose = FALSE, ...) {
  assertFlag(verbose)
  args = dropNamed(formals(x), reserved.params)
  mycts = function(x) {
    short = convertToShortString(x)
    if (short %in% c("<call>", "<name>")) {
      trystr = deparse(x)[1]
      if (nchar(trystr) <= 20) {
        short = trystr
      }
    }
    short
  }
  argvals = vcapply(args, function(y) if (identical(y, substitute())) "" else paste(" =", mycts(y)))
  argstring = paste(names(args), argvals, collapse = ", ", sep = "")
  catf("<<CPO %s(%s)>>", getCPOName(x), argstring)
  if (verbose) {
    type.extended = environment(x)$cpo.type.extended
    tf = environment(x)$trafo.funs
    allfuns = c("cpo.trafo", "cpo.retrafo", "cpo.train.invert", "cpo.invert")

    relfuns = switch(type.extended,
      feature = allfuns[1:2], target = allfuns, retrafoless = allfuns[1],
      feature.extended = allfuns[1:2], target.extended = allfuns[-3],
      other = list())  # for fauxCPOConstructors that don't fit in any of these roles.
    relfunlist = sapply(relfuns, function(x) tf[[paste0(x, ".orig")]], simplify = FALSE)
    if (type.extended %in% c("trafo", "retrafo")) {
      names(relfunlist)[1] = "train"
    }
    for (funname in names(relfunlist)) {
      fun = relfunlist[[funname]]
      if (!is.null(fun)) {
        catf("\n%s:", funname)
        print(fun)
      }
    }
  }
  invisible(NULL)
}

# verbose print function for CPOs:
# calls nonverbose-print in return for all included primitive CPOs.
# @param x [CPO] the CPO to print
vprint = function(x) {
  chain = as.list(x)
  catf("Trafo chain of %d cpos:", length(chain))
  is.first = TRUE
  for (cpo in chain) {
    if (!is.first) {
      cat("  ====>\n")
    }
    is.first = FALSE
    print(cpo)
    ot = getCPOOperatingType(cpo)
    catf("Operating: %s", ot, newline = FALSE)
    if (ot == "target") {
      if (cpo$constant.invert) {
        cat(" [constant invert]")
      }
      catf("\nConversion: %s -> %s", cpo$convertfrom, cpo$convertto)
      pt = getCPOPredictType(cpo)
      catf("Predict type mapping:\n%s", collapse(paste(names(pt), pt, sep = " -> "), sep = "\n"))
    } else {
      cat("\n")
    }
    cat("ParamSet:\n")
    print(getParamSet(cpo))
  }
}

#' @rdname print.CPOConstructor
#' @export
print.CPO = function(x, verbose = FALSE, ...) {
  if (verbose) {
    return(vprint(x))
  }
  isprim = "CPOPrimitive" %in% class(x)
  pv = if (isprim) getBareHyperPars(x, include.unexported = FALSE) else getHyperPars(x)
  argstring = paste(names(pv), vcapply(pv, convertToShortString), sep = " = ", collapse = ", ")
  template = ifelse("CPOPrimitive" %in% class(x), "%s(%s)", "(%s)(%s)")
  catf(template, x$debug.name, argstring, newline = FALSE)
  if (isprim && length({unexport = x$unexported.pars})) {
      catf("[not exp'd: %s]", paste(names(unexport), vcapply(unexport, convertToShortString), sep = " = ", collapse = ", "), newline = FALSE)
  }
  if (isprim && length({affect = getCPOAffect(x)})) {
    catf(" [%s]", paste(names(affect), vcapply(affect, convertToShortString), sep = " = ", collapse = ", "))
  } else {
    cat("\n")
  }
}

#' @rdname print.CPOConstructor
#' @family retrafo related
#' @family inverter related
#' @export
print.CPOTrained = function(x, verbose = FALSE, ...) {
  first = TRUE
  object.type = getCPOClass(x)
  invcap = getCPOTrainedCapability(x)
  reverse = invcap["retrafo"] < 1

  pt = names(getCPOPredictType(x))
  caps = names(Filter(function(x) x > 0, invcap))
  names = c(retrafo = "Retrafo", invert = "Inverter")
  catf("CPO %s chain", collapse(names[caps], sep = " / "), newline = FALSE)
  if (!is.null(x$convertfrom) && !is.null(x$convertto)) {
    if (x$convertfrom == x$convertto) {
      catf(" {type:%s}", x$convertfrom, newline = FALSE)
    } else {
      cc = c(x$convertfrom, x$convertto)
      if (reverse) {
        cc = rev(cc)
      }
      catf(" {conv:%s->%s}", cc[1], cc[2], newline = FALSE)
    }
  }
  if (invcap["invert"] > 0) {
    catf(" (able to predict '%s')", collapse(pt, sep = "', '"))
  } else {
    cat("\n")
  }
  plist = as.list(x)
  if (reverse) {
    plist = rev(plist)
  }
  for (primitive in plist) {
    if (!first) {
      cat(" =>\n")
    }
    first = FALSE
    pv = getBareHyperPars(primitive$element$cpo, include.unexported = verbose)
    argstring = paste(names(pv), vcapply(pv, convertToShortString), sep = " = ", collapse = ", ")
    pcap = getCPOTrainedCapability(primitive)
    if (pcap["invert"] > 0) {
      fromto = c(primitive$convertfrom, primitive$convertto)
      if (reverse) {
        fromto = rev(fromto)
      }
      if (fromto[1] == fromto[2]) {
        convstring = paste0("{type:", fromto[1], "}")
      } else {
        convstring = paste0("{conv:", collapse(fromto, "->"), "}")
      }
    } else {
      convstring = ""
    }
    if (pcap["retrafo"] < 1) {
      classname = "INVERTER"
    } else {
      classname = "RETRAFO"
    }
    catf("[%s %s(%s)%s]", classname, getCPOName(primitive), argstring, convstring, newline = FALSE)
  }
  cat("\n")
}

invisible.return = makeS3Obj("cpoinvisible")

#' @export
print.cpoinvisible = function(x, ...) invisible(NULL)

# verbose printing
#' @rdname print.CPOConstructor
#' @export
`!.CPOConstructor` = function(x) { print(x, verbose = TRUE) ; invisible.return }

# verbose printing
#' @rdname print.CPOConstructor
#' @export
`!.CPO` = function(x) { print(x, verbose = TRUE) ; invisible.return }

# verbose printing
#' @rdname print.CPOConstructor
#' @export
`!.CPOTrained` = function(x) { print(x, verbose = TRUE) ; invisible.return }


# helper function for retrafo / inverter 'element's
# The user shouldn't see these too often
#' @export
print.RetrafoElement = function(x, ...) {
  if (!is.null(x$prev.retrafo.elt)) {
      x$prev.retrafo.elt = paste0("<More ", class(x$prev.retrafo.elt), ">")
  }
  print(c(x))
}
#' @export
print.InverterElement = function(x, ...) {
  if (!is.null(x$truth)) {
    x$truth = paste0("<truth ", firstNonNull(nrow(x$truth), length(x$truth)), " lines>")
  }
  if (!is.null(x$task.desc) && "TaskDesc" %in% class(x$task.desc)) {
    x$task.desc = paste0("<task desc '", x$task.desc$id, "'>")
  }
  print.RetrafoElement(x)
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

#' @export
print.ListCPO = function(x, ...) {
  printHead(as.data.frame(dropNamed(x, drop = "description")), ...)
}
