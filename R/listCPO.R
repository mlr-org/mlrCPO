

registerCPO = function(cpo, category, subcategory = NULL, description) {
  name = deparse(substitute(cpo))
  if (!is.function(cpo)) {
    name = cpo$name
    cponame = cpo$cponame
  } else {
    cpo = cpo()
    assertClass(cpo, "CPO")
    cponame = getCPOName(cpo)
  }
  assertString(cponame)
  assertString(name)
  assertString(category)
  if (is.null(subcategory)) {
    subcategory = ""
  }
  assertString(subcategory)
  assertString(description)
  assign("CPOLIST", c(parent.env(environment())$CPOLIST,
    list(list(name = name, cponame = cponame, category = category,
      subcategory = subcategory, description = description))),
    envir = parent.env(environment()))
}

#' @title List all built-in CPOs
#'
#' @description
#' Return a \code{data.frame} with the columns \dQuote{name},
#' \dQuote{cponame}, \dQuote{category}, \dQuote{subcategory},
#' \dQuote{description}.
#'
#' @export
listCPO = function() {
  df = convertListOfRowsToDataFrame(parent.env(environment())$CPOLIST)
  df = df[order(paste(df$category, df$subcategory, df$name, sep = "$")), ]
  df$description = as.character(df$description)
  df
}
