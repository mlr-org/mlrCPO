# listCPO.R -- functionality for listing all included CPOs.

# registerCPO should be called for all internally defined CPOs. For clarity,
# that should happen right after the definition of the CPO Constructor.
# For possible caregories and subcategories, consult (and possibly extend!)
# the listCPO roxygen help.
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
#' Categories and subcategories are:
#' \tabular{lll}{
#'   \bold{category} \tab \bold{subcategory}   \tab \bold{description}
#'   meta            \tab                      \tab CPO that acts on other CPOs \cr
#'   tools           \tab                      \tab                             \cr
#'   data            \tab general              \tab                             \cr
#'                   \tab general data preproc \tab                             \cr
#'                   \tab factor data preproc  \tab                             \cr
#'                   \tab numeric data preproc \tab                             \cr
#'                   \tab feature conversion   \tab                             \cr
#'                   \tab cleanup              \tab                             \cr
#'   featurefilter   \tab general              \tab fltr CPO with operation arg \cr
#'                   \tab specialised          \tab specific feat filter CPO    \cr
#'   imputation      \tab general              \tab imp CPO with operation arg  \cr
#'                   \tab specialised          \tab specific imputation CPO     \cr
#'   tools           \tab imputation           \tab                             \cr
#'
#' @export
listCPO = function() {
  df = convertListOfRowsToDataFrame(parent.env(environment())$CPOLIST)
  df = df[order(paste(df$category, df$subcategory, df$name, sep = "$")), ]
  df$description = as.character(df$description)
  df
}