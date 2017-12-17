# listCPO.R -- functionality for listing all included CPOs.

# Register a CPO in the database of installed CPOs.
#
# registerCPO should be called for all internally defined CPOs. This
# should happen right after the definition of the CPO Constructor for clarity.
# For possible caregories and subcategories, consult (and possibly extend!)
# the listCPO roxygen help.
# @param cpo [CPOConstructor | CPO] the CPO constructor, or an example cpo, to register
# @param category [character(1)] the category to register the CPO under
# @param subcategory [character(1) | NULL] an optional subcategory
# @param description [character(1)] a short description of the CPO. #TODO: possibly mine roxygen docu instead?
# @return [invisible(NULL)]
registerCPO = function(cpo, category, subcategory = NULL, description) {
  name = deparse(substitute(cpo))
  if ("CPO" %in% class(cpo)) {
    name = deparse(substitute(cpo)[[1]])
    cponame = getCPOName(cpo)
  } else if (!is.function(cpo)) {
    name = cpo$name
    cponame = cpo$cponame
  } else {
    # don't load packages when we only want to inspect CPO
    # TODO: there has to be a better way
    environment(cpo) = new.env(parent = environment(cpo))
    environment(cpo)$packages = character(0)
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
  invisible(NULL)
}

#' @title List all Built-in CPOs
#'
#' @description
#' Return a \code{data.frame} with the columns \dQuote{name},
#' \dQuote{cponame}, \dQuote{category}, \dQuote{subcategory},
#' \dQuote{description}.
#'
#' Categories and subcategories are:
#' \tabular{lll}{
#'   \bold{category} \tab \bold{subcategory}   \tab \bold{description}          \cr
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
#' }
#' @export
listCPO = function() {
  df = convertListOfRowsToDataFrame(parent.env(environment())$CPOLIST)
  df = df[order(paste(df$category, df$subcategory, df$name, sep = "$")), ]
  df$description = as.character(df$description)
  df$name = as.character(df$name)
  df$cponame = as.character(df$cponame)
  addClasses(df, "ListCPO")
}
