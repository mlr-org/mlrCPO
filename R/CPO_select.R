

# we create two very similar CPOs, so write out the arguments here and do.call them later.
cpoSelect.callargs = alist(  # nolint
    cpo.name = "select",
    par.set = c(
        pSSLrn(type = list(): discrete[numeric, ordered, factor, other]^NA,
          index = integer(0): integer[1, ]^NA),
        makeParamSet(makeUntypedLearnerParam("names", default = character(0)),
          makeCharacterParam("pattern", NULL, special.vals = list(NULL))),
        pSSLrn(
            pattern.ignore.case = FALSE: logical [[requires = quote(!is.null(pattern))]],
            pattern.perl = FALSE: logical [[requires = quote(!is.null(pattern))]],
            pattern.fixed = FALSE: logical [[requires = quote(!is.null(pattern))]],
            invert = FALSE: logical)),
    dataformat = "df.features",
    export = FALSE,
    cpo.train = {
      assertCharacter(names, any.missing = FALSE, unique = TRUE)
      assertIntegerish(index, any.missing = FALSE, unique = TRUE)
      getColIndices(data, type, index, names, pattern, invert, pattern.ignore.case, pattern.perl, pattern.fixed)
    },
    cpo.retrafo = {
      data[control]
    })

#' @title Drop All Columns Except Certain Selected Ones from Data
#'
#' @template cpo_doc_intro
#'
#' @description
#' Select columns by type or name. The parameters \dQuote{type} and
#' \dQuote{pattern} are additive; if both are given, all column that match
#' either will be returned.
#'
#' \code{cpoSelectFreeProperties} behaves just as \code{cpoSelect}, with the additional function
#' that it is treated like a \code{\link{CPO}} that removes all data properties from the data.
#' This disables the internal property check and can be useful when trying to compose \code{\link{CPO}}s
#' that do not have compatible properties.
#'
#'
#' @param type [\code{character}]\cr
#'   One or more out of \dQuote{numeric}, \dQuote{ordered}, \dQuote{factor}, \dQuote{other}.
#'   The type of columns to keep. Default is \code{character(0)}.
#' @param index [\code{integer}]\cr
#'   Indices of columns to keep. Note that the index counts columns without the target column(s).
#'   This and the next parameter make it possible to re-order columns. While all columns which match either
#'   \dQuote{type}, \dQuote{pattern} or \dQuote{index} remain in the resulting data, the ones
#'   selected by \dQuote{index} are put at the front in the order specified.
#'   Default is \code{integer(0)}.
#' @param names [\code{character}]\cr
#'   Names of columns to keep. Matching columns will be kept in order of their names occurring, but after
#'   the columns indicated in \dQuote{index}.
#' @param pattern [\code{character(1)}]\cr
#'   A pattern to match against the column names. Same as in \code{\link{grep}}.
#'   Default is \code{NULL} for no matching.
#' @param pattern.ignore.case [\code{logical(1)}]\cr
#'   Influences behaviour of \dQuote{pattern}: Whether to perform case insensitive matching. Same as in \code{\link{grep}}.
#'   Default is \code{FALSE}.
#' @param pattern.perl [\code{logical(1)}]\cr
#'   Influences behaviour of \dQuote{pattern}: Should Perl-compatible regexps be used? Same as in \code{\link{grep}}.
#'   Default is \code{FALSE}.
#' @param pattern.fixed [\code{logical(1)}]\cr
#'   Influences behaviour of \dQuote{pattern}: Whether to use match \code{pattern} as as is. Same as in \code{\link{grep}}.
#'   Default is \code{FALSE}.
#' @param invert [\code{logical(1)}]\cr
#'   Invert column selection: Drop the named columns and return the rest, instead of keeping the selected
#'   columns only. Default is \code{FALSE}.
#'
#' @template cpo_doc_outro
#' @export
cpoSelect = do.call(makeCPO, cpoSelect.callargs)  # nolint
registerCPO(cpoSelect, "data", "feature selection ", "Select features from a data set by type, column name, or column index.")


cpoSelect.callargs$cpo.name = "selectfreeprop"
cpoSelect.callargs$properties.adding = paste0(cpo.dataproperties, ".sometimes")
cpoSelect.callargs$properties.needed = paste0(cpo.dataproperties, ".sometimes")
#' @rdname cpoSelect
#' @export
cpoSelectFreeProperties = do.call(makeCPO, cpoSelect.callargs)  # nolint
registerCPO(cpoSelectFreeProperties, "data", "feature selection ", "Select features from a data set, also reset data properties.")
