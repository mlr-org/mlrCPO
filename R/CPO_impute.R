
#' @title Impute and Re-Impute Data
#'
#' @template cpo_doc_intro
#'
#' @description
#' Allows imputation of missing feature values through various techniques.
#' Note that you have the possibility to re-impute a data set
#' in the same way as the imputation was performed during training.
#' This especially comes in handy during resampling when one wants to perform the
#' same imputation on the test set as on the training set.
#'
#' The function \code{impute} performs the imputation on a data set and returns,
#' alongside with the imputed data set, an \dQuote{ImputationDesc} object
#' which can contain \dQuote{learned} coefficients and helpful data.
#' It can then be passed together with a new data set to \code{\link{reimpute}}.
#'
#' The imputation techniques can be specified for certain features or for feature classes,
#' see function arguments.
#'
#' You can either provide an arbitrary object, use a built-in imputation method listed
#' under \code{\link{imputations}} or create one yourself using \code{\link{makeImputeMethod}}.
#'
#' \code{cpoImpute} will impute some columns. \code{cpoImputeAll} behaves just like \code{cpoImpute},
#' except that it will throw an error if there are any missings remaining in its output. \code{cpoImputeAll}
#' should be used if one wants to prepend an imputer to a learner.
#'
#' @details
#' The description object contains these slots
#' \describe{
#'   \item{target [\code{character}]}{See argument.}
#'   \item{features [\code{character}]}{Feature names (column names of \code{data}).},
#'   \item{classes [\code{character}]}{Feature classes (storage type of \code{data}).}
#'   \item{lvls [\code{named list}]}{Mapping of column names of factor features to their levels,
#'     including newly created ones during imputation.}
#'   \item{impute [\code{named list}]}{Mapping of column names to imputation functions.}
#'   \item{dummies [\code{named list}]}{Mapping of column names to imputation functions.}
#'   \item{impute.new.levels [\code{logical(1)}]}{See argument.}
#'   \item{recode.factor.levels [\code{logical(1)}]}{See argument.}
#' }
#'
#' @param target.cols [\code{character}]\cr
#'   Name of the column(s) specifying the response.
#'   Default is \code{character(0)}.
#' @param classes [\code{named list}]\cr
#'   Named list containing imputation techniques for classes of columns.
#'   E.g. \code{list(numeric = imputeMedian())}.
#' @param cols [\code{named list}]\cr
#'   Named list containing names of imputation methods to impute missing values
#'   in the data column referenced by the list element's name. Overrules imputation set via
#'   \code{classes}.
#' @param dummy.classes [\code{character}]\cr
#'   Classes of columns to create dummy columns for.
#'   Default is \code{character(0)}.
#' @param dummy.cols [\code{character}]\cr
#'   Column names to create dummy columns (containing binary missing indicator) for.
#'   Default is \code{character(0)}.
#' @param dummy.type [\code{character(1)}]\cr
#'   How dummy columns are encoded. Either as 0/1 with type \dQuote{numeric}
#'   or as \dQuote{factor}.
#'   Default is \dQuote{factor}.
#' @param force.dummies [\code{logical(1)}]\cr
#'   Force dummy creation even if the respective data column does not
#'   contain any NAs. Note that (a) most learners will complain about
#'   constant columns created this way but (b) your feature set might
#'   be stochastic if you turn this off.
#'   Default is \code{FALSE}.
#' @param impute.new.levels [\code{logical(1)}]\cr
#'   If new, unencountered factor level occur during reimputation,
#'   should these be handled as NAs and then be imputed the same way?
#'   Default is \code{TRUE}.
#' @param recode.factor.levels [\code{logical(1)}]\cr
#'   Recode factor levels after reimputation, so they match the respective element of
#'   \code{lvls} (in the description object) and therefore match the levels of the
#'   feature factor in the training data after imputation?.
#'   Default is \code{TRUE}.
#' @template cpo_doc_outro
#' @export
#' @family imputation CPOs
cpoImpute = makeCPOExtendedTrafo("impute", # nolint
  par.set = c(
      makeParamSet(makeUntypedLearnerParam("target.cols", default = character(0)),
        makeUntypedLearnerParam("classes", default = list()),
        makeUntypedLearnerParam("cols", default = list()),
        makeUntypedLearnerParam("dummy.classes", default = character(0)),
        makeUntypedLearnerParam("dummy.cols", default = character(0))),
      pSSLrn(dummy.type = "factor": discrete[numeric, factor],
        force.dummies = FALSE: logical,
        impute.new.levels = TRUE: logical,
        recode.factor.levels = TRUE: logical)),
  dataformat = "df.features", # no properties.adding 'missings', since we don't impute all cols
  properties.needed = "factors",
  cpo.trafo = function(data, target, target.cols, ...) {
    impresult = impute(data, target.cols, ...)
    control = impresult[[2]]
    impresult[[1]]
  }, cpo.retrafo = function(data, control, ...) {
    reimpute(data, control)
  })
registerCPO(cpoImpute, "imputation", "general", "General imputation CPO that uses mlr::impute.")


#' @rdname cpoImpute
#' @export
cpoImputeAll = makeCPOExtendedTrafo("impute", # nolint
  par.set = c(
      makeParamSet(makeUntypedLearnerParam("target.cols", default = character(0)),
        makeUntypedLearnerParam("classes", default = list()),
        makeUntypedLearnerParam("cols", default = list()),
        makeUntypedLearnerParam("dummy.classes", default = character(0)),
        makeUntypedLearnerParam("dummy.cols", default = character(0))),
      pSSLrn(dummy.type = "factor": discrete[numeric, factor],
        force.dummies = FALSE: logical,
        impute.new.levels = TRUE: logical,
        recode.factor.levels = TRUE: logical)),
  dataformat = "df.features",
  properties.adding = "missings",
  properties.needed = "factors",
  cpo.trafo = function(data, target, target.cols, ...) {
    impresult = impute(data, target.cols, ...)
    control = impresult[[2]]
    impresult[[1]]
  }, cpo.retrafo = function(data, control, ...) {
    reimpute(data, control)
  })
registerCPO(cpoImputeAll, "imputation", "general", "General imputation CPO that uses mlr::impute and checks that all columns were imputed.")

# Create the CPOConstructor for the given imputation method. The CPO performs the given imputation method
# and has logical parameters 'impute.new.levels' and 'recode.factor.levels', besides additionally supplied parameters.
# @param name [character(1)] the CPO name (default ID) of the CPO
# @param method [function] imputation method
# @param additional.params [ParamSet | NULL] additional parameter set to add to CPO
# @param types [character | NULL] which types the CPO can operate on. NULL: all types, otherwise: subset of "numerics", "factors", "ordered"
# @return [CPOConstructor] constructs a CPO that performs the given imputation operation.
declareImputeFunction = function(name, method, additional.params, types = NULL) {
  makeCPOExtendedTrafo(paste0("impute.", name),
    par.set = c(additional.params,
      pSSLrn(
        impute.new.levels = TRUE: logical,
        recode.factor.levels = TRUE: logical)),
    dataformat = "df.features",
    properties.data = c("missings", if (is.null(types)) c("numerics", "factors", "ordered") else types),
    properties.adding = "missings",
    cpo.trafo = function(data, target, impute.new.levels, recode.factor.levels, ...) {
      if (ncol(data) == 0) {
        control = "NOCOL"
        return(data)
      }
      imputer = method(...)
      impresult = impute(data, cols = lapply(data, function(dummy) imputer),
        dummy.cols = character(0),
        force.dummies = FALSE, impute.new.levels = impute.new.levels,
        recode.factor.levels = recode.factor.levels)
      control = impresult[[2]]
      impresult[[1]]
    }, cpo.retrafo = function(data, control, ...) {
      if (identical(control, "NOCOL")) {
        return(data)
      }
      reimpute(data, control)
    })
}

#' @title Perform Imputation with Constant Value
#'
#' @template impute_doc_intro
#'
#' @param const [any]\cr
#'  Constant valued use for imputation.
#' @template impute_doc_outro
#' @template cpo_doc_outro
#' @export
cpoImputeConstant = declareImputeFunction("constant", imputeConstant, makeParamSet(makeUntypedLearnerParam("const")))  # nolint
registerCPO(cpoImputeConstant, "imputation", "specialised", "Imputation using a constant value.")

#' @title Perform Imputation with Median Value
#'
#' @template impute_doc_intro
#'
#' @template impute_doc_outro
#' @template cpo_doc_outro
#' @export
cpoImputeMedian = declareImputeFunction("median", imputeMedian, makeParamSet(), "numerics")  # nolint
registerCPO(cpoImputeMedian, "imputation", "specialised", "Imputation using the median.")

#' @title Perform Imputation with Mean Value
#'
#' @template impute_doc_intro
#'
#' @template impute_doc_outro
#' @template cpo_doc_outro
#' @export
cpoImputeMean = declareImputeFunction("mean", imputeMean, makeParamSet(), "numerics")  # nolint
registerCPO(cpoImputeMean, "imputation", "specialised", "Imputation using the mean.")

#' @title Perform Imputation with Mode Value
#'
#' @template impute_doc_intro
#'
#' @template impute_doc_outro
#' @template cpo_doc_outro
#' @export
cpoImputeMode = declareImputeFunction("mode", imputeMode, makeParamSet())  # nolint
registerCPO(cpoImputeMode, "imputation", "specialised", "Imputation using the mode.")

#' @title Perform Imputation with Multiple of Minimum
#'
#' @template impute_doc_intro
#'
#' @description
#' This method imputes by the minimum value of each column, multiplied by a constant.
#'
#' @param multiplier [\code{numeric(1)}]\cr
#'   Value that stored minimum or maximum is multiplied with when imputation is done.
#' @template impute_doc_outro
#' @template cpo_doc_outro
#' @export
cpoImputeMin = declareImputeFunction("min", imputeMin, pSSLrn(multiplier = 1: numeric[, ]), "numerics")  # nolint
registerCPO(cpoImputeMin, "imputation", "specialised", "Imputation using constant values shifted below the minimum.")

#' @title Perform Imputation with Multiple of Minimum
#'
#' @template impute_doc_intro
#'
#' @description
#' This method imputes by the maximum value of each column, multiplied by a constant.
#'
#' @param multiplier [\code{numeric(1)}]\cr
#'   Value that stored minimum or maximum is multiplied with when imputation is done.
#' @template impute_doc_outro
#' @template cpo_doc_outro
#' @export
cpoImputeMax = declareImputeFunction("max", imputeMax, pSSLrn(multiplier = 1: numeric[, ]), "numerics")  # nolint
registerCPO(cpoImputeMax, "imputation", "specialised", "Imputation using constant values shifted above the maximum.")

#' @title Perform Imputation with Uniformly Random Values
#'
#' @template impute_doc_intro
#'
#' @param min [\code{numeric(1)}]\cr
#'   Lower bound for uniform distribution.
#'   If NA (default), it will be estimated from the data.
#' @param max [\code{numeric(1)}]\cr
#'   Upper bound for uniform distribution.
#'   If NA (default), it will be estimated from the data.
#' @template impute_doc_outro
#' @template cpo_doc_outro
#' @export
cpoImputeUniform = declareImputeFunction("uniform", imputeUniform, {  # nolint
  ps = pSSLrn(min = 0: numeric[, ] [[special.vals = list(NA_real_)]],
    max = 0: numeric[, ] [[special.vals = list(NA_real_)]])
  ps$pars$min$default = NA_real_
  ps$pars$max$default = NA_real_
  ps
}, "numerics")
registerCPO(cpoImputeUniform, "imputation", "specialised", "Imputation using uniformly distributed random values.")

#' @title Perform Imputation with Normally Distributed Random Values
#'
#' @template impute_doc_intro
#'
#' @param mu [\code{numeric(1)}]\cr
#'   Mean of normal distribution. If missing it will be estimated from the data.
#' @param sd [\code{numeric(1)}]\cr
#'   Standard deviation of normal distribution. If missing it will be estimated from the data.
#' @template impute_doc_outro
#' @template cpo_doc_outro
#' @export
cpoImputeNormal = declareImputeFunction("normal", imputeNormal, {  # nolint
  ps = pSSLrn(mu = 0: numeric[, ] [[special.vals = list(NA_real_)]],
    sd = 0: numeric[, ] [[special.vals = list(NA_real_)]])
  ps$pars$mu$default = NA_real_
  ps$pars$sd$default = NA_real_
  ps
}, "numerics")
registerCPO(cpoImputeNormal, "imputation", "specialised", "Imputation using normally distributed random values.")

#' @title Perform Imputation with Random Values
#'
#' @template impute_doc_intro
#'
#' @description
#' This imputation method imputes with random values drawn from a distribution
#' that approximates the data distribution as a histogram.
#'
#' @param breaks [\code{numeric(1)} | \dQuote{Sturges}]\cr
#'  Number of breaks to use in \code{\link[graphics]{hist}}.
#'  Defaults to auto-detection via \dQuote{Sturges}.
#' @param use.mids [\code{logical(1)}]\cr
#'  If \code{x} is numeric and a histogram is used, impute with bin mids (default)
#'  or instead draw uniformly distributed samples within bin range.
#' @template impute_doc_outro
#' @template cpo_doc_outro
#' @export
cpoImputeHist = declareImputeFunction("hist", imputeHist, pSSLrn(breaks = "Sturges": integer[1, ] [[special.vals = list("Sturges")]],  # nolint
  use.mids = TRUE: logical))
registerCPO(cpoImputeHist, "imputation", "specialised", "Imputation using random values with probabilities approximating the data.")

#' @title Perform Imputation with an \code{mlr} \code{Learner}
#'
#' @template impute_doc_intro
#'
#' @param learner [\code{\link{Learner}} | \code{character(1)}]\cr
#'  Supervised learner. Its predictions will be used for imputations.
#'  If you pass a string the learner will be created via \code{\link{makeLearner}}.
#'  Note that the target column is not available for this operation.
#' @param features [\code{character}]\cr
#'  Features to use in \code{learner} for prediction.
#'  Default is \code{NULL} which uses all available features except the target column
#'  of the original task.
#' @template impute_doc_outro
#' @template cpo_doc_outro
#' @export
cpoImputeLearner = declareImputeFunction("learner", imputeLearner, makeParamSet(makeUntypedLearnerParam("learner"),  # nolint
  makeUntypedLearnerParam("features", default = NULL)))
registerCPO(cpoImputeLearner, "imputation", "specialised", "Imputation using the response of a classification or regression learner.")
