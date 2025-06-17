#' @include RandomForestSRC.R

#' @title Filter Features by Thresholding Filter Values
#'
#' @template cpo_doc_intro
#'
#' @description
#' First, calls \code{\link[mlr]{generateFilterValuesData}}.
#' Features are then selected via \code{select} and \code{val}.
#'
#' @param method [\code{character(1)}]\cr
#'   See \code{\link[mlr]{listFilterMethods}}.
#'   Default is \dQuote{randomForestSRC.rfsrc}.
#' @param fval [\code{\link[mlr]{FilterValues}}]\cr
#'   Result of \code{\link[mlr]{generateFilterValuesData}}.
#'   If you pass this, the filter values in the object are used for feature filtering.
#'   \code{method} and \code{...} are ignored then.
#'   Default is \code{NULL} and not used.
#' @template arg_filter
#' @param filter.args [\code{list}]\cr
#'   Passed down to selected filter method. Default is \code{list()}.
#' @template cpo_doc_outro
#' @export
#' @family filter
cpoFilterFeatures = makeCPOExtendedTrafo("filterFeatures", #nolint
  par.set = c(
      makeParamSet(makeDiscreteLearnerParam("method", values = unique(c(names(.FilterRegister()), "randomForestSRC.rfsrc")), default = "randomForestSRC.rfsrc"),
        makeUntypedLearnerParam("fval", default = NULL)),
      pSSLrn(
          perc = NULL: numeric[0, 1] [[special.vals = list(NULL)]],
          abs = NULL: integer[0, ] [[special.vals = list(NULL)]],
          threshold = NULL: numeric[, ] [[special.vals = list(NULL)]]),
      makeParamSet(makeUntypedLearnerParam("filter.args", default = list()))),
  dataformat = "task",
  cpo.trafo = function(data, target, method, fval, perc, abs, threshold, filter.args) {
    assertList(filter.args)
    if (method == "randomForestSRC.rfsrc" && method %nin% names(.FilterRegister())) method = "randomForestSRC_importance"
    fullargs = c(list(task = data, method = method, fval = fval, perc = perc, abs = abs, threshold = threshold), filter.args)
    assertSubset(unique(names(fullargs)[duplicated(names(fullargs))]), "")
    data = do.call(filterFeatures, fullargs)
    control = getTaskFeatureNames(data)
    data
  }, cpo.retrafo = function(data, control, ...) {
    data[control]
  })
registerCPO(cpoFilterFeatures, "featurefilter", "general", "Filter features using a provided method.")

# Creates a CPOConstructor for the given filter method
#
# This saves us from having to write "makeCPOExtendedTrafo..." for every filter, since the
# structure of the filter CPOs is always the same.
# @param method [character] the method name, as found in mlr:::.FilterRegister. This can have
#   a length greater than 1; the given elements are looked up in order and the first one present
#   is used. This is to make compatibility with multiple R versions possible.
# @param ... parameters to use for the CPO, as used by ParamSet::pSSLrn
# @param .par.set [ParamSet | NULL] additional parameter set to use
# @return [CPOConstructor] A CPOConstructor that creates the CPO performing the `method`.
declareFilterCPO = function(method, ..., .par.set = makeParamSet()) {
  # put together parameters from '...' and from .par.set, as well as
  # the parameters that all filter-CPOs have in common: perc, abs, threshold
  par.set = c(pSSLrn(...,
      perc = NULL: numeric[0, 1] [[special.vals = list(NULL)]],
      abs = NULL: integer[0, ] [[special.vals = list(NULL)]],
      threshold = NULL: numeric[, ] [[special.vals = list(NULL)]]),
    .par.set)

  # get the filter object from mlr. The .FilterRegister was previously gotten
  # from mlr at the top of this file.
  for (mtry in method) {
    if (mtry %in% names(.FilterRegister())) {
      method = mtry
      break
    }
  }
  if (length(method) > 1) {
    stopf("None of the methods %s could be found", collapse(method))
  }
  methodobj = get(method, envir = .FilterRegister())

  makeCPO(method, par.set = par.set, dataformat = "task",
    properties.target = c(methodobj$supported.tasks, cpo.targetproperties),  # the supported tasks as declared by the filter object
    packages = methodobj$pkg,
    cpo.train = function(data, target, perc, abs, threshold, ...) {
      filter.args = list(...)  # all arguments that are not perc, abs, threshold and are therefore special to the given filter

      # we go on to subset the task to only the feature types supported by the filter (numerics, factors, ordered)
      td = getTaskData(data, target.extra = TRUE)$data
      tt = vcapply(td, function(x) class(x)[1])
      tt = c(numeric = "numerics", factor = "factors", ordered = "ordered")[tt]
      stask = subsetTask(data, features = tt %in% methodobj$supported.features)  # the subsetted task

      # the filterFeatures call
      ftask = do.call(filterFeatures, c(list(task = stask, method = method, fval = NULL, perc = perc, abs = abs, threshold = threshold), filter.args))

      # 'control' object contains the names of all features we don't throw away
      setdiff(getTaskFeatureNames(data), setdiff(getTaskFeatureNames(stask), getTaskFeatureNames(ftask)))
    }, cpo.retrafo = function(data, control, ...) {
      data[control]
    })
}

#' @title Filter Features: \dQuote{mrmr}
#'
#' @template cpo_doc_intro
#'
#' @description
#' Minimum redundancy, maximum relevance filter \dQuote{mrmr} computes the
#' mutual information between the target and each individual feature minus the
#' average mutual information of previously selected features and this feature
#' using the \pkg{mRMRe} package.
#' @template arg_filter
#' @template cpo_doc_outro
#' @export
cpoFilterMrmr = declareFilterCPO("mrmr") # nolint  #  missing parameters
registerCPO(cpoFilterMrmr, "featurefilter", "specialised", "Filter features using 'minimum redundancy, maximum relevance'.")

#' @title Filter Features: \dQuote{carscore}
#'
#' @template cpo_doc_intro
#'
#' @description
#' Filter \dQuote{carscore} determines the \dQuote{Correlation-Adjusted (marginal) coRelation
#' scores} (short CAR scores). The CAR scores for a set of features are defined as the
#' correlations between the target and the decorrelated features.
#' @param diagonal [\code{logical(1)}]\cr
#'   See the \code{carscore} help.
#' @template arg_filter
#' @template cpo_doc_outro
#' @export
cpoFilterCarscore = declareFilterCPO("carscore", diagonal = FALSE: logical)  # nolint # missing parameter 'lambda'
registerCPO(cpoFilterCarscore, "featurefilter", "specialised", "Filter features using correlation-adjusted marginal correlation.")

#' @title Filter Features: \dQuote{randomForestSRC.rfsrc}
#'
#' @template cpo_doc_intro
#'
#' @description
#' Filter \dQuote{randomForestSRC.rfsrc} computes the importance of random forests
#' fitted in package \pkg{randomForestSRC}. The concrete method is selected via
#' the \code{method} parameter. Possible values are \code{permute} (default), \code{random},
#' \code{anti}, \code{permute.ensemble}, \code{random.ensemble}, \code{anti.ensemble}.
#' See the VIMP section in the docs for \code{\link[randomForestSRC]{rfsrc}} for
#' details.
#' @template arg_filter
#' @template cpo_doc_outro
#' @export
cpoFilterRfSRCImportance = declareFilterCPO(c("randomForestSRC_importance", "randomForestSRC.rfsrc")) #,  # nolint
#  method = "permute": discrete[permute, random, anti, permute.ensemble, random.ensemble, anti.ensemble])  # missing parameters
registerCPO(cpoFilterRfSRCImportance, "featurefilter", "specialised", "Filter features using randomForestSRC.rfsrc.")

#' @title Filter Features: \dQuote{cforest.importance}
#'
#' @template cpo_doc_intro
#'
#' @description
#' Permutation importance of random forests fitted in package \pkg{party}.
#' The implementation follows the principle of mean decrese in accuracy used
#' by the \pkg{randomForest} package (see description of \dQuote{randomForest.importance})
#' filter.
#' @param mtry [\code{integer(1)}]\cr
#'   Number of features to draw during feature bagging
#' @template arg_filter
#' @template cpo_doc_outro
#' @export
cpoFilterRfCImportance = declareFilterCPO("cforest.importance", mtry = 5: integer[1, ])  # nolint  # missing parameters
registerCPO(cpoFilterRfCImportance, "featurefilter", "specialised", "Filter features using party::cforest variable importance.")

#' @title Filter Features: \dQuote{randomForest.importance}
#'
#' @template cpo_doc_intro
#'
#' @description
#' Filter \dQuote{randomForest.importance} makes use of the \code{\link[randomForest]{importance}}
#' from package \pkg{randomForest}. The importance measure to use is selected via
#' the \code{method} parameter:
#' \describe{
#'   \item{oob.accuracy}{Permutation of Out of Bag (OOB) data.}
#'   \item{node.impurity}{Total decrease in node impurity.}
#' }
#' @template arg_filter
#' @template cpo_doc_outro
#' @export
cpoFilterRfImportance = declareFilterCPO(c("randomForest_importance", "randomForest.importance"))  #, method = "oob.accuracy": discrete[oob.accuracy, node.impurity])  # nolint  # missing parameters
registerCPO(cpoFilterRfImportance, "featurefilter", "specialised", "Filter features using randomForest variable importance.")

#' @title Filter Features: \dQuote{linear.correlation}
#'
#' @template cpo_doc_intro
#'
#' @description
#' The Pearson correlation between each feature and the target is used as an indicator
#' of feature importance. Rows with NA values are not taken into consideration.
#' @template arg_filter
#' @template cpo_doc_outro
#' @export
cpoFilterLinearCorrelation = declareFilterCPO("linear.correlation")  # nolint
registerCPO(cpoFilterLinearCorrelation, "featurefilter", "specialised", "Filter features using Pearson correlation.")

#' @title Filter Features: \dQuote{rank.correlation}
#'
#' @template cpo_doc_intro
#'
#' @description
#' The Spearman correlation between each feature and the target is used as an indicator
#' of feature importance. Rows with NA values are not taken into consideration.
#' @template arg_filter
#' @template cpo_doc_outro
#' @export
cpoFilterRankCorrelation = declareFilterCPO("rank.correlation")  # nolint
registerCPO(cpoFilterRankCorrelation, "featurefilter", "specialised", "Filter features using Spearman correlation.")

#' @title Filter Features: \dQuote{information.gain}
#'
#' @template cpo_doc_intro
#'
#' @description
#' Filter \dQuote{information.gain} uses the entropy-based information gain
#' between each feature and target individually as an importance measure.
#' @template arg_filter
#' @template cpo_doc_outro
#' @export
cpoFilterInformationGain = declareFilterCPO(c("FSelector_information.gain", "FSelector.information.gain", "information.gain"))  # nolint
registerCPO(cpoFilterInformationGain, "featurefilter", "specialised", "Filter features using entropy-based information gain.")

#' @title Filter Features: \dQuote{gain.ratio}
#'
#' @template cpo_doc_intro
#'
#' @description
#' Filter \dQuote{gain.ratio} uses the entropy-based information gain ratio
#' between each feature and target individually as an importance measure.
#' @template arg_filter
#' @template cpo_doc_outro
#' @export
cpoFilterGainRatio = declareFilterCPO(c("FSelector_gain.ratio", "gain.ratio"))  # nolint
registerCPO(cpoFilterGainRatio, "featurefilter", "specialised", "Filter features using entropy-based information gain ratio")

#' @title Filter Features: \dQuote{symmetrical.uncertainty}
#'
#' @template cpo_doc_intro
#'
#' @description
#' Filter \dQuote{symmetrical.uncertainty} uses the entropy-based symmetrical uncertainty
#' between each feature and target individually as an importance measure.
#' @template arg_filter
#' @template cpo_doc_outro
#' @export
cpoFilterSymmetricalUncertainty = declareFilterCPO(c("FSelector_symmetrical.uncertainty", "symmetrical.uncertainty"))  # nolint
registerCPO(cpoFilterSymmetricalUncertainty, "featurefilter", "specialised", "Filter features using entropy-based symmetrical uncertainty")

#' @title Filter Features: \dQuote{chi.squared}
#'
#' @template cpo_doc_intro
#'
#' @description
#' The chi-square test is a statistical test of independence to determine whether
#' two variables are independent. Filter \dQuote{chi.squared} applies this
#' test in the following way. For each feature the chi-square test statistic is
#' computed checking if there is a dependency between the feature and the target
#' variable. Low values of the test statistic indicate a poor relationship. High
#' values, i.e., high dependency identifies a feature as more important.
#' @template arg_filter
#' @template cpo_doc_outro
#' @export
cpoFilterChiSquared = declareFilterCPO(c("FSelector_chi.squared", "chi.squared"))  # nolint
registerCPO(cpoFilterChiSquared, "featurefilter", "specialised", "Filter features using chi-squared test.")

#' @title Filter Features: \dQuote{relief}
#'
#' @template cpo_doc_intro
#'
#' @description
#' Filter \dQuote{relief} is based on the feature selection algorithm \dQuote{ReliefF}
#' by Kononenko et al., which is a generalization of the orignal \dQuote{Relief}
#' algorithm originally proposed by Kira and Rendell. Feature weights are initialized
#' with zeros. Then for each instance \code{sample.size} instances are sampled,
#' \code{neighbours.count} nearest-hit and nearest-miss neighbours are computed
#' and the weight vector for each feature is updated based on these values.
#'
#' @references
#' Kira, Kenji and Rendell, Larry (1992). The Feature Selection Problem: Traditional
#' Methods and a New Algorithm. AAAI-92 Proceedings.
#'
#' Kononenko, Igor et al. Overcoming the myopia of inductive learning algorithms
#' with RELIEFF (1997), Applied Intelligence, 7(1), p39-55.
#' @template arg_filter
#' @template cpo_doc_outro
#' @export
cpoFilterRelief = declareFilterCPO(c("FSelector_relief", "relief"))  # nolint # missing parameters
registerCPO(cpoFilterRelief, "featurefilter", "specialised", "Filter features using the ReliefF algorithm.")

#' @title Filter Features: \dQuote{oneR}
#'
#' @template cpo_doc_intro
#'
#' @description
#' Filter \dQuote{oneR} makes use of a simple \dQuote{One-Rule} (OneR) learner to
#' determine feature importance. For this purpose the OneR learner generates one
#' simple association rule for each feature in the data individually and computes
#' the total error. The lower the error value the more important the correspoding
#' feature.
#' @template arg_filter
#' @template cpo_doc_outro
#' @export
cpoFilterOneR = declareFilterCPO(c("FSelector_oneR", "oneR"))  # nolint
registerCPO(cpoFilterOneR, "featurefilter", "specialised", "Filter features using the OneR learner.")

#' @title Filter Features: \dQuote{univariate.model.score}
#'
#' @template cpo_doc_intro
#'
#' @description
#' The \dQuote{univariate.model.score} feature filter resamples an \pkg{mlr}
#' learner specified via \code{perf.learner} for each feature individually
#' with randomForest from package \pkg{rpart} being the default learner.
#' Further parameter are the resamling strategey \code{perf.resampling} and
#' the performance measure \code{perf.measure}.
#' @param perf.learner [\code{\link[mlr:makeLearner]{Learner}} | \code{NULL}]\cr
#'   Learner to resample. If this is \code{NULL}, \code{regr.randomForest} is used.
#'   Default is \code{NULL}.
#' @param perf.measure [\code{\link[mlr:makeMeasure]{Measure}} | \code{NULL}]\cr
#'   Measure to use for resampling. If this is \code{NULL}, the Task's default Measure is used.
#'   Default is \code{NULL}.
#' @param perf.resampling [\code{ResampleDesc} or \code{ResampleInstance}]\cr
#'   Resampling strategy to use. If this is \code{NULL}, 2/3 holdout resampling is used.
#'   Default is \code{NULL}.
#' @template arg_filter
#' @template cpo_doc_outro
#' @export
cpoFilterUnivariate = declareFilterCPO("univariate.model.score",  # nolint
  .par.set = makeParamSet(
      makeUntypedLearnerParam("perf.learner", NULL),
      makeUntypedLearnerParam("perf.measure", NULL),
      makeUntypedLearnerParam("perf.resampling", NULL)))
registerCPO(cpoFilterUnivariate, "featurefilter", "specialised", "Filter features using the predictiveness using a given learner.")

#' @title Filter Features: \dQuote{anova.test}
#'
#' @template cpo_doc_intro
#'
#' @description
#' Filter \dQuote{anova.test} is based on the Analysis of Variance (ANOVA) between
#' feature and class. The value of the F-statistic is used as a measure of feature
#' importance.
#' @template arg_filter
#' @template cpo_doc_outro
#' @export
cpoFilterAnova = declareFilterCPO("anova.test")  # nolint
registerCPO(cpoFilterAnova, "featurefilter", "specialised", "Filter features using analysis of variance.")

#' @title Filter Features: \dQuote{kruskal.test}
#'
#' @template cpo_doc_intro
#'
#' @description
#' Filter \dQuote{kruskal.test} applies a Kruskal-Wallis rank sum test of the
#' null hypothesis that the location parameters of the distribution of a feature
#' are the same in each class and considers the test statistic as an variable
#' importance measure: if the location parameters do not differ in at least one
#' case, i.e., the null hypothesis cannot be rejected, there is little evidence
#' that the corresponding feature is suitable for classification.
#' @template arg_filter
#' @template cpo_doc_outro
#' @export
cpoFilterKruskal = declareFilterCPO("kruskal.test")  # nolint
registerCPO(cpoFilterKruskal, "featurefilter", "specialised", "Filter features using the Kruskal-Wallis rank sum test.")

#' @title Filter Features: \dQuote{variance}
#'
#' @template cpo_doc_intro
#'
#' @description
#' Simple filter based on the variance of the features indepentent of each other.
#' Features with higher variance are considered more important than features with
#' low importance.
#' @template arg_filter
#' @template cpo_doc_outro
#' @export
cpoFilterVariance = declareFilterCPO("variance")  # nolint
registerCPO(cpoFilterVariance, "featurefilter", "specialised", "Filter features using feature variance.")

#' @title Filter Features: \dQuote{permutation.importance}
#'
#' @template cpo_doc_intro
#'
#' @description
#' Filter \dQuote{permutation.importance} computes a loss function between predictions made by a
#' learner before and after a feature is permuted.
#' @param imp.learner [\code{\link[mlr:makeLearner]{Learner}} | \code{character(1)}]\cr
#'   Specifies the learner to use when computing the permutation importance.
#' @param contrast [\code{function}]\cr
#'   Contrast: takes two numeric vectors and returns one (default is the difference).
#' @param aggregation [\code{function}]\cr
#'   Aggregation: takes a \code{numeric} and returns a \code{numeric(1)} (default is the mean).
#' @param measure [\code{\link[mlr:makeMeasure]{Measure}}]\cr
#'   Measure to use. Defaults to the default measure of the task.
#' @param nmc [\code{integer(1)}]\cr
#' @param replace [\code{logical(1)}]\cr
#'   Determines whether the feature being
#'   permuted is sampled with or without replacement.
#' @template arg_filter
#' @template cpo_doc_outro
#' @export
cpoFilterPermutationImportance = declareFilterCPO("permutation.importance",  # nolint
  .par.set = makeParamSet(
      makeUntypedLearnerParam("imp.learner"),
      makeUntypedLearnerParam("contrast", default = function (x, y) {x - y}),  # nolint
      makeUntypedLearnerParam("measure", default = NULL),
      makeFunctionLearnerParam("aggregation", default = mean),
      makeIntegerLearnerParam("nmc", lower = 0, default = 50),
      makeLogicalLearnerParam("replace", default = FALSE)))
registerCPO(cpoFilterPermutationImportance, "featurefilter", "specialised", "Filter features using predictiveness loss upon permutation of a variable.")









