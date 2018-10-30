# auxhelp.R contains documentation for CPO objects that don't fit in any
# particular function's documentation. E.g. CPOConstructor, CPO, etc.

#' @title Composable Preprocessing Operators
#'
#' @description
#' \bold{mlrCPO} is a toolset that enriches \code{mlr} with a diverse set of preprocessing operators.
#' Composable Preprocessing Operators (\dQuote{CPO}s) are first-class R objects that can be applied to
#' \code{data.frame}s and \code{mlr} \code{Task}s to modify data, they can be attached to \code{mlr}
#' \code{Learner}s to add preprocessing to machine learning algorithms, and they can be composed to form
#' preprocessing pipelines.
#'
#' \code{mlrCPO} focuses on preprocessing as part of automated machine learning pipelines. This means that
#' it is designed with the expectation that the same preprocessing options are applied to incoming training data,
#' and test data. A common mistake in machine learning is that a machine learning method is evaluated (e.g. using
#' resampling) on a dataset \emph{after} that dataset has been cleaned up and preprocessed in one go. The proper evaluation
#' would need to consider that the preprocessing of training data may not be influenced by any information contained
#' in the test data set. \code{mlrCPO} takes this duality into account by providing \code{\link{CPO}} objects that
#' run on training data, and which then create \code{\link{CPOTrained}} objects that can be used on test data (or entirely new
#' prediction data).
#'
#' This focus on preprocessing is the reason for a strict separation between \dQuote{Feature Operation} \code{CPO}s,
#' \dQuote{Target Operation} \code{CPO}s, and \dQuote{Retrafoless} \code{CPO}s (see \link{OperatingType}).
#' The first class only changes
#' (predictor) features of a dataset, and does so in a way reproducible on test data. The second class only changes
#' (outcome) target data of a dataset, and is then able to \code{\link{invert}} the prediction, made by a learner
#' on new data, back to the space of the original target data. The \dQuote{Retrafoless} \code{CPO} only operates
#' during training and may only add or subtract data rows (e.g. for SMOTE-ing or subsampling), without transforming
#' the space of either predictor or outcome variables.
#'
#' \code{CPO}'s design is supposed to help its user avoid bugs and errors. Therefore it often avoids doing things
#' implicitly and relies on explicit commands e.g. for removing data or converting between datatypes. It has certain
#' restrictions in place (e.g. \link{CPOProperties}, \link{CPOTrainedCapability}) that try to make it hard to do the
#' wrong thing while not being in the way of the right thing.
#'
#' Other packages with similar, partially overlapping functionality are \href{https://cran.r-project.org/package=recipes}{recipes},
#' \href{https://cran.r-project.org/package=dplyr}{dplyr}, and \href{https://cran.r-project.org/package=caret}{caret}.
#'
#' @name mlrCPO-package
NULL

#' @title Constructor for CPO Objects
#'
#' @description
#' \code{\link{CPO}} objects are created by calling \code{CPOConstructor}s, which are
#' R functions that have some parameters in common, use a convenient \code{\link{print.CPOConstructor}} generic,
#' and always return a \code{\link{CPO}} object. The mlrCPO package provides many \code{CPOConstructor}
#' functions, which can be listed using \code{\link{listCPO}}. It is also possible to
#' create custom \code{\link{CPOConstructor}}s using \code{\link{makeCPO}}, \code{\link{makeCPORetrafoless}},
#' \code{link{makeCPOTargetOp}}, and \code{\link{makeCPOExtendedTrafo}}.
#'
#' @section CPO creation:
#' CPOConstructors can be called like any R function, with any parameters given. Besides parameters that are
#' common to most CPOConstructors (listed below), it is possible to set CPO-specific hyperparameters in the
#' construction. Parameters that are being \emph{exported}  can also be modified later using the \code{\link{CPO}}
#' object, see the documentation there.
#'
#' @section \code{affect.*} parameters:
#' When creating a \code{\link{CPO}}, it is possible to choose which columns of the given data the CPO operates
#' on, and which columns it will ignore. This is done using the \code{affect.*} parameters. It is possible to
#' choose columns by types, indices, names, or a regular expression matching names.
#'
#' @param id [\code{character(1)} | \code{NULL}]\cr
#'   ID to use for the CPO. if \code{NULL} is given, this defaults to
#'   a name describing the action performed by the CPO, which can be retrieved using \code{\link{getCPOName}}.
#'   The ID is used to identify the CPO in print messages,
#'   and is prefixed to the CPO's hyperparameter names. This is can be used to avoid name clashes when composing
#'   a CPO with another CPO or \code{\link[mlr:makeLearner]{Learner}} with hyperparameters with clashing names. Default is \code{NULL}.
#' @param export [\code{character}]\cr
#'   Which hyperparameters to export. This can be a character vector naming the
#'   hyperparameters to export (\emph{excluding} the ID), or a \code{character(1)} with one of the special values:
#'   \tabular{ll}{
#'     \dQuote{export.all}            \tab  export all parameters                             \cr
#'     \dQuote{export.default}        \tab  exp. params that are exp. by def                  \cr
#'     \dQuote{export.set}            \tab  exp. params set in construct call                 \cr
#'     \dQuote{export.default.set}    \tab  intersection of \dQuote{default} and \dQuote{set} \cr
#'     \dQuote{export.unset}          \tab  params \emph{not} set in construct call           \cr
#'     \dQuote{export.default.unset}  \tab  isct. of \dQuote{default} and \dQuote{unset}      \cr
#'     \dQuote{export.all.plus}       \tab  not yet supported
#'   }
#'   Default is \dQuote{export.default}.
#' @param affect.type [\code{character} | \code{NULL}]\cr
#'   Type of columns to affect. May be a subset of \dQuote{numeric}, \dQuote{factor}, \dQuote{ordered}, \dQuote{other},
#'   or can be or \code{NULL} to match all columns. Default is \code{NULL}.
#' @param affect.index [\code{numeric}]\cr
#'   Indices of feature columns to affect. The order of indices given is respected. Default is \code{integer(0)}.
#' @param affect.names [\code{character}]\cr
#'   Feature names of feature columns to affect. The order of names given is respected. Default is \code{character(0)}.
#' @param affect.pattern [\code{character(1)} | \code{NULL}]\cr
#'   \code{\link[base]{grep}} pattern to match feature names by. Default is \code{NULL} (no pattern matching)
#' @param affect.invert [\code{logical(1)}]\cr
#'   Whether to affect all features \emph{not} matched by other \code{affect.*} parameters. Default is \code{FALSE}.
#' @param affect.pattern.ignore.case [\code{logical(1)}]\cr
#'   Ignore case when matching features with \code{affect.pattern}; see \code{\link[base]{grep}}. Has no effect when
#'   \code{affect.pattern} is \code{NULL}. Default is \code{FALSE}.
#' @param affect.pattern.perl [\code{logical(1)}]\cr
#'   Use Perl-style regular expressions for \code{affect.pattern}; see \code{\link[base]{grep}}. Has no effect when
#'   \code{affect.pattern} is \code{NULL}, or when \code{affect.pattern.fixed} is \code{TRUE}. Default is \code{FALSE}.
#' @param affect.pattern.fixed [\code{logical(1)}]\cr
#'   Use fixed matching instead of regular expressions for \code{affect.pattern}; see \code{\link[base]{grep}}.
#'   Has no effect when \code{affect.pattern} is \code{NULL}. Default is \code{FALSE}.
#' @return [\code{\link{CPO}}] the constructed CPO.
#'
#' @seealso \code{\link{print.CPOConstructor}} for possibly verbose printing.
#' @family CPO lifecycle related
#' @family CPOConstructor related
#' @examples
#' class(cpoPca)  # c("CPOConstructor", "function")
#' print(cpoPca)  # default printer
#' print(cpoPca, verbose = TRUE)  # shows the trafo / retrafo functions
#'
#' cpoPca()  # creating a CPO
#' class(cpoPca())  # c("CPOPrimitive", "CPO")
#'
#' @name CPOConstructor
NULL

#' @title Composable Preprocessing Operators
#'
#' @description
#' Composable Preprocessing Operators, or \code{CPO}, are the central entity provided by the \code{mlrCPO} package.
#' CPOs can perform operations on a \code{\link[base]{data.frame}} or a \code{\link[mlr]{Task}}, for the latter even
#' modifying target values and converting between different \code{\link[mlr]{Task}} types.
#'
#' CPOs can be \dQuote{composed} using the \code{\link{\%>>\%}} operator, the \code{\link{composeCPO}} function, or
#' the \code{\link{pipeCPO}} function, to create new (\dQuote{compound}) operators that perform multiple operations
#' in a pipeline. While all CPOs have the class \dQuote{CPO}, primitive (i.e. not compound) CPOs have the additional class
#' \dQuote{CPOPrimitive}, and compound CPOs have the class \dQuote{CPOPipeline}. It is possible to split a compound CPOs
#' into its primitive constituents using \code{\link{as.list.CPO}}.
#'
#' CPOs can be \dQuote{attached} to a mlr-\code{\link[mlr:makeLearner]{Learner}} objects to create \code{\link{CPOLearner}}s,
#' using the \code{\link{\%>>\%}} operator, or the \code{\link{attachCPO}} function. These \code{\link{CPOLearner}}s
#' fit the model specified by the \code{\link[mlr:makeLearner]{Learner}} to the data after applying the attached CPO. Many CPOs can
#' be attached to a \code{\link[mlr:makeLearner]{Learner}} sequentially, or in form of a compound CPO.
#'
#' CPOs can be \dQuote{applied} to a \code{\link[base]{data.frame}} or a \code{\link[mlr]{Task}} using the
#' \code{\link{\%>>\%}} operator, or the \code{\link{applyCPO}} function. Applying a CPO performs the operations specified
#' by the (possibly compound) CPO, and returns the modified data. This data also contains a \dQuote{retrafo} and and
#' \dQuote{inverter} tag, which can be accessed using the \code{\link{retrafo}} and \code{\link{inverter}} functions to
#' get \code{\link{CPORetrafo}} and \code{\link{CPOInverter}} objects, respectively. These objects represent the \dQuote{trained}
#' CPOs that can be used when performing validation or predictions with new data.
#'
#' @section Hyperparameters:
#' CPOs can have hyperparameters that determine how they operate on data. These hyperparameters can be set during
#' construction, as function parameters of the \code{\link{CPOConstructor}}, or they can potentially be modified
#' later as exported hyperparameters. Which hyperparameters are exported is controlled using the \code{export} parameter
#' of the \code{\link{CPOConstructor}} when the CPO was created. Hyperparameters can be listed using \code{\link[mlr]{getParamSet}},
#' queried using \code{\link[mlr]{getHyperPars}} and set using \code{\link[mlr]{setHyperPars}}.
#'
#' @section S3 properties:
#' A CPO object should be treated as an opaque object and should only be queried / modified using the given \code{set*} and
#' \code{get*} functions. A list of them is given below in the section \dQuote{See Also}--\dQuote{cpo-operations}.
#'
#' @section Special CPO:
#' A special CPO is \code{\link{NULLCPO}}, which functions as the neutral element of the \code{\link{\%>>\%}} operator
#' and represents the identity operation on data.
#'
#' @seealso \code{\link{print.CPO}} for possibly verbose printing.
#'
#' @family CPO lifecycle related
#' @family operators
#' @family getters and setters
#' @family CPO classifications
#' @examples
#' class(cpoPca())  # c("CPOPrimitive", "CPO")
#' class(cpoPca() %>>% cpoScale())  # c("CPOPipeline", "CPO")
#' print(cpoPca() %>>% cpoScale(), verbose = TRUE)
#'
#' getHyperPars(cpoScale(center = FALSE))
#'
#' head(getTaskData(iris.task %>>% cpoScale()))
#' @name CPO
NULL

#' @title CPO Learner Object
#'
#' @description
#' CPO Learners are created when a \code{\link{CPO}} gets attached to an mlr-\code{\link[mlr:makeLearner]{Learner}} object. The resulting
#' learner performs the operation described by the attached \code{\link{CPO}} before fitting the model specified by the
#' \code{\link[mlr:makeLearner]{Learner}}. It is possible to attach compound CPOs, and it is possible to attach more CPOs to a learner
#' that is already a \code{CPOLearner}. If the attached CPO exports hyperparameters, these become part of the newly created
#' learner and can be queried and set using functions such as \code{\link[mlr]{getParamSet}}, \code{\link[mlr]{getHyperPars}},
#' and \code{\link[mlr]{setHyperPars}}.
#'
#' The model created when training a \code{CPOLearner} also contains the relevant \code{\link{CPORetrafo}} information to be applied
#' to prediction data; this can be retrieved using \code{\link{retrafo}}. The \code{\link{CPOInverter}} functionality is handled
#' equally transparently by the model.
#'
#' A CPOLearner can possibly have different \code{\link[mlr]{LearnerProperties}} than the base \code{\link[mlr:makeLearner]{Learner}} to which
#' it is attached. This depends on the \code{\link{CPO}}'s properties, see \code{\link{CPOProperties}}.
#'
#' It is possible to retrieve the \code{CPOLearner}'s base learner using \code{\link{getLearnerBare}}, and to get the attached CPOs
#' using \code{\link{getLearnerCPO}}.
#'
#'
#' @family CPO lifecycle related
#' @family CPOLearner related
#' @examples
#' lrn = makeLearner("classif.logreg")
#' cpolrn = cpoScale() %>>% lrn
#' print(cpolrn)
#'
#' getLearnerBare(cpolrn)  # classif.logreg Learner
#' getLearnerCPO(cpolrn)  # cpoScale() CPO
#'
#' getParamSet(cpolrn)  # includes cpoScale hyperparameters
#'
#' model = train(cpolrn, pid.task)  # behaves like a learner
#' retrafo(model)  # the CPORetrafo that was trained
#'
#' predict(model, pid.task)  # otherwise behaves like an mlr model
#'
#' @name CPOLearner
NULL



