#' @title Create a Custom CPO Constructor
#'
#' @description
#' \code{makeCPO} creates a \emph{Feature Operation} \code{\link{CPOConstructor}}, i.e. a constructor for a \code{\link{CPO}} that will
#' operate on feature columns. \code{makeCPOTargetOp} creates a \emph{Target Operation} \code{\link{CPOConstructor}}, which
#' creates \code{\link{CPO}}s that operate on the target column. \code{makeCPORetrafoless} creates a \emph{Retrafoless} \code{\link{CPOConstructor}},
#' which creates \code{\link{CPO}}s that may operate on both feature and target columns, but have no retrafo operation. See \link{OperatingType} for further
#' details on the distinction of these. \code{makeCPOExtendedTrafo} creates a \emph{Feature Operation} \code{\link{CPOConstructor}} that
#' has slightly more flexibility in its data transformation behaviour than \code{makeCPO} (but is otherwise identical).
#' \code{makeCPOExtendedTargetOp} creates a \emph{Target Operation} \code{\link{CPOConstructor}} that has slightly more flexibility in its
#' data transformation behaviour than \code{makeCPOTargetOp} but is otherwise identical.
#'
#' See example section for some simple custom CPO.
#'
#' @section CPO Internals:
#' The mlrCPO package offers a powerful framework for handling the tasks necessary for preprocessing, so that the user, when creating custom CPOs,
#' can focus on the actual data transformations to perform. It is, however, useful to understand \emph{what} it is that the framework does, and how
#' the process can be influenced by the user during CPO definition or application. Aspects of preprocessing that the user needs to influence are:
#' \describe{
#'   \item{\strong{Operating Type}}{
#'     The core of preprocessing is the actual transformation being performed. In the most general sense, there are three points in a machine
#'     learning pipeline that preprocessing can influence.
#'     \enumerate{
#'       \item Transformation of training data \emph{before model fitting}, done in mlr using \code{\link[mlr]{train}}. In the CPO framework
#'         (\emph{when not using a \code{\link{CPOLearner}} which makes all of these steps transparent to the user}), this is
#'         done by a \code{\link{CPO}}.
#'       \item transformation of new validation or prediction data that is given to the fitted model for \emph{prediction}, done using
#'         \code{\link[stats]{predict}}. This is done by a \code{\link{CPORetrafo}} retrieved using \code{\link{retrafo}} from the result of step 1.
#'       \item transformation of the predictions made to invert the transformation of the target values done in step 1, which is done using
#'         the \code{\link{CPOInverter}} retrieved using \code{\link{inverter}} from the result of step 2.
#'     }
#'     The framework poses restrictions on primitive (i.e. not compound using \code{\link{composeCPO}}) \code{\link{CPO}}s to simplify internal
#'     operation: A \code{\link{CPO}} may be one of three \link{OperatingType}s (see there). The \emph{Feature Operation} \code{\link{CPO}} does not
#'     transform target columns and hence only needs to be involved in steps 1 and 2. The \emph{Target Operation} \code{\link{CPO}} only transforms
#'     target columns, and therefore mostly concerns itself with steps 1 and 3. A \emph{Retrafoless} \code{\link{CPO}} may change both feature and
#'     target columns, but may not perform a retrafo \emph{or} inverter operation (and is therefore only concerned with step 1). Note that this
#'     is effectively a restriction on what kind of transformation a Retrafoless CPO may perform: it must not be a transformation of the data
#'     or target \emph{space}, it may only act or subtract points within this space.
#'
#'     The Operating Type of a \code{\link{CPO}} is ultimately dependent on the function that was used to create the \code{\link{CPOConstructor}}:
#'     \code{makeCPO} / \code{makeCPOExtendedTrafo}, \code{makeCPOTargetOp} / \code{makeCPOExtendedTargetOp}, or \code{makeCPORetrafoless}.}
#'   \item{\strong{Data Transformation}}{
#'     At the core of a CPO is the modification of data it performs. For Feature Operation CPOs, the transformation of each row,
#'     during training \emph{and} prediction, should
#'     happen in the same way, and it may only depend on the entirety of the \emph{training} data--i.e. the value of a data row in a prediction
#'     data set may not influence the transformation of a different prediction data row. Furthermore, if a data row occurs in both training and prediction
#'     data, its transformation result should ideally be the same.
#'
#'     This property is ensured by \code{makeCPO} by splitting the transformation
#'     into two functions: One function that collects all relevant information from the training data (called \code{cpo.train}), and one that transforms
#'     given data, using this collected information and (\emph{potentially new, unseen}) data to be transformed (called \code{cpo.retrafo}). The \code{cpo.retrafo}
#'     function should handle all data as if it were prediction data and unrelated to the data given to \code{cpo.train}.
#'
#'     Internally, when a \code{\link{CPO}} gets applied to a data set using \code{\link{applyCPO}}, the \code{cpo.train} function is called, and the
#'     resulting control object is used for a subsequent \code{cpo.retrafo} call which transforms the data. Before the result is given back from the
#'     \code{\link{applyCPO}} call, the control object is used to create a \code{\link{CPORetrafo}} object,
#'     which is attached to the result as attribute. Target Operating CPOs additionally create and add a \code{\link{CPOInverter}} object.
#'
#'     When a \code{\link{CPORetrafo}} is then applied to new prediction data, the control object previously returned by \code{cpo.train} is given,
#'     combined with this \emph{new} data, to another \code{cpo.retrafo} call that performs the new transformation.
#'
#'     \code{makeCPOExtendedTrafo} gives more flexibility by having calling only the \code{cpo.trafo} in the training step, which both creates a control
#'     object \emph{and} modifies the data. This can increase performance if the underlying operation creates a control object and the transformed data in one step,
#'     as for example \emph{PCA} does. Note that the requirement that the same row in training and prediction data should result in the same transformation
#'     result still stands. The \code{cpo.trafo} function returns the transformed data and creates a local variable with the control information, which the
#'     CPO framework will access.}
#'   \item{\strong{Inversion}}{
#'     If a \code{\link{CPO}} performs transformations of the \emph{target} column, the predictions made by a following machine learning process should
#'     ideally have this transformation undone, so that if the process makes a prediction that coincides with a target value \emph{after} the
#'     transformation, the whole pipeline should return a prediction that equals to the target value \emph{before} this transformation.
#'
#'     This is done by the \code{cpo.invert} function given to \code{makeCPOTargetOp}. It has access to information from both the preceding training and prediction
#'     steps. During the training step, \code{cpo.train} createas a \code{control} object that is not only given to \code{cpo.retrafo}, but also
#'     to \code{cpo.train.invert}. This latter function is called before the prediction step, whenever new data is fed to the machine learning process.
#'     It takes the new data and the old \code{control} object and transforms it to a new \code{control.invert} object to include information about the prediction
#'     data. This object is then given to \code{cpo.invert}.
#'
#'     It is possible to have Target Operation CPOs that do not require information from the retrafo step. This is specified by setting
#'     \code{constant.invert} to \code{TRUE}. It has the advantage that the same \code{\link{CPOInverter}}
#'     can be used for inversion of predictions made with any new data. Otherwise, a new \code{\link{CPOInverter}} object must be obtained for each
#'     new data set after the retrafo step (using the \code{\link{inverter}} function on the retrafo result). Having \code{constant.invert} set to \code{TRUE}
#'     results in \emph{hybrid} retrafo / inverter objects: The \code{\link{CPORetrafo}} object can then also be used for \code{inversions}.
#'     When defining a \code{constant.invert} Target Operating CPO, no \code{cpo.train.invert} function is given, and the same \code{control}
#'     object is given to both \code{cpo.retrafo} and \code{cpo.invert}.
#'
#'     \code{makeCPOExtendedTargetOp} gives more flexibility and allows more efficient implementation of Target Operating CPOs at cost of more complexity.
#'     With this method, a \code{cpo.trafo} function is given that is executed during the first training step; It must return the transformed target column,
#'     as well as a \code{control} and \code{control.invert} object. The \code{cpo.retrafo} function not only transforms the target, but must also
#'     create a new \code{control.invert} object (unless \code{constant.invert} is \code{TRUE}). The semantics of \code{cpo.invert} is identical with the
#'     basic \code{makeCPOTargetOp}.}
#'   \item{\strong{\code{cpo.train}-\code{cpo.retrafo} information transfer}}{
#'     One possibility to transfer information from \code{cpo.train} to \code{cpo.retrafo} is to have \code{cpo.train} return a
#'     control object (a \code{\link[base]{list}})
#'     that is then given to \code{cpo.retrafo}. The CPO is then called an \emph{object based} CPO.
#'
#'     Another possibility is to not give the \code{cpo.retrafo}
#'     argument (set it to \code{NULL} in the \code{makeCPO} call) and have \code{cpo.train} instead return a \emph{function} instead. This function is then
#'     used as the \code{cpo.retrafo} function, and should have access to all relevant information about the training data as a closure. This is called
#'     \emph{functional} CPO. To save memory, the actual data (including target) given to \code{cpo.train} is removed from the environment of its
#'     return value in this case
#'     (i.e. the environment of the \code{cpo.retrafo} function). This means the \code{cpo.retrafo} function may not reference a \dQuote{\code{data}} variable.
#'
#'     There are similar possibilities of functional information transfer for other types of CPOs: \code{cpo.trafo} in \code{makeCPOExtendedTargetOp} may
#'     create a \code{cpo.retrafo} function instead of a \code{control} object. \code{cpo.train} in \code{makeCPOTargetOp} has the option of creating
#'     a \code{cpo.retrafo} and \code{cpo.train.invert} (\code{cpo.invert} if \code{constant.invert} is \code{TRUE}) function (and returning \code{NULL})
#'     instead of returning a \code{control} object. Similarly, \code{cpo.train.invert} may return a \code{cpo.invert} function instead of a \code{control.invert}
#'     object. In \code{makeCPOExtendedTargetOp}, \code{cpo.trafo} may create a \code{cpo.retrafo} or a \code{cpo.invert} function, each optionally instead
#'     of a \code{control} or \code{control.invert} object (one \emph{or} both may be functional). \code{cpo.retrafo} similarly may create a \code{cpo.invert}
#'     function instead of giving a \code{control.invert} object. Functional information transfer may be more parsimonious and elegant than control
#'     object information transfer.}
#'   \item{\strong{Hyperparameters}}{
#'     The action performed by a CPO may be influenced using \emph{hyperparameters}, during its construction as well as afterwards (then using
#'     \code{\link[mlr]{setHyperPars}}). Hyperparameters must be specified as a \code{\link[ParamHelpers:makeParamSet]{ParamSet}} and given as argument \code{par.set}.
#'     Default values for each parameter may be specified in this \code{\link[ParamHelpers:makeParamSet]{ParamSet}} or optionally as another argument \code{par.vals}.
#'
#'     Hyperparameters given are made part of the \code{\link{CPOConstructor}} function and can thus be given during construction.
#'     Parameter default values function as the default values for the \code{\link{CPOConstructor}} function parameters (which are thus made optional function
#'     parameters of the \code{\link{CPOConstructor}} function). The CPO framework handles storage and changing of hyperparameter values.
#'     When the \code{cpo.train} and \code{cpo.retrafo} functions are called to transform data, the hyperparameter values are given to them as arguments, so
#'     \code{cpo.train} and \code{cpo.retrafo} functions must be able to accept these parameters, either directly, or with a \code{...} argument.
#'
#'     Note that with \emph{functional} \code{\link{CPO}}s, the \code{cpo.retrafo} function does not take hyperparameter arguments (and instead can usually
#'     refer to them by its environment).
#'
#'     Hyperparameters may be \emph{exported} (or not), thus making them available for \code{\link[mlr]{setHyperPars}}. Not exporting a parameter
#'     has advantage that it does not clutter the \code{\link[ParamHelpers:makeParamSet]{ParamSet}} of a big \code{\link{CPO}} or \code{\link{CPOLearner}} pipeline with
#'     many hyperparameters. Which hyperparameters are exported is chosen during the constructing call of a \code{\link{CPOConstructor}}, but the default
#'     exported hyperparameters can be chosen with the \code{export.params} parameter.}
#'   \item{\strong{Properties}}{
#'     Similarly to \code{\link[mlr:makeLearner]{Learner}}s, \code{\link{CPO}}s may specify what kind of data they are and are not able to handle. This is done by
#'     specifying \code{.properties.*} arguments. The names of possible properties are the same as possible \code{\link[mlr]{LearnerProperties}}, but since
#'     \code{\link{CPO}}s mostly concern themselves with data, only the properties indicating column and task types are relevant.
#'
#'     For each \code{\link{CPO}} one must specify
#'     \enumerate{
#'       \item which kind of data does the \code{\link{CPO}} handle,
#'       \item which kind of data must the \code{\link{CPO}} or \code{\link[mlr:makeLearner]{Learner}} be able to handle that comes \emph{after}
#'         the given \code{\link{CPO}}, and
#'       \item which kind of data handling capability does the given \code{\link{CPO}} \emph{add} to a following
#'         \code{\link{CPO}} or \code{\link[mlr:makeLearner]{Learner}} if coming before it in a pipeline.
#'     }
#'     The specification of (1) is done with \code{properties.data} and \code{properties.target}, (2) is specified using \code{properties.needed}, and
#'     (3) is specified using \code{properties.adding}. Internally, \code{properties.data} and \code{properties.target} are concatenated and treated as
#'     one vector, they are specified separately in \code{makeCPO} etc. for convenience reasons. See \code{\link{CPOProperties}} for details.
#'
#'     The CPO framework checks the \code{cpo.retrafo} etc. functions for adherence to these properties, so it e.g. throws an error if a \code{cpo.retrafo}
#'     function adds missing values to some data but didn't declare \dQuote{missings} in \code{properties.needed}. It may be desirable to have this
#'     internal checking happen to a laxer standard than the property checking when composing CPOs (e.g. when a CPO adds missings only with certain
#'     hyperparameters, one may still want to compose this CPO to another one that can't handle missings). Therefore it is possible to postfix
#'     listed properties with \dQuote{.sometimes}. The internal CPO checking will ignore these when listed in \code{properties.adding}
#'     (it uses the \sQuote{minimal} set of adding properties, \code{adding.min}), and it will not declare them externally when listed in
#'     \code{properties.needed} (but keeps them internally in the \sQuote{maximal} set of needed properties, \code{needed.max}). The \code{adding.min}
#'     and \code{needed.max} can be retrieved using \code{\link{getCPOProperties}} with \code{get.internal = TRUE}.}
#'   \item{\strong{Data Format}}{
#'     Different CPOs may want to change different aspects of the data, e.g. they may only care about numeric columns, they may or may not care about
#'     the target column values, sometimes they might need the actual task used as input. The CPO framework offers to present the data in a specified
#'     formats to the \code{cpo.train}, \code{cpo.retrafo} and other functions, to reduce the need for boilerplate data subsetting on the user's part. The format is
#'     requested using the \code{dataformat} and \code{dataformat.factor.with.ordered} parameter. A \code{cpo.retrafo} function is expected to return
#'     data in the same format as it requested, so if it requested a \code{\link[mlr]{Task}}, it must return one, while if it only
#'     requested the feature \code{data.frame}, a \code{data.frame} must be returned.}
#'   \item{\strong{Task Conversion}}{
#'     Target Operation CPOs can be used for conversion between \code{\link[mlr]{Task}}s. For this, the \code{type.out} value must be given. Task conversion
#'     works with all values of \code{dataformat} and is handled by the CPO framework. The \code{cpo.trafo} function must take care to return the target data
#'     in a proper format (see above). Note that for conversion, not only does the \code{\link[mlr]{Task}} type need to be changed during \code{cpo.trafo}, but
#'     also the \emph{prediction} format (see above) needs to change.}
#'   \item{\strong{Fix Factors}}{
#'     Some preprocessing for factorial columns needs the factor levels to be the same during training and prediction. This is usually not guarranteed
#'     by mlr, so the framework offers to do this if the \code{fix.factors} flag is set.}
#'   \item{\strong{ID}}{
#'     To prevent parameter name clashes when \code{\link{CPO}}s are concatenated, the parameters are prefixed with the \code{\link{CPO}}s
#'     \emph{\link[=getCPOId]{id}}.
#'     The ID can be set during \code{\link{CPO}} construction, but will default to the \code{\link{CPO}}s \emph{name} if not given. The name is set
#'     using the \code{cpo.name} parameter.}
#'   \item{\strong{Packages}}{
#'     Whenever a \code{\link{CPO}} needs certain packages to be installed to work, it can specify these in the \code{packages} parameter. The framework
#'     will check for the availability of the packages and throw an error if not found \emph{during construction}. This means that loading a \code{\link{CPO}}
#'     from a savefile will omit this check, but in most cases it is a sufficient measure to make the user aware of missing packages in time.}
#'   \item{\strong{Target Column Format}}{
#'     Different \code{\link[mlr]{Task}} types have the target in a different formats. They are listed here for reference. Target data is in this format
#'     when given to the \code{target} argument of some functions, and must be returned in this format by \code{cpo.trafo}
#'     in Target Operation CPOs. Target values are always in the format of a \code{\link[base]{data.frame}}, even when only one column.
#'     \tabular{ll}{
#'       \bold{Task type}    \tab \bold{target format}                          \cr
#'       \dQuote{classif}    \tab one column of \code{\link[base]{factor}}      \cr
#'       \dQuote{cluster}    \tab \code{data.frame} with zero columns.          \cr
#'       \dQuote{multilabel} \tab several columns of \code{\link[base]{logical}}\cr
#'       \dQuote{regr}       \tab one column of \code{\link[base]{numeric}}     \cr
#'       \dQuote{surv}       \tab two columns of \code{\link[base]{numeric}}    \cr
#'     }
#'
#'     When inverting, the format of the \code{target} argument, as well as the return value of, the \code{cpo.invert} function depends on the
#'     \code{\link[mlr]{Task}} type as well as the \code{predict.type}. The requested return value \code{predict.type} is given to the \code{cpo.invert} function
#'     as a parameter, the \code{predict.type} of the \code{target} parameter depends on this and the \code{predict.type.map} (see \link{PredictType}).
#'     The format of the prediction, depending on the task type and \code{predict.type}, is:
#'     \tabular{lll}{
#'       \bold{Task type}    \tab \bold{\code{predict.type}} \tab \bold{target format}                          \cr
#'       \dQuote{classif}    \tab \dQuote{response}          \tab \code{\link[base]{factor}}                    \cr
#'       \dQuote{classif}    \tab \dQuote{prob}              \tab \code{\link[base]{matrix}} with nclass cols   \cr
#'       \dQuote{cluster}    \tab \dQuote{response}          \tab \code{\link[base]{integer}} cluster index     \cr
#'       \dQuote{cluster}    \tab \dQuote{prob}              \tab \code{\link[base]{matrix}} with nclustr cols  \cr
#'       \dQuote{multilabel} \tab \dQuote{response}          \tab \code{\link[base]{logical}} \code{\link[base]{matrix}} \cr
#'       \dQuote{multilabel} \tab \dQuote{prob}              \tab \code{\link[base]{matrix}} with nclass cols   \cr
#'       \dQuote{regr}       \tab \dQuote{response}          \tab \code{\link[base]{numeric}}                   \cr
#'       \dQuote{regr}       \tab \dQuote{se}                \tab 2-col \code{\link[base]{matrix}}              \cr
#'       \dQuote{surv}       \tab \dQuote{response}          \tab \code{\link[base]{numeric}}                   \cr
#'       \dQuote{surv}       \tab \dQuote{prob}              \tab [NOT YET SUPPORTED]                           \cr
#'     }
#'     All \code{\link[base]{matrix}} formats are \code{\link[base]{numeric}}, unless otherwise stated.}
#' }
#'
#' @section Headless function definitions:
#' In the place of all \code{cpo.*} arguments, it is possible to make a \emph{headless} function definition, consisting only of the function body.
#' This function body must always begin with a \sQuote{\code{\{}}. For example, instead of
#' \code{cpo.retrafo = function(data, control) data[-1]}, it is possible to use
#' \code{cpo.retrafo = function(data, control) \{ data[-1] \}}. The necessary function head is then added automatically by the CPO framework.
#' This will always contain the necessary parameters (e.g. \dQuote{\code{data}}, \dQuote{\code{target}}, hyperparameters as defined in \code{par.set})
#' in the names as required. This can declutter the definition of a \code{\link{CPOConstructor}} and is recommended if the CPO consists of
#' few lines.
#'
#' Note that if this is used when writing an R package, inside a function, this may lead to the automatic R correctness checker to print warnings.
#'
#'
#' @param cpo.name [\code{character(1)}]\cr
#'   The name of the resulting \code{\link{CPOConstructor}} / \code{\link{CPO}}. This is used for identification in output,
#'   and as the default \code{\link[=getCPOId]{id}}.
#' @param par.set [\code{\link[ParamHelpers:makeParamSet]{ParamSet}}]\cr
#'   Optional parameter set, for configuration of CPOs during construction or by hyperparameters.
#'   Default is an empty \code{\link[ParamHelpers:makeParamSet]{ParamSet}}.
#'   It is recommended to use \code{\link{pSS}} to construct this, as it greatly reduces the verbosity of
#'   creating a \code{\link[ParamHelpers:makeParamSet]{ParamSet}} and makes it more readable.
#' @param par.vals [\code{list} | \code{NULL}]\cr
#'   Named list of default parameter values for the CPO. These are used \emph{instead of} the
#'   parameter default values in \code{par.set}, if not \code{NULL}. It is preferred to use
#'   \code{\link[ParamHelpers:makeParamSet]{ParamSet}} default values,
#'   and not \code{par.vals}. Default is \code{NULL}.
#' @param dataformat [\code{character(1)}]\cr
#'   Indicate what format the data should be as seen by the \code{cpo.train} and \code{cpo.retrafo} function.
#'   The following table shows what values of \code{dataformat} lead to what is given to \code{cpo.train} and \code{cpo.retrafo}
#'   as \code{data} and \code{target} parameter value. (Note that for Feature Operating CPOs, \code{cpo.retrafo} has no \code{target} argument.) Possibilities are:
#'   \tabular{lll}{
#'     \bold{dataformat}    \tab \bold{data}                            \tab \bold{target}                \cr
#'     \dQuote{df.all}       \tab \code{data.frame} with target cols     \tab target colnames              \cr
#'     \dQuote{df.features}  \tab \code{data.frame} without target       \tab \code{data.frame} of target  \cr
#'     \dQuote{task}         \tab full \code{\link[mlr]{Task}}           \tab target colnames              \cr
#'     \dQuote{split}        \tab list of \code{data.frames} by type     \tab \code{data.frame} of target  \cr
#'     [type]                \tab \code{data.frame} of [type] feats only \tab \code{data.frame} of target  \cr
#'   }
#'   [type] can be any one of \dQuote{factor}, \dQuote{numeric}, \dQuote{ordered}; if these are given, only a subset of the total
#'   data present is seen by the \code{\link{CPO}}.
#'
#'   Note that \code{makeCPORetrafoless} accepts only \dQuote{task} and \dQuote{df.all}.
#'
#'   For \code{dataformat == "split"}, \code{cpo.train} and \code{cpo.retrafo} get a list with entries \dQuote{factor}, \dQuote{numeric},
#'   \dQuote{other}, and, if \code{dataformat.factor.with.ordered} is \code{FALSE}, \dQuote{ordered}.
#'
#'   If the CPO is a Feature Operation CPO, then the return value of the \code{cpo.retrafo} function must be in the same format as the one requested.
#'   E.g. if \code{dataformat} is \dQuote{split}, the return value must be a named list with entries \code{$numeric},
#'   \code{$factor}, and \code{$other}. The types of the returned data may be arbitrary: In the given example,
#'   the \code{$factor} slot of the returned list may contain numeric data. (Note however that if data is returned
#'   that has a type not already present in the data, \code{properties.needed} must specify this.)
#'
#'   For Feature Operating CPOs, if \code{dataformat} is either \dQuote{df.all} or \dQuote{task}, the
#'   target column(s) in the returned value of the retrafo function must be identical with the target column(s) given as input.
#'
#'   If \code{dataformat} is \dQuote{split}, the \code{$numeric} slot of the value returned by the \code{cpo.retrafo} function
#'   may also be a \code{\link[base]{matrix}}. If \code{dataformat} is \dQuote{numeric}, the returned object may also be a
#'   matrix.
#'
#'   Default is \dQuote{df.features} for all functions except \code{makeCPORetrafoless}, for which it is \dQuote{df.all}.
#' @param dataformat.factor.with.ordered [\code{logical(1)}]\cr
#'   Whether to treat \code{ordered} typed features as \code{factor} typed features. This affects how \code{dataformat} is handled, for which it only
#'   has an effect if \code{dataformat} is \dQuote{split} or \dQuote{factor}. If \code{dataformat} is \dQuote{ordered}, this must be \code{FALSE}.
#'   It also affects how strictly data fed to a \code{\link{CPORetrafo}} object
#'   is checked for adherence to the data format of data given to the generating \code{\link{CPO}}. Default is \code{TRUE}.
#' @param export.params [\code{logical(1)} | \code{character}]\cr
#'   Indicates which CPO parameters are exported by default. Exported parameters can be changed after construction using \code{\link[mlr]{setHyperPars}},
#'   but exporting too many parameters may lead to messy parameter sets if many CPOs are combined using \code{\link{composeCPO}} or \code{\link{\%>>\%}}.
#'   The exported parameters can be set during construction, but \code{export.params} determines the \emph{default} exported parameters.
#'   If this is a \code{logical(1)}, \code{TRUE} exports all parameters, \code{FALSE} to exports no parameters. It may also be a \code{character},
#'   indicating the names of parameters to be exported. Default is \code{TRUE}.
#' @param fix.factors [\code{logical(1)}]\cr
#'   Whether to constrain factor levels of new data to the levels of training data, for each factorial or ordered column. If new data contains
#'   factors that were not present in training data, the values are set to \code{NA}. Default is \code{FALSE}.
#' @param properties.data [\code{character}]\cr
#'   The kind if data that the CPO will be able to handle. This can be one or more of: \dQuote{numerics},
#'   \dQuote{factors}, \dQuote{ordered}, \dQuote{missings}.
#'   There should be a bias towards including properties. If a property is absent, the preproc
#'   operator will reject the data. If an operation e.g. only works on numeric columns that have no
#'   missings (like PCA), it is recommended to give all properties, ignore the columns that
#'   are not numeric (using \code{dataformat = "numeric"}), and giving an error when
#'   there are missings in the numeric columns (since missings in factorial features are not a problem).
#'   Defaults to the maximal set.
#' @param properties.target [\code{character}]\cr
#'   For Feature Operation CPOs, this can be one or more of \dQuote{cluster}, \dQuote{classif}, \dQuote{multilabel}, \dQuote{regr}, \dQuote{surv},
#'   \dQuote{oneclass}, \dQuote{twoclass}, \dQuote{multiclass}. Just as \code{properties.data}, it
#'   indicates what kind of data a CPO can work with. To handle data given as \code{data.frame}, the \dQuote{cluster} property is needed. Default is the maximal set.
#'
#'   For Target Operation CPOs, this \emph{must} contain exactly one of \dQuote{cluster}, \dQuote{classif}, \dQuote{multilabel}, \dQuote{regr}, \dQuote{surv}.
#'   This indicates the type of \code{\link[mlr]{Task}} the
#'   \code{\link{CPO}} can work on. If the input is a \code{data.frame}, it is treated as a \dQuote{cluster} type \code{\link[mlr]{Task}}.
#'   If the \code{properties.target} contains \dQuote{classif}, the value must then also contain one or more of \dQuote{oneclass},
#'   \dQuote{twoclass}, or \dQuote{multiclass}. Default is \dQuote{cluster}.
#' @param properties.adding [\code{character}]\cr
#'   Can be one or many of the same values as \code{properties.data} for Feature Operation CPOs, and one or many of the same values as \code{properties.target}
#'   for Target Operation CPOs. These properties \emph{get added} to a \code{\link[mlr:makeLearner]{Learner}} (or \code{\link{CPO}}) coming after / behind this CPO.
#'   When a CPO imputes missing values, for example, this should be \dQuote{missings}. This must be a subset of \dQuote{properties.data} or
#'   \dQuote{properties.target}.
#'
#'   Note that this may \emph{not} contain a \code{\link[mlr]{Task}}-type property, even if the \code{\link{CPO}} is a Target Operation CPO that performs
#'   conversion.
#'
#'   Property names may be postfixed with \dQuote{.sometimes}, to indicate that adherence should not be checked internally. This distinction is made by
#'   not putting them in the \code{$adding.min} slot of the \code{\link{getCPOProperties}} return value when \code{get.internal = TRUE}.
#'
#'   Default is \code{character(0)}.
#' @param properties.needed [\code{character}]\cr
#'   Can be one or many of the same values as \code{properties.data} for Feature Operation CPOs,
#'   and one or many of the same values as \code{properties.target}. These properties are \emph{required}
#'   from a \code{\link[mlr:makeLearner]{Learner}} (or \code{\link{CPO}}) coming after / behind this CPO. E.g., when a CPO converts factors to
#'   numerics, this should be \dQuote{numerics} (and \code{properties.adding} should be \dQuote{factors}).
#'
#'   Note that this may \emph{not} contain a \code{\link[mlr]{Task}}-type property, even if the \code{\link{CPO}} is a Target Operation CPO that performs
#'   conversion.
#'
#'   Property names may be postfixed with \dQuote{.sometimes}, to indicate that adherence should not be checked internally. This distinction is made by
#'   not putting them in the \code{$needed} slot of properties. They can still be found in the \code{$needed.max} slot of the
#'   \code{\link{getCPOProperties}} return value when \code{get.internal = TRUE}.
#'
#'   Default is \code{character(0)}.
#' @param packages [\code{character}]\cr
#'   Package(s) that should be loaded when the CPO is constructed. This gives the user an error if
#'   a package required for the CPO is not available on his system, or can not be loaded. Default is \code{character(0)}.
#' @param constant.invert [\code{logical(1)}]\cr
#'   Whether the \code{cpo.invert} step should not have information from the previous \code{cpo.retrafo} or \code{cpo.train.invert} step in
#'   Target Operation CPOs (\code{makeCPOTargetOp} or \code{makeCPOExtendedTargetOp}).
#'
#'   For \code{makeCPOTargetOp}, if this is \code{TRUE}, the
#'   \code{cpo.train.invert} argument must be \code{NULL}. If \code{cpo.retrafo} and \code{cpo.invert} are given, the same \code{control}
#'   object is given to both of them. Otherwise, if \code{cpo.retrafo} and \code{cpo.invert} are \code{NULL}, the \code{cpo.train} function
#'   must return \code{NULL} and define a \code{cpo.retrafo} and \code{cpo.invert} function in its namespace (see \code{cpo.train} documentation
#'   for more details). If \code{constant.invert} is \code{FALSE}, \code{cpo.train} may either return a \code{control} object that will then be
#'   given to \code{cpo.train.invert}, or define a \code{cpo.retrafo} and \code{cpo.train.invert} function in its namespace.
#'
#'   For \code{makeCPOExtendedTargetOp}, if this is \code{TRUE}, \code{cpo.retrafo} does not need to generate a \code{control.invert} object.
#'   The \code{control.invert} object created in \code{cpo.trafo} will then always be given to \code{cpo.invert} for all data sets.
#'
#'   Default is \code{FALSE}.
#' @param predict.type.map [\code{character} | \code{list}]\cr
#'   This becomes the \code{\link{CPO}}'s \code{predict.type}, explained in detail in \link{PredictType}.
#'
#'   In short, the \code{predict.type.map} is a character vector, or a \code{list} of \code{character(1)},
#'   with \emph{names} according to the predict types \code{predict} can request
#'   in its \code{predict.type} argument when the created \code{\link{CPO}} was used as part of a \code{\link{CPOLearner}} to create the
#'   model under consideration. The \emph{values} of \code{predict.type.map} are the \code{predict.type} that will be requested from the
#'   underlying \code{\link[mlr:makeLearner]{Learner}} for prediction.
#'
#'   \code{predict.type.map} thus determines the format that the \code{target} parameter of \code{cpo.invert} can take: It is
#'   the format according to \code{predict.type.map[predict.type]}, where \code{predict.type} is the respective \code{cpo.invert} parameter.
#' @param task.type.out [\code{character(1)} | \code{NULL}]\cr
#'   If \code{\link[mlr]{Task}} conversion is to take place, this is the output task that the data should be converted to. Note that the
#'   CPO framework takes care of the conversion if \code{dataformat} is not \dQuote{task}, but the target column needs to have the
#'   proper format for that.
#'
#'   If this is \code{NULL}, \code{\link[mlr]{Task}}s will not be converted. Default is \code{NULL}.
#' @param cpo.train [\code{function} | \code{NULL}]\cr
#'   This is a function which must have the parameters \code{data} and \code{target},
#'   as well as the parameters specified in \code{par.set}. (Alternatively,
#'   the function may have only some of these arguments and a \code{\link[methods:dotsMethods]{dotdotdot}} argument).
#'   It is called whenever a \code{\link{CPO}} is applied to
#'   a data set to prepare for transformation of the training \emph{and} prediction data.
#'   Note that this function is only used in Feature Operating CPOs created with \code{makeCPO}, and in Target Operating CPOs
#'   created with \code{makeCPOExtendedTargetOp}.
#'
#'   The behaviour of this function differs slightly in Feature Operation and Target Operation CPOs.
#'
#'   For \bold{Feature Operation CPOs}, if \code{cpo.retrafo} is \code{NULL}, this is a constructor function which must return a \dQuote{retrafo} function which
#'   will then modify (possibly new unseen) data. This retrafo function must have exactly one argument--the (new) data--and return the modified data. The format
#'   of the argument, and of the return value of the retrafo function, depends on the value of the \code{dataformat} parameter, see documentation there.
#'
#'   If \code{cpo.retrafo} is not \code{NULL}, this is a function which must return a control object.
#'   This control object returned by \code{cpo.train} will then be given as the \code{control} argument of the \code{cpo.retrafo} function, along with
#'   (possibly new unseen) data to manipulate.
#'
#'   For \bold{Target Operation CPOs}, if \code{cpo.retrafo} is \code{NULL}, \code{cpo.train.invert}
#'   (or \code{cpo.invert} if \code{constant.invert} is \code{TRUE}) must likewise be \code{NULL}.
#'   In that case \code{cpo.train}'s return value is ignored and it must define, within its namespace, two
#'   functions \code{cpo.retrafo} and \code{cpo.train.invert} (or \code{cpo.invert} if \code{constant.invert}
#'   is \code{TRUE}) which will take the place of the respective functions. \code{cpo.retrafo} must take the
#'   parameters \code{data} and \code{target}, and return the modified target \code{target} (or \code{data},
#'   depending on \code{dataformat}) data. \code{cpo.train.invert} must take a \code{data} and \code{control}
#'   argument and return either a modified control object, or a \code{cpo.invert} function.
#'   \code{cpo.invert} must have a \code{target} and \code{predict.type} argument and return the modified
#'   target data.
#'
#'   If \code{cpo.retrafo} is not \code{NULL}, \code{cpo.train.invert}
#'   (or \code{cpo.invert} if \code{constant.invert} is \code{TRUE}) must likewise be non-\code{NULL}.
#'   In that case, \code{cpo.train} must return a control object. This control object will then be
#'   given as the \code{control} argument of both \code{cpo.retrafo} and \code{cpo.train.invert}
#'   (or the \code{control.invert} argument of \code{cpo.invert} if \code{constant.invert} is \code{TRUE}).
#'
#'   This parameter may be \code{NULL}, resulting in a so-called \emph{stateless} CPO. For Target Operation CPOs created with \code{makeCPOTargetOp},
#'   \code{constant.invert} must be \code{TRUE} in this case.
#'   A stateless CPO does the same transformation for initial CPO
#'   application and subsequent prediction data transformation (e.g. taking the logarithm of numerical columns). Note that \code{cpo.retrafo}
#'   and \code{cpo.invert} should not
#'   have a \code{control} argument in a stateless CPO.
#' @param cpo.trafo [\code{function}]\cr
#'   This is a function which must have the parameters \code{data} and \code{target},
#'   as well as the parameters specified in \code{par.set}. (Alternatively,
#'   the function may have only some of these arguments and a \code{\link[methods:dotsMethods]{dotdotdot}} argument).
#'   It is called whenever a \code{\link{CPO}} is applied to
#'   a data set to transform the training data, and (except for Retrafoless CPOs) to collect a control object used by other transformation functions.
#'   Note that this function is not used in \code{makeCPO}.
#'
#'   This functions primary task is to transform the given data when the \code{\link{CPO}} gets applied to training data. For Target Operating CPOs
#'   (created with \code{makeCPOExtendedTargetOp}(!)),
#'   it must return the complete transformed target column(s), unless \code{dataformat} is \dQuote{df.all} (in which case the complete, modified,
#'   \code{data.frame} must be returned) or \dQuote{task} (in which case the complete, modified, \code{Task} must be returned). It must furthermore
#'   create the control objects for \code{cpo.retrafo} and \code{cpo.invert}, or create these functins themselves, and save them in its function
#'   environment (see below). For Retrafoless CPOs
#'   (created with \code{makeCPORetrafoless}) and Feature Operation CPOs (created with \code{makeCPOExtendedTrafo}(!)), it must return the
#'   data in the same format as received it in its \code{data} argument (depending on \code{dataformat}). If \code{dataformat} is a
#'   \code{df.all} or \code{task}, this means the target column(s) contained in the \code{data.frame} or \code{Task} returned must not be modified.
#'
#'   For CPOs that are not Retrafoless, a unit of information to be carried over to the retrafo step needs to be created inside the \code{cpo.trafo}
#'   function. This unit of information is a variable that must be defined inside the environment of the \code{cpo.trafo} function and will be
#'   retrieved by the CPO framework.
#'
#'   If \code{cpo.retrafo} is not \code{NULL}
#'   the unit is an object named \dQuote{\code{control}} that will be passed on as the \code{control} argument to the
#'   \code{cpo.retrafo} function. If \code{cpo.retrafo} is \code{NULL}, the unit is a \emph{function}, called \dQuote{\code{cpo.retrafo}},
#'   that will be used
#'   \emph{instead} of the \code{cpo.retrafo}
#'   function passed over to \code{makeCPOExtendedTargetOp} / \code{makeCPOExtendedTrafo}. It must behave
#'   the same as the function it replaces, but has only the \code{data} (and \code{target}, for Target Operation CPOs) argument.
#'
#'   For Target Operation CPOs created with \code{makeCPOExtendedTargetOp}, another unit of information to be used by \code{cpo.invert}
#'   must be used. The options here are similar to \code{cpo.retrafo}: Either a control object, named \code{control.invert}, is created,
#'   or the \code{cpo.invert} function itself is given (and \code{cpo.invert} in the \code{makeCPOExtendedTargetOp} call is set to \code{NULL}),
#'   with the \code{target} and \code{predict.type} arguments.
#' @param cpo.retrafo [\code{function} | \code{NULL}]\cr
#'   This is a function which must have the parameters \code{data}, \code{target} (Target Operation CPOs only) and \code{control},
#'   as well as the parameters specified in \code{par.set}. (Alternatively,
#'   the function may have only some of these arguments and a \code{\link[methods:dotsMethods]{dotdotdot}} argument).
#'   In Feature Operation CPOs created with \code{makeCPO}, if \code{cpo.train} is \code{NULL}, the \code{control} argument must be absent.
#'
#'   This function gets called during the \dQuote{retransformation} step where prediction data is given to the \code{\link{CPORetrafo}} object before it
#'   is given to a fitted machine learning model for prediction. In \code{makeCPO} Featore Operation CPOs and \code{makeCPOTargetOp} Target Operation CPOs,
#'   this is \emph{also} called during the
#'   first trafo step, where the \code{\link{CPO}} object is applied to training data.
#'
#'   In Feature Operation CPOs, this function receives the data to be
#'   transformed and must return the transformed data in the same format as it received them.
#'   The format of \code{data} is the same as the format in \code{cpo.train} and \code{cpo.trafo}, with the exception that if \code{dataformat} is
#'   \dQuote{task} or \dQuote{df.all}, the behaviour here is as if \dQuote{df.split} had been given.
#'
#'   In Target Operation CPOs created with \code{makeCPOTargetOp}, this function receives the data and target to be transformed
#'   and must return the transformed target. The input format of these parameters depends on \code{dataformat}.
#'   If \code{dataformat} is \dQuote{task} or \dQuote{df.all}, the returned value must be the modified \code{\link[mlr]{Task}} / \code{data.frame}
#'   with the feature columns not modified. Otherwise, the target values to be modified are in the \code{target} parameter, and the return
#'   value must be a \code{data.frame} of the modified target values only.
#'
#'   In Target Operation CPOs created with \code{makeCPOExtendedTargetOp}, this function is called during the retrafo step, and it must
#'   create a \code{control.invert} object in its environment to be used in the inversion step, as well as return the modified target
#'   data.The format of the data given to \code{cpo.retrafo} in Target Operation CPOs created with \code{makeCPOExtendedTargetOp} is the same
#'   as in other functions, with the exception that, if \code{dataformat} is \dQuote{df.all} or \dQuote{task}, the full \code{data.frame}
#'   or \code{\link[mlr]{Task}} will be given as the \code{target} parameter, while the \code{data} parameter will behave as if
#'   \code{dataformat} \dQuote{df.split}. Depending on what object the \code{\link{CPORetrafo}} object was applied to,
#'   the \code{target} argument \emph{may be \code{NULL}}; in that case \code{NULL} must also be returned by the function.
#'
#'   If \code{cpo.invert} is \code{NULL}, \code{cpo.retrafo} should create a \code{cpo.invert} function in its environment instead of
#'   creating the control object; this function should then take the \code{target} and \code{predict.type} arguments. If \code{constant.invert}
#'   is \code{TRUE}, this function does not need to define the \code{control.invert} or \code{cpo.invert} variables, they are instead
#'   taken from \code{cpo.trafo}.
#' @param cpo.train.invert
#'   This is a function which must have the parameters \code{data}, and \code{control},
#'   as well as the parameters specified in \code{par.set}. (Alternatively,
#'   the function may have only some of these arguments and a \code{\link[methods:dotsMethods]{dotdotdot}} argument).
#'
#'   This function receives the feature columns given for prediction, and must return a
#'   control object that will be passed on to the \code{cpo.invert} function, \emph{or} it must return a \emph{function} that will be treated
#'   as the \code{cpo.invert} function if the \code{cpo.invert} argument is \code{NULL}. In the latter case, the returned function takes
#'   exactly two arguments (the prediction column to be inverted, and \code{predict.type}), and otherwise behaves identically to \code{cpo.invert}.
#'
#'   If \code{constant.invert} is \code{TRUE}, this must be \code{NULL}.
#'
#' @param cpo.invert [\code{function} | \code{NULL}]\cr
#'   This is a function which must have the parameters \code{target} (a \code{data.frame} containing the columns of a prediction made), \code{control.invert},
#'   and \code{predict.type}, as well as the parameters specified in \code{par.set}. (Alternatively,
#'   the function may have only some of these arguments and a \code{\link[methods:dotsMethods]{dotdotdot}} argument).
#'
#'   The \code{predict.type} \emph{requested} by the \code{\link[stats]{predict}} or \code{\link{invert}} call is given as a \code{character(1)} in
#'   the \code{predict.type} argument. Note that this is not necessarily the \code{predict.type} of the prediction made and given as \code{target} argument,
#'   depending on the value of \code{predict.type.map} (see there).
#'
#'   This function performs the inversion for a Target Operation CPO. It takes a control object, which summarizes information from the training and
#'   retrafo step, and the prediction as returned by a machine learning model, and undoes the operation done to the target column in the \code{cpo.trafo}
#'   function.
#'
#'   For example, if the trafo step consisted of taking the logarithm of a regression target, the \code{cpo.invert} function could return the exponentiated
#'   prediction values by taking the \code{exp} of the only column in the \code{target} \code{data.frame} and returning the result of that. This kind of
#'   operation does not need the \code{cpo.retrafo} step and should have \code{skip.retrafo} set to \code{TRUE}.
#'
#'   As a more elaborate example, a CPO could train a model on the training data and set the target values to the \emph{residues} of that trained model.
#'   The \code{cpo.retrafo} function would then make predictions with that model on the new prediction data and save the result to the \code{control} object.
#'   The \code{cpo.invert} function would then add these predictions to the predictions given to it in the \code{target} argument to \dQuote{invert} the
#'   antecedent subtraction of model predictions from target values when taking the residues.
#' @return [\code{\link{CPOConstructor}}]. A Constructor for \code{\link{CPO}}s.
#' @family CPOConstructor related
#' @family CPO lifecycle related
#' @family advanced topics
#'
#' @examples
#' # an example constant feature remover CPO
#' constFeatRem = makeCPO("constFeatRem",
#'  dataformat = "df.features",
#'  cpo.train = function(data, target) {
#'    names(Filter(function(x) {  # names of columns to keep
#'        length(unique(x)) > 1
#'      }, data))
#'    }, cpo.retrafo = function(data, control) {
#'    data[control]
#'  })
#' # alternatively:
#' constFeatRem = makeCPO("constFeatRem",
#'   dataformat = "df.features",
#'   cpo.train = function(data, target) {
#'     cols.keep = names(Filter(function(x) {
#'         length(unique(x)) > 1
#'       }, data))
#'     # the following function will do both the trafo and retrafo
#'     result = function(data) {
#'       data[cols.keep]
#'     }
#'     result
#'   }, cpo.retrafo = NULL)
#' @name makeCPO
NULL


###################
# The following is a rudiment, possibly some of this needs to be used.
# TODO: Delete if documentation is done and it turns out this is not needed.
###################
# @title Create a custom CPO constructor
#
# @description
# \code{makeCPOExtended} creates a Feature Operation CPO constructor, i.e. a constructor for a CPO that will
# operate on feature columns. \code{makeCPOTargetOp} creates a Target Operation CPO constructor, which
# creates CPOs that operate on the target column.
#
# \code{makeCPOExtended} is for advanced users and internal use; for a much simpler user-interface, use
# \code{\link{makeCPO}}.
#
# @inheritparams makeCPO
# @param ...
#   Parameters of the CPO, in the format of \code{\link[ParamHelpers]{pSS}}. These parameters are used in addition
#   to the \code{par.set} parameters.
# @param trafo.type [\code{character(1)}]\cr
#   Indicates what API is used for \code{cpo.trafo} and \code{cpo.retrafo}, and how state information is transferred
#   between them. Possibilities are:
#   \itemize{
#     \item{trafo.returns.data} \code{cpo.trafo} must be specified and is called with the training data and the CPO parameters.
#       It must return the modified data, and within its namespace must either specify a \dQuote{control} variable ("Object-Based CPO"),
#       if \code{cpo.retrafo} is given, or a \dQuote{cpo.retrafo} variable, if (the makeCPOExtended parameter) \code{cpo.retrafo}
#       is \code{NULL} ("Functional CPO"). For Object-Based CPO, \code{cpo.retrafo} is called with the \code{control} object
#       created in \code{cpo.trafo}, additionally with the new data, and the CPO parameters. For Functional CPO, \code{cpo.retrafo} is
#       constructed inside the \code{cpo.trafo} call and is used for transformation of new data. It must take a single argument and
#       return the transformed data.
#     \item{trafo.returns.control} \code{cpo.trafo} must be specified and is called with the training data and the CPO parameters. It must return
#       a \code{cpo.retrafo} function that takes the data to be transformed as a single argument, and returns the transformed data.
#       If \code{trafo.type} is \dQuote{trafo.returns.control}, \code{pco.retrafo} must be \code{NULL}.
#     \item{stateless} Specification of \code{cpo.trafo} is optional and may be \code{NULL}. If it is not given, \code{cpo.retrafo} is used on both
#       training and new data; otherwise, \code{cpo.trafo} is applied to training data, \code{cpo.retrafo} is used on predict data. There
#       is no transfer of information from trafo to retrafo. If \code{cpo.trafo} is not given, \code{dataformat} must not be \dQuote{task} or \dQuote{df.all}.
#   }
# @param .type [\code{character(1)}]\cr
#   For Target Operation CPOs, the type of task that it operates on. Must be one of \dQuote{cluster}, \dQuote{classif}, \dQuote{multilabel}, \dQuote{regr},
#   or \dQuote{surv}. If input data is a data.frame, it will be treated as a cluster task. Default is \dQuote{cluster}.
# @param .type.out [\code{character(1)}]\cr
#   For Target Operation CPOs, the type of task that will be generated by this CPO. If this is the same as \code{.type}, no conversion takes place.
#   Possible values are the same as for \code{.type}. Default is \code{.type}.
# @param predict.type [\code{character} | \code{list}]\cr
#   Must be a named \code{character}, or named \code{list} of \code{character(1)}, indicating
#   what \code{predict.type} (see \link{Prediction}) a prediction must have if the output prediction
#   is to be of some type. E.g. if a CPO converts a \dQuote{regr} \code{Task} into a
#   \dQuote{classif} \code{Task}, and if for \dQuote{se} prediction it needs a classification
#   learner to give \dQuote{prob} type predictions, while for \dQuote{response} prediction it
#   also needs \dQuote{response} predictions, this would be \code{c(response = "response",
#   se = "prob")}. The names are the prediction types that are requested from this CPO, the
#   values are types that this CPO will request from an underlying learner. If a name is not
#   present, the \code{predict.type} is assumed not supported. Default is \code{c(response = "response")}.
# @param data.dependent [\code{logical(1)}]\cr
#   Whether to make a data-dependent inverter CPO. If this is \code{FALSE}, the \code{cpo.trafo} function does not have
#   a \code{data} parameter.
# @param cpo.trafo [\code{language} | \code{function} | \code{NULL}]\cr
#   This can either be a function, or just the function body wrapped in curly braces.
#   If this is a function, it must have the parameters \dQuote{data} and \dQuote{target},
#   as well as the parameters specified in \dQuote{...} or \dQuote{par.set}. (Alternatively,
#   the function may have a dotdotdot argument). Depending on the values of \code{trafo.type} and
#   \code{dataformat} -- see there --, it must return a \dQuote{data.frame}, a \dQuote{task},
#   a dQuote{matrix}, \dQuote{list} of \dQuote{data.frame} and \dQuote{matrix} objects, or a retrafo function.
#
#   If \dQuote{cpo.retrafo} is given and \code{trafo.type} is \dQuote{trafo.returns.data}, it must create a \dQuote{control}
#   variable in its namespace, which will be passed on to \dQuote{cpo.retrafo}. If \dQuote{cpo.retrafo} is
#   not given and \code{trafo.type} is \dQuote{trafo.returns.data}, it must create a \dQuote{cpo.retrafo} function within its namespace, which will be called
#   for re-transformation.
#
#   If \code{trafo.type} is \dQuote{trafo.returns.control}, this function must return a \dQuote{cpo.retrafo} function.
#
#   If \code{trafo.type} is
#   \dQuote{stateless}, this argument may be \code{NULL}, or a function which just returns the transformed data.
#
#   If \dQuote{cpo.trafo} is a list of expressions (preferred), it is turned into a function by mlr, with the correct function arguments.
# @param cpo.retrafo [\code{language} | \code{function}]\cr
#   Similarly to \dQuote{cpo.trafo}, this is either a function, the function body in curly braces (preferred), or \code{NULL}.
#   If this is not \code{NULL}, this function must have the same arguments as \code{cpo.trafo}, with the exception that
#   the \dQuote{target} argument is replaced by a \dQuote{control} argument, which will be
#   the value created in the \dQuote{cpo.trafo} run. It gets its input data in the same format as
#   \dQuote{cpo.trafo}, with the exception that if \dQuote{dataformat} is \dQuote{task}, it gets a
#   \dQuote{data.frame} as if \dQuote{dataformat} were \dQuote{df.all}. This function must similarly return an
#   object in the same format as it received as input.
#
# @family CPO
# @export
#
# @examples
# # an example 'pca' CPO
# # demonstrates the (object based) "trafo.returns.data" CPO API
# pca = makeCPOExtended("pca",  # name
#   center = TRUE: logical,  # one logical parameter 'center'
#   dataformat= "numeric",  # only handle numeric columns
#   trafo.type = "trafo.returns.data",  # default, can be omitted
#   # cpo.trafo is given as a function body. The function head is added
#   # automatically, containing 'data', 'target', and 'center'
#   # (since a 'center' parameter was defined)
#   cpo.trafo = {
#     pcr = prcomp(as.matrix(data), center = center)
#     # The following line creates a 'control' object, which will be given
#     # to retrafo.
#     control = list(rotation = pcr$rotation, center = pcr$center)
#     pcr$x  # returning a matrix is ok
#   # Just like cpo.trafo, cpo.retrafo is a function body, with implicit
#   # arguments 'data', 'control', and 'center'.
#   }, cpo.retrafo = {
#     scale(as.matrix(data), center = control$center, scale = FALSE) %*%
#       control$rotation
#   })
#
# # an example 'scale' CPO
# # demonstrates the (functional) "trafo.returns.data" CPO API
# scaleCPO = makeCPOExtended("scale",
#   dataformat = "numeric",
#   # trafo.type = "trafo.returns.data" is implicit
#   cpo.trafo = function(data, target) {
#     result = scale(as.matrix(data), center = center, scale = scale)
#     cpo.retrafo = function(data) {
#       # here we can use the 'result' object generated in cpo.trafo
#       scale(as.matrix(data), attr(result, "scaled:center"),
#         attr(result, "scaled:scale"))
#     }
#     result
#   }, cpo.retrafo = NULL)  # don't forget to set it cpo.retrafo to NULL
#
# # an example constant feature remover CPO
# # demonstrates the "trafo.returns.control" CPO API
# constFeatRem = makeCPOExtended("constFeatRem",
#   dataformat = "df.features",
#   trafo.type = "trafo.returns.control",
#   cpo.trafo = function(data, target) {
#     cols.keep = names(Filter(function(x) {
#         length(unique(x)) > 1
#       }, data))
#     # the following function will do both the trafo and retrafo
#     result = function(data) {
#       data[cols.keep]
#     }
#     result
#   }, cpo.retrafo = NULL)
#
# # an example 'square' CPO
# # demonstrates the "stateless" CPO API
# square = makeCPOExtended("scale",
#   dataformat = "numeric",
#   trafo.type = "stateless",
#   cpo.trafo = function(data) {
#     as.matrix(data) * 2
#   }, cpo.retrafo = NULL) # optional, we don't need it since trafo & retrafo same
#
