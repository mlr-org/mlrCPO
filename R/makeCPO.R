# makeCPO.R contains all CPO constructor functions, that are used both
# internally and are exported for user use.

#' @include listCPO.R parameters.R properties.R

cpo.dataproperties = c("numerics", "factors", "ordered", "missings")
cpo.tasktypes = c("cluster", "classif", "multilabel", "regr", "surv")  # these are the SUPPORTED tasks
cpo.targetproperties = c("oneclass", "twoclass", "multiclass")
cpo.predict.properties = c("prob", "se")

#' @title Create a custom CPO constructor
#'
#' @description
#' \code{makeCPO} creates a \emph{Feature Operation} \code{\link{CPOConstructor}}, i.e. a constructor for a \code{\link{CPO}} that will
#' operate on feature columns. \code{makeCPOTargetOp} creates a \emph{Target Operation} \code{\link{CPOConstructor}}, which
#' creates \code{\link{CPO}}s that operate on the target column. \code{makeCPORetrafoless} creates a \emph{Retrafoless} \code{\link{CPOConstructor}},
#' which creates \code{\link{CPO}}s that may operate on both feature and target columns, but have no retrafo operation. See \link{OperatingType} for further
#' details on the distinction of these.
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
#'     operation: A \code{\link{CPO}} may be one of three \link{OperatingType} (see there). The \emph{Feature Operation} \code{\link{CPO}} does not
#'     transform target columns and hence only needs to be involved in steps 1 and 2. The \emph{Target Operation} \code{\link{CPO}} only transforms
#'     target columns, and therefore only concerns itself with steps 1 and 3. A \emph{Retrafoless} \code{\link{CPO}} may change both feature and
#'     target columns, but may not perform a retrafo \emph{or} inverter operation (and is therefore only concerned with step 1). Note that this
#'     is effectively a restriction on what kind of transformation a Retrafoless CPO may perform: it must not be a transformation of the data
#'     or target \emph{space}, it may only act or subtract points within this space.
#'
#'     The Operating Type of a \code{\link{CPO}} is ultimately dependent on the function that was used to create the \code{\link{CPOConstructor}}:
#'     \code{makeCPO}, \code{makeCPOTargetOp}, or \code{makeCPORetrafoless}.}
#'   \item{\strong{Data Transformation}}{
#'     At the core of a CPO is the modification of data it performs. The transformation of each row, during training \emph{and} prediction, should
#'     happen in the same way, and it may only depend on the entirety of the \emph{training} data--i.e. the value of a data row in a prediction
#'     data set may not influence the transformation of a different prediction data row. This property is ensured by splitting the transformation
#'     into two functions: One function that collects all relevant information from the training data (called \code{train}), and one that transforms
#'     given data, using this collected information and (\emph{potentially new, unseen}) data to be transformed (called \code{retrafo}). The \code{retrafo}
#'     function should handle all data as if it were prediction data and unrelated to the data given to \code{train}.
#'
#'     Internally, when a \code{\link{CPO}} gets applied to a data set using \code{\link{applyCPO}}, the \code{train} function is called, and the
#'     resulting control object is used for a subsequent \code{retrafo} call which transforms the data. Before the result is given back from the
#'     \code{\link{applyCPO}} call, the control object is used to create a \code{\link{CPORetrafo}} object and a \code{\link{CPOInverter}} object,
#'     which are attached to the result as attributes.
#'
#'     When a \code{\link{CPORetrafo}} is then applied to new prediction data, the control object previously returned by \code{train} is given,
#'     combined with this \emph{new} data, to another \code{retrafo} call that performs the new transformation.
#'
#'     Note that the \code{*Extended} functions optionally offer different semantics for the given functions.}
#'   \item{\strong{\code{train}-\code{retrafo} information transfer}}{
#'     One possibility to transfer information from \code{train} to \code{retrafo} is to have \code{train} return a control object (a \code{\link[base]{list}})
#'     that is then given to \code{retrafo}. The CPO is then called an \emph{object based} CPO.
#'
#'     Another possibility is to not give a \code{retrafo}
#'     function (set it to \code{NULL} in the \code{makeCPO} call) and have \code{train} instead return a \emph{function} instead. This function is then
#'     used as the \code{retrafo} function, and should have access to all relevant information about the training data as a closure. This is called
#'     \emph{functional} CPO. To save memory, the actual data given to \code{train} is removed from the environment of its return value in this case
#'     (i.e. the environment of the retrafo function). This means the retrafo function may not reference a \dQuote{\code{data}} variable.}
#'   \item{\strong{Hyperparameters}}{
#'     The action performed by a CPO may be influenced using \emph{hyperparameters}, during its construction as well as afterwards (then using
#'     \code{\link[mlr]{setHyperPars}}). Hyperparameters must be specified as a \code{\link[ParamHelpers:makeParamSet]{ParamSet}} and given as argument \code{par.set}.
#'     Default values for each parameter may be specified in this \code{\link[ParamHelpers:makeParamSet]{ParamSet}} or optionally as another argument \code{par.vals}.
#'
#'     Hyperparameters given are made part of the \code{\link{CPOConstructor}} function and can thus be given during construction.
#'     Parameter default values function as the default values for the \code{\link{CPOConstructor}} function parameters (which are thus made optional function
#'     parameters of the \code{\link{CPOConstructor}} function). The CPO framework handles storage and changing of hyperparameter values.
#'     When the \code{train} and \code{retrafo} functions are called to transform data, the hyperparameter values are given to them as arguments, so
#'     \code{train} and \code{retrafo} functions must be able to accept these parameters, either directly, or with a \code{...} argument.
#'
#'     Note that with \emph{functional} \code{\link{CPO}}s, the \code{retrafo} function does not take hyperparameter arguments (and instead can usually
#'     refer to them by its environment).
#'
#'     Hyperparameters may be \emph{exported} (or not), thus making them available for \code{\link[mlr]{setHyperPars}}. Not exporting a parameter
#'     has advantage that it does not clutter the \code{\link[ParamHelpers:makeParamSet]{ParamSet}} of a big \code{\link{CPO}} or \code{\link{CPOLearner}} pipeline with
#'     many hyperparameters. Which hyperparameters are exported is chosen during the constructing call of a \code{\link{CPOConstructor}}, but the default
#'     exported hyperparameters can be chosen with the \code{export.params} parameter.}
#'   \item{\strong{Properties}}{
#'     Similarly to \code{\link[mlr:makeLearner]{Learner}}s, \code{\link{CPO}}s may specify what kind of data they are and are not able to handle. This is done by
#'     specifying \code{properties.*} arguments. The names of possible properties are the same as possible \code{\link[mlr]{LearnerProperties}}, but since
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
#'     one vector, they are specified separately in \code{makeCPO} etc. for convenience reasons. See \code{\link{CPOProperties}} for details.}
#'   \item{\strong{Data Format}}{
#'     EXPLANATION # TODO}
#'   \item{\strong{ID}}{
#'     EXPLANATION # TODO}
#'   \item{\strong{Packages}}{
#'     EXPLANATION # TODO}
#' }
#'
#' \code{makeCPO}, \code{makeCPOTargetOp} and \code{\link{makeCPORetrafoless}} have a comparatively easy user-interface; for more advanced use-cases and
#' interesting shortcuts, use \code{\link{makeCPOExtended}}, \code{\link{makeCPOTargetOpExtended}}, \code{\link{makeCPORetrafolessExtended}}.
#'
#' @param cpo.name [\code{character(1)}]\cr
#'   The name of the resulting CPO constructor / CPO. This is used for identification in output,
#'   and as the default \code{id}.
#' @param par.set [\code{ParamSet} | \code{NULL}]\cr
#'   Optional parameter set, for configuration of CPOs during construction or by hyperparameters.
#'   Default is \code{NULL}.
#' @param par.vals [\code{list}]\cr
#'   Named list of default parameter values for the CPO. These are used additionally to the
#'   parameter default values in \code{par.set}. It is preferred to use
#'   these default values, and not \code{par.vals}. Default is \code{list()}.
#' @param dataformat [\code{character(1)}]\cr
#'   Indicate what format the data should be as seen by \dQuote{cpo.trafo} and the retrafo function. Possibilities are:
#'   \tabular{lll}{
#'     \bold{dataformat} \tab \bold{data}               \tab \bold{target}         \cr
#'     df.all      \tab data.frame with target cols     \tab target colnames       \cr
#'     df.features \tab data.frame without target       \tab data.frame of target  \cr
#'     task        \tab full task                       \tab target colnames       \cr
#'     split       \tab list of data.frames by type     \tab data.frame of target  \cr
#'     [type]      \tab data.frame of [type] feats only \tab data.frame of target  \cr
#'   }
#'   [type] can be any one of \dQuote{factor}, \dQuote{numeric}, \dQuote{ordered}.\cr
#'   For \code{dataformat} \code{==} \dQuote{split}, the list has entries \dQuote{factor}, \dQuote{numeric},
#'   \dQuote{other}, and possibly \dQuote{ordered}--the last one only present if \code{dataformat.factor.with.ordered}
#'   is \code{FALSE}.
#'
#'   If the CPO is a Feature Operation CPO, then the return value of the retrafo function must be in the same format as the one requested.
#'   E.g. if \code{dataformat} is \dQuote{split}, the return value must be a named list with entries \dQuote{numeric},
#'   \dQuote{factor}, and \dQuote{other}. The types of the returned data may be arbitrary: In the given example,
#'   the \dQuote{factor} slot of the returned list may contain numeric data. (Note however that if data is returned
#'   that has a type not already present in the data, \dQuote{properties.needed} must specify this.)
#'
#'   If \code{dataformat} is either \dQuote{df.all} or \dQuote{task}, the
#'   target column(s) in the returned value of the retrafo function must be identical with the target column(s) given as input.
#'
#'   If \dQuote{dataformat} is \dQuote{split}, the \dQuote{$numeric} slot of the value returned by the retrafo function
#'   object may also be a \code{matrix}. If \dQuote{dataformat} is \dQuote{numeric}, the returned object may also be a
#'   matrix.
#' @param dataformat.factor.with.ordered [\code{logical(1)}]\cr
#'   Whether to treat \code{ordered} typed features as \code{factor} typed features. This affects how \code{dataformat} is handled, and only
#'   has an effect if \code{dataformat} is \dQuote{split} or \dQuote{factor}.
#' @param export.params [\code{logical(1)} | \code{character}]\cr
#'   Indicates which CPO parameters are exported by default. Exported parameters can be changed after construction using \code{\link{setHyperPars}},
#'   but exporting too many parameters may lead to messy parameter sets if many CPOs are combined. This can be overridden on construction.
#'   If this is a \code{logical(1)}, \code{TRUE} exports all parameters, \code{FALSE} to exports no parameters. It may also be a \code{character},
#'   indicating the names of parameters to be exported. Default is \code{TRUE}.
#' @param fix.factors [\code{logical(1)}]\cr
#'   Whether to constrain factor levels of new data to the levels of training data, for each factorial or ordered column. If new data contains
#'   factors that were not present in training data, the values are set to \code{NA}. Default is \code{FALSE}.
#' @param properties [\code{character}]\cr
#'   The kind if data that the CPO will be able to handle. This can be one or many of: \dQuote{numerics},
#'   \dQuote{factors}, \dQuote{ordered}, \dQuote{missings}.
#'   There should be a bias towards including properties. If a property is absent, the preproc
#'   operator will reject the data. If an operation e.g. only works on numeric columns that have no
#'   missings (like PCA), it is recommended to give all properties, ignore the columns that
#'   are not numeric (using \dQuote{dataformat} = \dQuote{split}), and giving an error when
#'   there are missings in the numeric columns (since missings in factorial features are not a problem).
#'   Defaults to the maximal set.
#' @param properties.adding [\code{character}]\cr
#'   Can be one or many of the same values as \dQuote{properties} for Feature Operation CPOs, and one or many of the same values as \dQuote{properties.target}
#'   for Target Operation CPOs. These properties get added to a Learner (or CPO) coming after / behind this CPO. When a CPO imputes missing values, for example,
#'   this should be \dQuote{missings}. This must be a subset of \dQuote{properties} or \dQuote{properties.target}. Default is
#'   \code{character(0)}.
#' @param properties.needed [\code{character}]\cr
#'   Can be one or many of the same values as \dQuote{properties} for Feature Operation CPOs,
#'   and one or many of the same values as \dQuote{properties.target}. These properties are required
#'   from a Learner (or CPO) coming after / behind this CPO. E.g., when a CPO converts factors to
#'   numerics, this should be \dQuote{numerics} (and \dQuote{properties.adding} should be \dQuote{factors}).
#'   Default is \code{character(0)}.
#' @param properties.target [\code{character}]\cr
#'   For Feature Operation CPOs, this can be one or many of \dQuote{cluster}, \dQuote{classif}, \dQuote{multilabel}, \dQuote{regr}, \dQuote{surv},
#'   \dQuote{oneclass}, \dQuote{twoclass}, \dQuote{multiclass}. Just as \code{properties}, it
#'   indicates what kind of data a CPO can work with. Data given as data.frame needs the \dQuote{cluster} property. Default is the maximal set.
#'
#'   For Target Operation CPOs, this should only be given if the CPO operates on classification tasks. It must then be a subset of \dQuote{oneclass},
#'   \dQuote{twoclass}, or \dQuote{multiclass}. Otherwise, it should be \code{character(0)}. Default is \code{character(0)}.
#' @param packages [\code{character}]\cr
#'   Package(s) that should be loaded when the CPO is constructed. This gives the user an early error if
#'   a package required for the CPO is not available on his system, or can not be loaded. Default is \code{character(0)}.
#' @param cpo.trafo [\code{function}]\cr
#'   This is a function which must have the parameters \dQuote{data} and \dQuote{target},
#'   as well as the parameters specified in \dQuote{par.set}. (Alternatively,
#'   the function may have a dotdotdot argument). This is a constructor function which must return a \dQuote{retrafo} function which
#'   modifies data. This retrafo function must have exactly one argument--the (new) data--and return the modified data. The format
#'   of the argument, and of the return value of the retrafo function, depends on the value of the \code{dataformat} parameter.
#' @param cpo.retrafo [\code{language} | \code{function}]\cr
#'   Similarly to \dQuote{cpo.trafo}, this is either a function, the function body in curly braces (preferred), or \code{NULL}.
#'   If this is not \code{NULL}, this function must have the same arguments as \code{cpo.trafo}, with the exception that
#'   the \dQuote{target} argument is replaced by a \dQuote{control} argument, which will be
#'   the value created in the \dQuote{cpo.trafo} run. It gets its input data in the same format as
#'   \dQuote{cpo.trafo}, with the exception that if \dQuote{.dataformat} is \dQuote{task}, it gets a
#'   \dQuote{data.frame} as if \dQuote{.dataformat} were \dQuote{df.all}. This function must similarly return an
#'   object in the same format as it received as input.
#' @family CPO
#' @export
#'
#' @examples
#' # an example constant feature remover CPO
#' constFeatRem = makeCPO("constFeatRem",
#'  dataformat = "df.features",
#'  cpo.trafo = function(data, target) {
#'    names(Filter(function(x) {  # names of columns to keep
#'        length(unique(x)) > 1
#'      }, data))
#'    }, cpo.retrafo = function(data, control) {
#'    data[control]
#'  })
#' # alternatively:
#' constFeatRem = makeCPO("constFeatRem",
#'   dataformat = "df.features",
#'   cpo.trafo = function(data, target) {
#'     cols.keep = names(Filter(function(x) {
#'         length(unique(x)) > 1
#'       }, data))
#'     # the following function will do both the trafo and retrafo
#'     result = function(data) {
#'       data[cols.keep]
#'     }
#'     result
#'   }, cpo.retrafo = NULL)
#
# Developer Notes: Like all CPO defining functions, this one just calls makeCPOGeneral, with certain parameters already set.
makeCPO = function(cpo.name, par.set = NULL, par.vals = list(), dataformat = c("df.features", "split", "df.all", "task", "factor", "ordered", "numeric"),
                   dataformat.factor.with.ordered = TRUE, export.params = TRUE,  # FALSE, TRUE, names of parameters to export
                   fix.factors = FALSE, properties = c("numerics", "factors", "ordered", "missings"),
                   properties.adding = character(0), properties.needed = character(0),
                   properties.target = c("cluster", "classif", "multilabel", "regr", "surv",
                     "oneclass", "twoclass", "multiclass"),
                   packages = character(0), cpo.trafo, cpo.retrafo) {
  dataformat = match.arg(dataformat)

  assertSubset(properties, cpo.dataproperties)
  assertSubset(properties.target, c(cpo.tasktypes, cpo.targetproperties))
  assertSubset(properties.needed, cpo.dataproperties)

  makeCPOGeneral(.cpotype = "feature",
    .cpo.name = cpo.name, .par.set = par.set, .par.vals = par.vals,
    .dataformat = dataformat, .dataformat.factor.with.ordered = dataformat.factor.with.ordered,
    .fix.factors = fix.factors, .data.dependent = TRUE,
    .trafo.type = "trafo.returns.control", .export.params = export.params, .properties = properties,
    .properties.adding = properties.adding, .properties.needed = properties.needed,
    .properties.target = properties.target, .type.from = NULL, .type.to = NULL,
    .predict.type = NULL, .packages = packages,
    cpo.trafo = substitute(cpo.trafo), cpo.retrafo = substitute(cpo.retrafo))
}


makeCPORetrafoless = function(cpo.name, par.set = NULL, par.vals = list(), dataformat = c("df.all", "task"),
                   dataformat.factor.with.ordered = TRUE, export.params = TRUE,  # FALSE, TRUE, names of parameters to export
                   fix.factors = FALSE, properties = c("numerics", "factors", "ordered", "missings"),
                   properties.adding = character(0), properties.needed = character(0),
                   properties.target = c("cluster", "classif", "multilabel", "regr", "surv",
                     "oneclass", "twoclass", "multiclass"),
                   packages = character(0), cpo.trafo) {
  dataformat = match.arg(dataformat)

  assertSubset(properties, cpo.dataproperties)
  assertSubset(properties.target, c(cpo.tasktypes, cpo.targetproperties))
  assertSubset(properties.needed, cpo.dataproperties)

  makeCPOGeneral(.cpotype = "feature",
    .cpo.name = cpo.name, .par.set = par.set, .par.vals = par.vals,
    .dataformat = dataformat, .dataformat.factor.with.ordered = dataformat.factor.with.ordered,
    .fix.factors = fix.factors, .data.dependent = TRUE,
    .trafo.type = "retrafoless", .export.params = export.params, .properties = properties,
    .properties.adding = properties.adding, .properties.needed = properties.needed,
    .properties.target = properties.target, .type.from = NULL, .type.to = NULL,
    .predict.type = NULL, .packages = packages,
    cpo.trafo = substitute(cpo.trafo), cpo.retrafo = NULL)
}

makeCPOTargetOp = function(cpo.name, par.set = NULL, par.vals = list(), dataformat = c("df.features", "split", "df.all", "task", "factor", "ordered", "numeric"),
                   dataformat.factor.with.ordered = TRUE, data.dependent = TRUE, export.params = TRUE,  # FALSE, TRUE, names of parameters to export
                   properties.data = c("numerics", "factors", "ordered", "missings"),
                   properties.adding = character(0), properties.needed = character(0),
                   properties.target = character(0),
                   type = c("cluster", "classif", "multilabel", "regr", "surv"),
                   type.out = type,
                   predict.type = c(response = "response"), packages = character(0), cpo.trafo, cpo.inverter) {
  # TODO
}


#' @title Create a custom CPO constructor
#'
#' @description
#' \code{makeCPOExtended} creates a Feature Operation CPO constructor, i.e. a constructor for a CPO that will
#' operate on feature columns. \code{makeCPOTargetOp} creates a Target Operation CPO constructor, which
#' creates CPOs that operate on the target column.
#'
#' \code{makeCPOExtended} is for advanced users and internal use; for a much simpler user-interface, use
#' \code{\link{makeCPO}}.
#'
#' @param .cpo.name [\code{character(1)}]\cr
#'   The name of the resulting CPO constructor / CPO. This is used for identification in output,
#'   and as the default \code{id}.
#' @param ...
#'   Parameters of the CPO, in the format of \code{\link[ParamHelpers]{pSS}}.
#' @param .par.set [\code{ParamSet}]\cr
#'   Optional parameter set. If this is not \code{NULL}, the \dQuote{...} parameters are ignored.
#'   Default is \code{NULL}.
#' @param .par.vals [\code{list}]\cr
#'   Named list of default parameter values for the CPO. These are used additionally to the
#'   parameter default values in \dQuote{...} and \code{.par.set}. It is preferred to use
#'   these default values, and not \code{.par.vals}. Default is \code{list()}.
#' @param .dataformat [\code{character(1)}]\cr
#'   Indicate what format the data should be as seen by \dQuote{cpo.trafo} and \dQuote{cpo.retrafo}. Possibilities are:
#'   \tabular{lll}{
#'     \bold{dataformat} \tab \bold{data}               \tab \bold{target}         \cr
#'     df.all      \tab data.frame with target cols     \tab target colnames       \cr
#'     df.features \tab data.frame without target       \tab data.frame of target  \cr
#'     task        \tab full task                       \tab target colnames       \cr
#'     split       \tab list of data.frames by type     \tab data.frame of target  \cr
#'     [type]      \tab data.frame of [type] feats only \tab data.frame of target  \cr
#'   }
#'   [type] can be any one of \dQuote{factor}, \dQuote{numeric}, \dQuote{ordered}.\cr
#'   For \code{.dataformat} \code{==} \dQuote{split}, the list has entries \dQuote{factor}, \dQuote{numeric},
#'   \dQuote{other}, and possibly \dQuote{ordered}--the last one only present if \code{.dataformat.factor.with.ordered}
#'   is \code{FALSE}.
#'
#'   If the CPO is a Target Operation CPO, the return value of both \dQuote{cpo.trafo} and \dQuote{cpo.retrafo}
#'   must be either a task if \code{.dataformat} is \dQuote{task}, the complete (modified) data.frame
#'   if \code{.dataformat} is \dQuote{df.all}, and a data.frame containing only the target column(s) otherwise.
#'   Default is \dQuote{df.features}.
#'
#'   If the CPO is a Feature Operation CPO, then the return value must be in the same format as the one requested.
#'   E.g. if \code{.dataformat} is \dQuote{split}, the return value must be a named list with entries \dQuote{numeric},
#'   \dQuote{factor}, and \dQuote{other}. The types of the returned data may be arbitrary: In the given example,
#'   the \dQuote{factor} slot of the returned list may contain numeric data. (Note however that if data is returned
#'   that has a type not already present in the data, \dQuote{.properties.needed} must specify this.)
#'
#'   If \code{.dataformat} is either \dQuote{df.all} or \dQuote{task}, the
#'   target column(s) in the returned value must be identical with the target column(s) given as input.
#'
#'   If \dQuote{.dataformat} is \dQuote{split}, the \dQuote{$numeric} slot of the returned
#'   object may also be a \code{matrix}. If \dQuote{.dataformat} is \dQuote{numeric}, the returned object may also be a
#'   matrix.
#' @param .dataformat.factor.with.ordered [\code{logical(1)}]\cr
#'   Whether to treat \code{ordered} typed features as \code{factor} typed features. This affects how \code{.dataformat} is handled, and only
#'   has an effect if \code{dataformat} is \dQuote{split} or \dQuote{factor}.
#' @param .trafo.type [\code{character(1)}]\cr
#'   Indicates what API is used for \code{cpo.trafo} and \code{cpo.retrafo}, and how state information is transferred
#'   between them. Possibilities are:
#'   \itemize{
#'     \item{trafo.returns.data} \code{cpo.trafo} must be specified and is called with the training data and the CPO parameters.
#'       It must return the modified data, and within its namespace must either specify a \dQuote{control} variable ("Object-Based CPO"),
#'       if \code{cpo.retrafo} is given, or a \dQuote{cpo.retrafo} variable, if (the makeCPOExtended parameter) \code{cpo.retrafo}
#'       is \code{NULL} ("Functional CPO"). For Object-Based CPO, \code{cpo.retrafo} is called with the \code{control} object
#'       created in \code{cpo.trafo}, additionally with the new data, and the CPO parameters. For Functional CPO, \code{cpo.retrafo} is
#'       constructed inside the \code{cpo.trafo} call and is used for transformation of new data. It must take a single argument and
#'       return the transformed data.
#'     \item{trafo.returns.control} \code{cpo.trafo} must be specified and is called with the training data and the CPO parameters. It must return
#'       a \code{cpo.retrafo} function that takes the data to be transformed as a single argument, and returns the transformed data.
#'       If \code{.trafo.type} is \dQuote{trafo.returns.control}, \code{pco.retrafo} must be \code{NULL}.
#'     \item{stateless} Specification of \code{cpo.trafo} is optional and may be \code{NULL}. If it is not given, \code{cpo.retrafo} is used on both
#'       training and new data; otherwise, \code{cpo.trafo} is applied to training data, \code{cpo.retrafo} is used on predict data. There
#'       is no transfer of information from trafo to retrafo. If \code{cpo.trafo} is not given, \code{.dataformat} must not be \dQuote{task} or \dQuote{df.all}.
#'   }
#' @param .export.params [\code{logical(1)} | \code{character}]\cr
#'   Indicates which CPO parameters are exported by default. Exported parameters can be changed after construction using \code{\link{setHyperPars}},
#'   but exporting too many parameters may lead to messy parameter sets if many CPOs are combined. This can be overridden on construction.
#'   If this is a \code{logical(1)}, \code{TRUE} exports all parameters, \code{FALSE} to exports no parameters. It may also be a \code{character},
#'   indicating the names of parameters to be exported. Default is \code{TRUE}.
#' @param .fix.factors [\code{logical(1)}]\cr
#'   Whether to constrain factor levels of new data to the levels of training data, for each factorial or ordered column. If new data contains
#'   factors that were not present in training data, the values are set to \code{NA}. Default is \code{FALSE}.
#' @param .properties [\code{character}]\cr
#'   The kind if data that the CPO will be able to handle. This can be one or many of: \dQuote{numerics},
#'   \dQuote{factors}, \dQuote{ordered}, \dQuote{missings}.
#'   There should be a bias towards including properties. If a property is absent, the preproc
#'   operator will reject the data. If an operation e.g. only works on numeric columns that have no
#'   missings (like PCA), it is recommended to give all properties, ignore the columns that
#'   are not numeric (using \dQuote{.dataformat} = \dQuote{split}), and giving an error when
#'   there are missings in the numeric columns (since missings in factorial features are not a problem).
#'   Defaults to the maximal set.
#' @param .properties.adding [\code{character}]\cr
#'   Can be one or many of the same values as \dQuote{.properties} for Feature Operation CPOs, and one or many of the same values as \dQuote{.properties.target}
#'   for Target Operation CPOs. These properties get added to a Learner (or CPO) coming after / behind this CPO. When a CPO imputes missing values, for example,
#'   this should be \dQuote{missings}. This must be a subset of \dQuote{.properties} or \dQuote{.properties.target}. Default is
#'   \code{character(0)}.
#' @param .properties.needed [\code{character}]\cr
#'   Can be one or many of the same values as \dQuote{.properties} for Feature Operation CPOs,
#'   and one or many of the same values as \dQuote{.properties.target}. These properties are required
#'   from a Learner (or CPO) coming after / behind this CPO. E.g., when a CPO converts factors to
#'   numerics, this should be \dQuote{numerics} (and \dQuote{.properties.adding} should be \dQuote{factors}).
#'   Default is \code{character(0)}.
#' @param .properties.target [\code{character}]\cr
#'   For Feature Operation CPOs, this can be one or many of \dQuote{cluster}, \dQuote{classif}, \dQuote{multilabel}, \dQuote{regr}, \dQuote{surv},
#'   \dQuote{oneclass}, \dQuote{twoclass}, \dQuote{multiclass}. Just as \code{.properties}, it
#'   indicates what kind of data a CPO can work with. Data given as data.frame needs the \dQuote{cluster} property. Default is the maximal set.
#'
#'   For Target Operation CPOs, this should only be given if the CPO operates on classification tasks. It must then be a subset of \dQuote{oneclass},
#'   \dQuote{twoclass}, or \dQuote{multiclass}. Otherwise, it should be \code{character(0)}. Default is \code{character(0)}.
#' @param .properties.data [\code{character}]\cr
#'   For Target operating CPOs: Properties of feature columns that can be handled.
#' @param .type [\code{character(1)}]\cr
#'   For Target Operation CPOs, the type of task that it operates on. Must be one of \dQuote{cluster}, \dQuote{classif}, \dQuote{multilabel}, \dQuote{regr},
#'   or \dQuote{surv}. If input data is a data.frame, it will be treated as a cluster task. Default is \dQuote{cluster}.
#' @param .type.out [\code{character(1)}]\cr
#'   For Target Operation CPOs, the type of task that will be generated by this CPO. If this is the same as \code{.type}, no conversion takes place.
#'   Possible values are the same as for \code{.type}. Default is \code{.type}.
#' @param .predict.type [\code{character} | \code{list}]\cr
#'   Must be a named \code{character}, or named \code{list} of \code{character(1)}, indicating
#'   what \code{predict.type} (see \link{Prediction}) a prediction must have if the output prediction
#'   is to be of some type. E.g. if a CPO converts a \dQuote{regr} \code{Task} into a
#'   \dQuote{classif} \code{Task}, and if for \dQuote{se} prediction it needs a classification
#'   learner to give \dQuote{prob} type predictions, while for \dQuote{response} prediction it
#'   also needs \dQuote{response} predictions, this would be \code{c(response = "response",
#'   se = "prob")}. The names are the prediction types that are requested from this CPO, the
#'   values are types that this CPO will request from an underlying learner. If a name is not
#'   present, the \code{predict.type} is assumed not supported. Default is \code{c(response = "response")}.
#' @param .data.dependent [\code{logical(1)}]\cr
#'   Whether to make a data-dependent inverter CPO. If this is \code{FALSE}, the \code{cpo.trafo} function does not have
#'   a \code{data} parameter.
#' @param .packages [\code{character}]\cr
#'   Package(s) that should be loaded when the CPO is constructed. This gives the user an early error if
#'   a package required for the CPO is not available on his system, or can not be loaded. Default is \code{character(0)}.
#' @param cpo.trafo [\code{language} | \code{function} | \code{NULL}]\cr
#'   This can either be a function, or just the function body wrapped in curly braces.
#'   If this is a function, it must have the parameters \dQuote{data} and \dQuote{target},
#'   as well as the parameters specified in \dQuote{...} or \dQuote{.par.set}. (Alternatively,
#'   the function may have a dotdotdot argument). Depending on the values of \code{.trafo.type} and
#'   \code{.dataformat} -- see there --, it must return a \dQuote{data.frame}, a \dQuote{task},
#'   a dQuote{matrix}, \dQuote{list} of \dQuote{data.frame} and \dQuote{matrix} objects, or a retrafo function.
#'
#'   If \dQuote{cpo.retrafo} is given and \code{.trafo.type} is \dQuote{trafo.returns.data}, it must create a \dQuote{control}
#'   variable in its namespace, which will be passed on to \dQuote{cpo.retrafo}. If \dQuote{cpo.retrafo} is
#'   not given and \code{.trafo.type} is \dQuote{trafo.returns.data}, it must create a \dQuote{cpo.retrafo} function within its namespace, which will be called
#'   for re-transformation.
#'
#'   If \code{.trafo.type} is \dQuote{trafo.returns.control}, this function must return a \dQuote{cpo.retrafo} function.
#'
#'   If \code{.trafo.type} is
#'   \dQuote{stateless}, this argument may be \code{NULL}, or a function which just returns the transformed data.
#'
#'   If \dQuote{cpo.trafo} is a list of expressions (preferred), it is turned into a function by mlr, with the correct function arguments.
#' @param cpo.retrafo [\code{language} | \code{function}]\cr
#'   Similarly to \dQuote{cpo.trafo}, this is either a function, the function body in curly braces (preferred), or \code{NULL}.
#'   If this is not \code{NULL}, this function must have the same arguments as \code{cpo.trafo}, with the exception that
#'   the \dQuote{target} argument is replaced by a \dQuote{control} argument, which will be
#'   the value created in the \dQuote{cpo.trafo} run. It gets its input data in the same format as
#'   \dQuote{cpo.trafo}, with the exception that if \dQuote{.dataformat} is \dQuote{task}, it gets a
#'   \dQuote{data.frame} as if \dQuote{.dataformat} were \dQuote{df.all}. This function must similarly return an
#'   object in the same format as it received as input.
#'
#' @family CPO
#' @export
#'
#' @examples
#' # an example 'pca' CPO
#' # demonstrates the (object based) "trafo.returns.data" CPO API
#' pca = makeCPOExtended("pca",  # name
#'   center = TRUE: logical,  # one logical parameter 'center'
#'   .dataformat= "numeric",  # only handle numeric columns
#'   .trafo.type = "trafo.returns.data",  # default, can be omitted
#'   # cpo.trafo is given as a function body. The function head is added
#'   # automatically, containing 'data', 'target', and 'center'
#'   # (since a 'center' parameter was defined)
#'   cpo.trafo = {
#'     pcr = prcomp(as.matrix(data), center = center)
#'     # The following line creates a 'control' object, which will be given
#'     # to retrafo.
#'     control = list(rotation = pcr$rotation, center = pcr$center)
#'     pcr$x  # returning a matrix is ok
#'   # Just like cpo.trafo, cpo.retrafo is a function body, with implicit
#'   # arguments 'data', 'control', and 'center'.
#'   }, cpo.retrafo = {
#'     scale(as.matrix(data), center = control$center, scale = FALSE) %*%
#'       control$rotation
#'   })
#'
#' # an example 'scale' CPO
#' # demonstrates the (functional) "trafo.returns.data" CPO API
#' scaleCPO = makeCPOExtended("scale",
#'   .dataformat = "numeric",
#'   # .trafo.type = "trafo.returns.data" is implicit
#'   cpo.trafo = function(data, target) {
#'     result = scale(as.matrix(data), center = center, scale = scale)
#'     cpo.retrafo = function(data) {
#'       # here we can use the 'result' object generated in cpo.trafo
#'       scale(as.matrix(data), attr(result, "scaled:center"),
#'         attr(result, "scaled:scale"))
#'     }
#'     result
#'   }, cpo.retrafo = NULL)  # don't forget to set it cpo.retrafo to NULL
#'
#' # an example constant feature remover CPO
#' # demonstrates the "trafo.returns.control" CPO API
#' constFeatRem = makeCPOExtended("constFeatRem",
#'   .dataformat = "df.features",
#'   .trafo.type = "trafo.returns.control",
#'   cpo.trafo = function(data, target) {
#'     cols.keep = names(Filter(function(x) {
#'         length(unique(x)) > 1
#'       }, data))
#'     # the following function will do both the trafo and retrafo
#'     result = function(data) {
#'       data[cols.keep]
#'     }
#'     result
#'   }, cpo.retrafo = NULL)
#'
#' # an example 'square' CPO
#' # demonstrates the "stateless" CPO API
#' square = makeCPOExtended("scale",
#'   .dataformat = "numeric",
#'   .trafo.type = "stateless",
#'   cpo.trafo = function(data) {
#'     as.matrix(data) * 2
#'   }, cpo.retrafo = NULL) # optional, we don't need it since trafo & retrafo same
#'
#
# Developer Notes: Like all CPO defining functions, this one just calls makeCPOGeneral
makeCPOExtended = function(.cpo.name, ..., .par.set = NULL, .par.vals = list(),
                   .dataformat = c("df.features", "split", "df.all", "task", "factor", "ordered", "numeric"),
                   .dataformat.factor.with.ordered = TRUE,
                   .trafo.type = c("trafo.returns.data", "trafo.returns.control", "stateless", "retrafoless"),
                   .export.params = TRUE,  # FALSE, TRUE, names of parameters to export
                   .fix.factors = FALSE, .properties = c("numerics", "factors", "ordered", "missings"),
                   .properties.adding = character(0), .properties.needed = character(0),
                   .properties.target = c("cluster", "classif", "multilabel", "regr", "surv",
                     "oneclass", "twoclass", "multiclass"),
                   .packages = character(0),
                    cpo.trafo, cpo.retrafo) {
  # dotted parameter names are necessary to avoid problems with partial argument matching.
  # The reason cpo.trafo and cpo.retrafo are not dotted is that they always need to be given.
  # If that changes, they would also neede to be dotted.

  .dataformat = match.arg(.dataformat)
  .trafo.type = match.arg(.trafo.type)

  assertSubset(.properties, cpo.dataproperties)
  assertSubset(.properties.target, c(cpo.tasktypes, cpo.targetproperties))
  assertSubset(.properties.needed, cpo.dataproperties)

  makeCPOGeneral(.cpotype = "feature",
    .cpo.name = .cpo.name, .par.set = .par.set, .par.vals = .par.vals,
    .dataformat = .dataformat, .dataformat.factor.with.ordered = .dataformat.factor.with.ordered,
    .fix.factors = .fix.factors, .data.dependent = TRUE,
    .trafo.type = .trafo.type, .export.params = .export.params, .properties = .properties,
    .properties.adding = .properties.adding, .properties.needed = .properties.needed,
    .properties.target = .properties.target, .type.from = NULL, .type.to = NULL,
    .predict.type = NULL, .packages = .packages,
    cpo.trafo = substitute(cpo.trafo), cpo.retrafo = substitute(cpo.retrafo), ...)
}

#' @title Create a custom Target Operation CPO Constructor
#'
#' @rdname makeCPOExtended
#' @export
makeCPOTargetOpExtended = function(.cpo.name, ..., .par.set = NULL, .par.vals = list(),
                           .dataformat = c("df.features", "split", "df.all", "task", "factor", "ordered", "numeric"),
                           .dataformat.factor.with.ordered = TRUE,
                           .data.dependent = TRUE, .trafo.type = c("trafo.returns.data", "trafo.returns.control", "stateless"),
                           .export.params = TRUE,
                           .properties.data = c("numerics", "factors", "ordered", "missings"),
                           .properties.adding = character(0), .properties.needed = character(0),
                           .properties.target = character(0),
                           .type = c("cluster", "classif", "multilabel", "regr", "surv"),
                           .type.out = .type,
                           .predict.type = c(response = "response"),
                           .packages = character(0),
                           cpo.trafo, cpo.retrafo) {

  .type = match.arg(.type)
  .trafo.type = match.arg(.trafo.type)
  .type.out = match.arg(.type.out, choices = c("cluster", "classif", "multilabel", "regr", "surv"))

  possible.properties = list(multilabel = character(0), regr = character(0), cluster = character(0),
      classif = c("oneclass", "twoclass", "multiclass"))

  .dataformat = match.arg(.dataformat)

  assertFlag(.data.dependent)

  if (!.data.dependent) {
    if (.dataformat %in% c("df.all", "task")) {
      stop("When .data.dependent is FALSE, .dataformat must not be 'no' or 'task'")
    }
    if (!setequal(.properties.data, c("numerics", "factors", "ordered", "missings"))) {
      stop("When .data.dependent is FALSE, .properties must have the default value.")
    }
  }

  if (length(possible.properties[[.type]])) {
    assertSubset(.properties.target, possible.properties[[.type]])
    if (.type.out != .type && length(setdiff(.properties.target, .properties.adding))) {
      stopf("For conversion away from %s, .properties.adding must equal .properties.", .type)
    }
  } else if (length(.properties.target)) {
    stopf("CPO handling type %s must have empty properties.", .type)
  }

  if (length(possible.properties[[.type.out]])) {
    assertSubset(.properties.needed, possible.properties[[.type.out]])
  } else if (length(.properties.needed)) {
    stopf("Output type is %s, so .properties.needed must be empty.", .type.out)
  }

  predtypes = list(classif = c("response", "prob"), regr = c("response", "se"),
    cluster = c("response", "prob"), multilabel = c("response", "prob"),
    surv = c("response", "prob"))

  if (is.list(.predict.type)) {
    .predict.type = sapply(.predict.type, identity)
  }
  if (!isTRUE(checkCharacter(.predict.type, any.missing = FALSE, min.len = 1, names = "unique"))) {
    stop(".predict.type argument is not, and could not be converted into, a uniquely named character vector.")
  }
  if (!isTRUE(checkSubset(names(.predict.type), predtypes[[.type]]))) {
    stop("names of .predict.type must be a subset of the possible prediction types %s of input Task type %s.", predtypes[[.type]], .type)
  }
  if (!isTRUE(checkSubset(.predict.type, predtypes[[.type.out]]))) {
    stop(".predict.type values must be a subset of the possible prediction types %s of output Task type %s.", predtypes[[.type.out]], .type.out)
  }

  if (!"response" %in% names(.predict.type)) {
    stop("CPO must always support predict.type 'response', so .predict.type must have one value named 'response'.")
  }

  if (.predict.type["response"] != "response") {
    # the lower learner must provide what we need for 'response' prediction.
    # alternatively, we could drop the requirement that every learner / CPO must always be able to deliver response.
    .properties.needed = c(.properties.needed, unname(.predict.type["response"]))
  }

  .properties.adding = c(.properties.adding, setdiff(names(.predict.type), c("response", unname(.predict.type))))
  .properties.target = c(.properties.target, setdiff(names(.predict.type), "response"))

  makeCPOGeneral(.cpotype = "feature",
    .cpo.name = .cpo.name, .par.set = .par.set, .par.vals = .par.vals,
    .dataformat = .dataformat, .dataformat.factor.with.ordered = .dataformat.factor.with.ordered,
    .fix.factors = FALSE, .data.dependent = .data.dependent,
    .trafo.type = .trafo.type, .export.params = .export.params, .properties = .properties.data,
    .properties.adding = .properties.adding, .properties.needed = .properties.needed,
    .properties.target = .properties.target, .type.from = .type, .type.to = .type.out,
    .predict.type = .predict.type, .packages = .packages,
    cpo.trafo = substitute(cpo.trafo), cpo.retrafo = substitute(cpo.retrafo), ...)
}

# This is the central CPO defining function, for Feature Operating CPOs and Target Operating CPOs.
# It checks that the given parameters are valid, creates functions and ParamSet from nonstandardevaluation
# arguments, and then returns the CPO creator function.
# For parameters, see docu of `makeCPO`, `makeCPOExtended`, `makeCPOTargetOpExtended`.
makeCPOGeneral = function(.cpotype = c("feature", "target", "retrafoless"), .cpo.name, .par.set, .par.vals,
                          .dataformat, .dataformat.factor.with.ordered, .fix.factors, .data.dependent, .trafo.type, .export.params,
                          .properties, .properties.adding, .properties.needed,
                          .properties.target, .type.from, .type.to, .predict.type, .packages, cpo.trafo, cpo.retrafo, ...) {

  .cpotype = match.arg(.cpotype)
  assertFlag(.data.dependent)
  assertString(.cpo.name)
  assertList(.par.vals, names = "unique")
  assertFlag(.dataformat.factor.with.ordered)
  if (.cpotype == "retrafoless") assert(.trafo.type != "stateless")

  # we encode the information in .dataformat.factor.with.ordered into .dataformat:
  # split  --> most   | all
  # factor --> factor | onlyfactor
  if (.dataformat == "split") {
    .dataformat = ifelse(.dataformat.factor.with.ordered, "most", "all")
  } else if (.dataformat == "factor" && !.dataformat.factor.with.ordered) {
    .dataformat = "onlyfactor"
  }

  # Reserved parameter names:
  # these parameters are either special parameters given to the constructor function (id, affect.*),
  # the possible special values of 'export' that should not clash with param names,
  # special parameters given to the cpo.trafo function (data, target), special parameters given to the
  # cpo.retrafo function (predict.type, control),
  reserved.params = c("data", "df.features", "predict.type", "control", "id", "export", affect.params, export.possibilities)

  params = prepareParams(.par.set, .par.vals, .export.params, pSSLrn(..., .pss.env = parent.frame(2)), reserved.params)
  .par.set = params$.par.set
  .par.vals = params$.par.vals
  .export.params = params$.export.params

  if (.cpotype == "target") {
    assertCharacter(.predict.type, any.missing = FALSE, names = "unique")
  } else {
    # for feature operating CPOs, this is the identity.
    .predict.type = c(response = "response", prob = "prob", se = "se")
  }
  properties.list = assembleProperties(.properties, .properties.needed, .properties.adding, .properties.target, .cpotype, .type.from, .type.to)

  funargs = lapply(.par.set$pars, function(dummy) substitute())
  funargs = insert(funargs, .par.vals)

  trafo.funs = constructTrafoFunctions(funargs, cpo.trafo, cpo.retrafo, parent.frame(2),
    .cpo.name, .cpotype, .dataformat, .trafo.type, .data.dependent)

  funargs = insert(funargs, list(id = NULL, export = "export.default"))
  default.affect.args = list(affect.type = NULL, affect.index = integer(0),
    affect.names = character(0), affect.pattern = NULL, affect.invert = FALSE,
    affect.pattern.ignore.case = FALSE, affect.pattern.perl = FALSE, affect.pattern.fixed = FALSE)
  if (.cpotype != "retrafoless") {
    # "retrafoless" CPOs have no affect.* arguments
    funargs = insert(funargs, default.affect.args)
  }

  control.type = if (.trafo.type == "stateless") "stateless" else if (is.null(cpo.retrafo)) "functional" else "object"

  ####
  # The CPO creator function
  ####
  funbody = quote({
    # in the first two code lines, there are still arguments around that we don't know the names of.
    # therefore, we need to catch them into the 'args' list and delete them, always referencing the
    # functions we mean by '::'.
    args = base::match.call()
    base::rm(list = base::setdiff(base::ls(), "args"))  # delete all arguments to avoid name clashes
    args[[1]] = quote(list)
    args = eval(args, envir = parent.frame())
    nondefault.args = args
    args = insert(funargs, args)
    id = args$id
    if (is.null(id)) {
      id = .cpo.name
    }
    args$id = NULL
    export = args$export
    args$export = NULL
    if (!is.null(id)) {
      assertString(id)
    }
    if (.cpotype == "retrafoless") {
      # "retrafoless" CPOs must always take all data.
      affect.args = default.affect.args
    } else {
      affect.args = args[affect.params]
      names(affect.args) = substring(names(affect.args), 8)
      if (!is.null(affect.args$type)) {
        assertSubset(affect.args$type, c("numeric", "factor", "ordered", "other"))
      }
      assertIntegerish(affect.args$index, any.missing = FALSE, unique = TRUE)
      assertCharacter(affect.args$names, any.missing = FALSE, unique = TRUE)
      if (!is.null(affect.args$pattern)) {
        assertString(affect.args$pattern)
      }
      assertFlag(affect.args$invert)
      assertFlag(affect.args$pattern.ignore.case)
      assertFlag(affect.args$pattern.perl)
      assertFlag(affect.args$pattern.fixed)
      args = dropNamed(args, affect.params)
    }

    present.pars = Filter(function(x) !identical(x, substitute()), args[names(.par.set$pars)])
    checkParamsFeasible(.par.set, present.pars)

    nondefault.args = nondefault.args[intersect(names2(nondefault.args), names2(present.pars))]

    if (length(export) == 1 && export %in% export.possibilities) {
      export = switch(export,
        export.default = .export.params,
        export.set = names2(nondefault.args),
        export.default.set = intersect(.export.params, names2(nondefault.args)),
        export.unset = setdiff(names2(.par.set$pars), names2(nondefault.args)),
        export.default.unset = intersect(.export.params, setdiff(names2(.par.set$pars), names2(nondefault.args))),
        export.all = names2(.par.set$pars),
        export.none = character(0),
        export.all.plus = stop('"export" setting "export.all.plus" not yet supported.'),
        stopf('Unknown "export" setting "%s".', export))
    }
    if (length(.par.set$pars) == 0) {
      assert(identical(export, character(0)))
    } else {
      assertSubset(export, names2(.par.set$pars))
    }
    needed = setdiff(names2(Filter(function(x) is.null(x$requires), .par.set$pars)), names2(present.pars))
    missing = setdiff(needed, export)
    if (length(missing)) {
      singular = length(missing) == 1
      are = ifelse(singular, "is", "are")
      stopf("Paramter%s '%s' %s no default, %s not exported, and %s not given on construction.",
        ifelse(singular, "", "s"), collapse(missing, sep = "', '"), ifelse(singular, "has", "have"), are, are)
    }

    unexported.pars = dropNamed(present.pars, export)
    unexported.par.set = .par.set
    unexported.par.set$pars = dropNamed(.par.set$pars, export)
    .par.set$pars = .par.set$pars[export]
    present.pars = present.pars[intersect(names2(present.pars), export)]

    cpo = makeS3Obj(c("CPOPrimitive", "CPO"),
      # --- CPO part
      name = .cpo.name,                              # [character(1)] the name of the operation performed by this CPO
      debug.name = .cpo.name,                        # [character(1)] Readable representation of of name and ID
      par.set = .par.set,                            # [ParamSet] exported parameters
      par.vals = present.pars,                       # [named list] values of exported parameters
      properties = properties.list,                  # properties$properties: [character] properties handled by this CPO
                                                     # properties$adding [character] capabilities that this CPO adds to the next processor
                                                     # properties$needed [character] capabilities needed by the next processor
      properties.raw  = properties.list$properties,  # [character] properties handled by the cpo.trafo / cpo.retrafo internally, after filtering for affect.*
      operating.type = .cpotype,                     # [character(1)] one of "feature", "target", "retrafoless": what the CPO operates on
      predict.type = .predict.type,                  # [named character] translation of predict.type of underlying learner. Only for operating = "target"
      # --- CPOPrimitive part
      id = NULL,                                     # [character(1)] ID of the CPO -- prefix to parameters and possibly postfix to printed name
      trafo = trafo.funs$cpo.trafo,                  # [function] trafo function
      retrafo = trafo.funs$cpo.retrafo,              # [function] retrafo function
      control.type = control.type,                   # [character(1)] whether retrafo is taken from trafo environment ("functional"),
                                                     #                uses 'control' object ("object"), or is stateless ("stateless")
      packages = .packages,                          # [character] package(s) to load when constructing the CPO
      affect.args = affect.args,                     # [named list] values of the "affect.*" arguments
      unexported.pars = unexported.pars,             # [named list] values of parameters that are not exported
      unexported.par.set = unexported.par.set,       # [ParamSet] unexported parameter set
      bare.par.set = .par.set,                       # [ParamSet] exported parameters with names not containing the ID prefix
      datasplit = .dataformat,                       # [character(1)] data format as received by trafo / retrafo
      fix.factors = .fix.factors,                    # [logical(1)] whether to clean up factor levels in retrafo
      # --- Target Operating CPO relevant things
      convertfrom = .type.from,                      # [character(1)] task type to convert from.
      convertto = .type.to,                          # [character(1)] task type to convert to.
      data.dependent = .data.dependent)              # [logical(1)] whether trafo uses data at all.
    if (length(getCPOAffect(cpo))) {
      # data is subset, so the overall 'properties' is the maximal set
      cpo$properties$properties = union(cpo$properties$properties,  c("numerics", "factors", "ordered", "missings"))
    }
    requireCPOPackages(cpo)
    setCPOId(cpo, id)  # this also adjusts par.set and par.vals
  })
  addClasses(eval(call("function", as.pairlist(funargs), funbody)), "CPOConstructor")
}

# check the validity of .properties, .properties.needed, .properties.adding, .properties.target and assemble
# the 'properties' list that will be part of the CPO
# @param .properties [character] properties a CPO can handle
# @param .properties.needed [character] properties that a unit coming after the current CPO must be able to handle
# @param .properties.adding [character] properties that this CPO adds to a learner / another CPO when attached / prepended to it.
# @param .properties.target [character] target properties (e.g. prediction type, task types)
# @param .cpotype [character(1)] one of "feature", "target", or "retrafoless" -- the type of CPO being built
# @param .type.from [character(1)] only for target operating CPO: specify what type of task the CPO operates on
# @param .type.to [character(1)] only for target operating CPO: specify what type of task results when the CPO is applied
# @return [list] list(properties, properties.adding, properties.needed) to be used as the `$properties` slot of a CPO object.
assembleProperties = function(.properties, .properties.needed, .properties.adding, .properties.target, .cpotype, .type.from, .type.to) {
  assertCharacter(.properties, unique = TRUE)
  assertCharacter(.properties.needed, unique = TRUE)
  assertCharacter(.properties.adding, unique = TRUE)
  assertCharacter(.properties.target, unique = TRUE)

  if (.cpotype == "target") {
    assertChoice(.type.from, cpo.tasktypes)
    assertChoice(.type.to, cpo.tasktypes)
    if (.type.from != .type.to) {
      .properties.adding = union(.properties.adding, .type.from)
      .properties.needed = union(.properties.needed, .type.to)
    }
    .properties = union(.properties, .type.from)
  } else {
    .properties = c(.properties, "prob", "se")
  }

  if (length(.properties)) {
    assertSubset(.properties.adding, .properties)
  } else {
    # doing this because a bug in assertSubset doesn't recognize the empty set as a subset of itself.
    assert(length(.properties.adding) == 0)
  }
  .properties = union(.properties, .properties.target)
  badprops = intersect(.properties.adding, .properties.needed)
  if (length(badprops)) {
    stopf(".properties.adding and .properties.needed must not contain the same properties, but both contained %s.",
      collapse(badprops, sep = ", "))
  }
  list(properties = .properties,
    properties.adding = .properties.adding,
    properties.needed = .properties.needed)
}

# All "affect.*" parameters
affect.params = c("affect.type", "affect.index", "affect.names", "affect.pattern", "affect.invert", "affect.pattern.ignore.case",
  "affect.pattern.perl", "affect.pattern.fixed")
# The "export" parameter may either contain a list of names of parameters to export, or one of these special values:
export.possibilities = c("export.default", "export.set", "export.default.set", "export.unset", "export.default.unset",
  "export.all", "export.none", "export.all.plus")

# check the given parameter set and parameters for validity.
# .par.set.
# @param .par.set [ParamSet | NULL] parameter set given to CPOConstructor to create, or NULL if none given
# @param .par.vals [list] named list of default parameter values
# @param .export.params [logical(1) | character] TRUE if all parameters are to be exported, FALSE if none are to be
#   exported, a character vector of parameter names that are to be exported otherwise.
# @param addnl.par.set [ParamSet] (addnl = "additional") are the params given as `...` and are added to the already present parameters
# @param reserved.params [character] parameter names that are reserved and may not be used for CPOs.
# @return [list] list(.par.set, .par.vals, .export.params)
prepareParams = function(.par.set, .par.vals, .export.params, addnl.par.set, reserved.params) {
  if (is.null(.par.set)) {
    .par.set = addnl.par.set
  } else {
    .par.set = c(.par.set, addnl.par.set)
  }

  if (any(names(.par.set$pars) %in% reserved.params)) {
    stopf("Parameters %s are reserved", collapse(reserved.params, ", "))
  }
  .par.vals = insert(getParamSetDefaults(.par.set), .par.vals)
  assert(length(setdiff(names(.par.vals), names(.par.set$pars))) == 0)
  .par.vals = convertItemsToNamesDVP(.par.vals, .par.set)
  checkParamsFeasible(.par.set, .par.vals)

  if (isTRUE(.export.params)) {
    .export.params = names2(.par.set$pars)
  } else if (isFALSE(.export.params)) {
    .export.params = character(0)
  } else {
    assertSubset(.export.params, names2(.par.set$pars))
  }

  list(.par.set = .par.set, .par.vals = .par.vals, .export.params = .export.params)
}

# Create cpo.trafo and cpo.retrafo from provided expressions and put them into canonical form
#
# Internally, cpo.trafo and cpo.retrafo must be either function- or object-based, and must furthermore
# be split up
# @param funargs [list] named list of function arguments for cpo.trafo and cpo.retrafo. The names are
#   treated as parameter names, the values as default parameter values. This list must contain the
#   CPO parameters, as well as id, and export, and affect.* parameters. Only `data`, `control`, `predict.type`
#   must be absent and will be added here.
# @param cpo.trafo [language | function] the CPO trafo function, in a format as used by `makeFunction`
# @param cpo.retrafo [language | function] the CPO retrafo function, in a format as used by `makeFunction`
# @param eval.env [environment] the environment in which to evaluate `cpo.trafo` and `cpo.retrafo`
# @param .cpo.name [character(1)] name of the CPO, for error messages
# @param .cpotype [character(1)] whether CPO is target or data operating; must be either "target" or "retrafoless"
# @param .dataformat [character(1)] data format used by cpo.trafo and cpo.retrafo, in its internally used semantics. Must
#   be one of "most", "all", "factor", "onlyfactor", "numeric", "ordered", "df.features", "split", "task"
# @param .trafo.type [character(1)] one of "trafo.returns.control", "trafo.returns.data", "stateless": How and whether trafo returns a control object
# @param .data.dependent [logical(1)] whether training is (feature) data dependent. Must be TRUE if `.cpotype` is not "target".
# @return [list] list(cpo.trafo, cpo.retrafo, cpo.trafo.orig) trafo and retrafo as used internally when handling CPO. "cpo.trafo.orig"
#   is needed by print.CPOConstructor for pretty printing of the trafo function, since the internal representation of `cpo.trafo` returned
#   otherwise is not informative.
constructTrafoFunctions = function(funargs, cpo.trafo, cpo.retrafo, eval.env, .cpo.name, .cpotype, .dataformat, .trafo.type, .data.dependent) {
  required.arglist.trafo = funargs
  if (!.data.dependent) {
    assert(.cpotype == "target")
  } else {
    required.arglist.trafo$data = substitute()
  }
  .stateless = .trafo.type == "stateless"
  if (!.stateless || !is.null(cpo.retrafo) || .cpotype == "target") {
    required.arglist.trafo$target = substitute()
  }
  if ((is.recursive(cpo.trafo) && identical(cpo.trafo[[1]], quote(`{`))) || !is.null(eval(cpo.trafo, envir = eval.env))) {
    cpo.trafo = makeFunction(cpo.trafo, required.arglist.trafo, env = eval.env)
  } else {
    stop("cpo.trafo must be provided.")
  }

  if (.cpotype == "target" && !.stateless && .dataformat == "df.all") {
    .dataformat = "df.features"
  }

  if ((is.recursive(cpo.retrafo) && identical(cpo.retrafo[[1]], quote(`{`))) || !is.null(eval(cpo.retrafo, envir = eval.env))) {
    if (.cpotype == "retrafoless") {
      stop("retrafoless cpo must have cpo.retrafo = NULL")
    }

    required.arglist.retrafo = funargs
    if (.cpotype == "target") {
      required.arglist.retrafo$target = substitute()
      required.arglist.retrafo$predict.type = substitute()
    } else if (.data.dependent) {
      required.arglist.retrafo$data = substitute()
    }
    if (!.stateless) {
      required.arglist.retrafo$control = substitute()
    }
    cpo.retrafo = makeFunction(cpo.retrafo, required.arglist.retrafo, env = eval.env)
    if (is.null(cpo.trafo) && .stateless) {
      cpo.trafo = function(target, ...) cpo.retrafo(...)
    }
  } else if (.stateless) {
    if (.cpotype == "target") {
      # stateless, no cpo.trafo, type is 'target'
      stop("A Target Operating CPO must have a cpo.retrafo function, even if stateless.")
    } else if (.dataformat %in% c("task", "df.all")) {
      # stateless, no cpo.retrafo, .dataformat is "task" or "df.all"
      stop("A stateless CPO without cpo.retrafo cannot have .dataformat 'task' or 'df.all'.")
    }
  } else {
    cpo.retrafo = NULL
  }
  cpo.trafo.new = cpo.trafo
  if (.trafo.type == "trafo.returns.control") {
    if (is.null(cpo.retrafo)) {
      # functional
      cpo.trafo.new = function(data, target, ...) {
        cpo.retrafo = cpo.trafo(data = data, target = target, ...)
        if (!isTRUE(checkFunction(cpo.retrafo, nargs = 1))) {
          stopf("CPO %s cpo.trafo did not generate a retrafo function with one argument.", .cpo.name)
        }
        cpo.retrafo(data)
      }
    } else {
      # object based
      cpo.trafo.new = function(data, target, ...) {
        control = cpo.trafo(data = data, target = target, ...)
        cpo.retrafo(data = data, control = control, ...)
      }
    }
  }
  if (!.stateless) {
    cpo.trafo.new = captureEnvWrapper(cpo.trafo.new)
  }

  list(cpo.trafo = cpo.trafo.new, cpo.retrafo = cpo.retrafo, cpo.trafo.orig = cpo.trafo)
}

# create a function with expressions 'expr' in the environment 'env'.
#
# the function gets the argument list 'required.arglist.
# if 'expr' is actually a function, we just check that it has at least all the
# arguments in 'required.arglist' (or that is has ellipses), that all
# arguments have the same default values as required.arglist, and that
# arguments of the function that are not 'required' have a default value so
# there won't be an error when the function gets called later.
# @param expr [language | function] if this is an expression starting with `{`, the created function will have
#   this expression as its body. Otherwise `expr` is interpreted as a function name or definition, the returned
#   function will just be the function given.
# @param required.arglist [list] named list of arguments that will be used as function parameters
#   for the newly created function. If `expr` is an expression starting with `{`, the names correspond to parameter
#   names, the values are the default parameter values, use `substitute()` for parameters without default.
#   If `expr` is a function or function definition, it is checked that `do.call(expr, required.arglist, quote = TRUE)` would not
#   give an error for missing values or unused arguments. For this, only the names of `required.arglist`
#   are used: the function parameters of `expr` are checked that they (1a) either contain `...` or (1b)
#   contain all parameter names in the list. It is also checked that (2) all function parameters of `expr`
#   that do not have a default value, occur in `required.arglist`.
# @parem env [environment] the environment that will be the parent.env of the function created, or in which
#   `expr` is evaluated.
# @return [function] a function which can handle arguments as requested by `required.arglist` and either has the function
#   body or is the function defined by `expr`.
makeFunction = function(expr, required.arglist, env = parent.frame()) {
  if (is.recursive(expr) && identical(expr[[1]], quote(`{`))) {
    # we have a headless list of expressions
    # so we build our own function
    args = as.pairlist(required.arglist)
    newfun = eval(call("function", args, expr), envir = env)
  } else {
    newfun = eval(expr, envir = env)
    assertFunction(newfun)
    if (!"..." %in% names(formals(newfun))) {
      # with a vararg function we tentatively trust the function
      # handles its arguments

      assertFunction(newfun, args = names(required.arglist))
    }
    for (arg in names(formals(newfun))) {
      if (arg == "...") {
        # can't say anything about ellipses
        next
      }
      if (identical(formals(newfun)[[arg]], substitute())) {
        # the argument has no default, so we only care that this it will always be given
        if (!arg %in% names(required.arglist)) {
          stopf("Bad argument %s.\nNeed a function with arguments %s.", arg, collapse(names(required.arglist)), sep = ", ")
        }
        next
      }
      if (!arg %in% names(required.arglist)) {
        # the argument is not required, but it has a default; that is fine.
        next
      }
      if (!identical(formals(newfun)[[arg]], required.arglist[[arg]])) {
        if (identical(formals(newfun)[[arg]], substitute())) {
          funargstr = "no default"
          funarg = "no default"
        } else {
          funarg = eval(formals(newfun)[[arg]], envir = env)
          funargstr = sprintf("default %s", collapse(funarg))
        }
        if (identical(required.arglist[[arg]], substitute())) {
          reqarg = "no default"
        } else {
          reqarg = required.arglist[[arg]]
        }
        # there is a chance that they evaluate to the same value, so we check again
        if (!identical(funarg, reqarg) && (!length(funarg) == 1 || !length(reqarg) == 1 || !is.na(funarg) || !is.na(reqarg))) {
          stopf("Given function parameter %s has %s, but required default is %s.",
                arg, funargstr, collapse(reqarg))
        }
      }
    }
  }
  newfun
}

# capture the environment of the call to 'fun'
#
# This puts a wrapper around a function so when it is called, the call environment
# will be assigned to a variable `.ENV`.
#
# Example:
# ```
# f = function(x) x
# fw = captureEnvWrapper(f)
# fw(10)
# ls(.ENV)  # .ENV variable was created, contains "x"
# .ENV$x    # == 10, since fw(10) assigned 10 to x
# @param fun [function] the function to wrap
# @return [function] a function that behaves the same, as `fun`, but also creates
#   the variable `.ENV` when called.
captureEnvWrapper = function(fun) {
  envcapture = quote({ assign(".ENV", environment(), envir = parent.frame()) ; 0 })
  envcapture[[3]] = body(fun)
  body(fun) = envcapture
  environment(fun) = new.env(parent = environment(fun))
  fun
}
