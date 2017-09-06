# makeCPO.R contains all CPO constructor functions, that are used both
# internally and are exported for user use.

#' @include listCPO.R parameters.R generics.R CPO_operators.R

cpo.dataproperties = c("numerics", "factors", "ordered", "missings")
cpo.tasktypes = c("cluster", "classif", "multilabel", "regr", "surv")  # these are the SUPPORTED tasks
cpo.targetproperties = c("oneclass", "twoclass", "multiclass")
cpo.predict.properties = c("prob", "se")

#' @title Create a custom CPO constructor
#'
#' @description
#' \code{makeCPO} creates a Feature Operation CPO constructor, i.e. a constructor for a CPO that will
#' operate on feature columns. \code{makeCPOTargetOp} creates a Target Operation CPO constructor, which
#' creates CPOs that operate on the target column.
#'
#' \code{makeCPO} has a comparatively easy user-interface; for more advanced use-cases and interesting shortcuts,
#' use \code{\link{makeCPOExtended}}.
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
#'
#' @family CPO
#' @export
#'
#' @examples
#' # an example constant feature remover CPO
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
#'   })
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
#'   cpo.trafo = NULL, # optional, we don't need it since trafo & retrafo same
#'   cpo.retrafo = function(data) {
#'     as.matrix(data) * 2
#'   })
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
    if (!setequal(.properties, c("numerics", "factors", "ordered", "missings"))) {
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
    .trafo.type = .trafo.type, .export.params = .export.params, .properties = .properties.target,
    .properties.adding = .properties.adding, .properties.needed = .properties.needed,
    .properties.target = .properties, .type.from = .type, .type.to = .type.out,
    .predict.type = .predict.type, .packages = .packages,
    cpo.trafo = substitute(cpo.trafo), cpo.retrafo = substitute(cpo.retrafo), ...)
}

# This is the central CPO defining function, for Feature Operating CPOs and Target Operating CPOs.
# It checks that the given parameters are valid, creates functions and ParamSet from nonstandardevaluation
# arguments, and then returns the CPO creator function.
makeCPOGeneral = function(.cpotype = c("feature", "target", "traindata"), .cpo.name, .par.set, .par.vals,
                          .dataformat, .dataformat.factor.with.ordered, .fix.factors, .data.dependent, .trafo.type, .export.params,
                          .properties, .properties.adding, .properties.needed,
                          .properties.target, .type.from, .type.to, .predict.type, .packages, cpo.trafo, cpo.retrafo, ...) {

  .cpotype = match.arg(.cpotype)
  assertFlag(.data.dependent)
  assertString(.cpo.name)
  assertList(.par.vals, names = "unique")
  assertFlag(.dataformat.factor.with.ordered)
  if (.cpotype == "traindata") assert(.trafo.type != "stateless")

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
    .cpo.name, .cpotype, .dataformat, .trafo.type, .data.dependent, .trafo.type == "stateless")

  funargs = insert(funargs, list(id = NULL, export = "export.default"))
  default.affect.args = list(affect.type = NULL, affect.index = integer(0),
    affect.names = character(0), affect.pattern = NULL, affect.invert = FALSE,
    affect.pattern.ignore.case = FALSE, affect.pattern.perl = FALSE, affect.pattern.fixed = FALSE)
  if (.cpotype != "traindata") {
    # "traindata" CPOs have no affect.* arguments
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
    if (.cpotype == "traindata") {
      # "traindata" CPOs must always take all data.
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
      operating.type = .cpotype,                     # [character(1)] one of "feature", "target", "traindata": what the CPO operates on
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
# addnl.par.set are the params given as `...` and are added to the already present
# .par.set.
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

constructTrafoFunctions = function(funargs, cpo.trafo, cpo.retrafo, eval.env, .cpo.name, .cpotype, .dataformat, .trafo.type, .data.dependent, .stateless) {
  required.arglist.trafo = funargs
  if (!.data.dependent) {
    assert(.cpotype == "target")
  } else {
    required.arglist.trafo$data = substitute()
  }
  required.arglist.trafo$target = substitute()
  if ((is.recursive(cpo.trafo) && identical(cpo.trafo[[1]], quote(`{`))) || !is.null(eval(cpo.trafo, envir = eval.env))) {
    cpo.trafo = makeFunction(cpo.trafo, required.arglist.trafo, env = eval.env)
  } else {
    stop("cpo.trafo must be provided.")
  }

  if (.cpotype == "target" && !.stateless && .dataformat == "df.all") {
    .dataformat = "df.features"
  }

  if ((is.recursive(cpo.retrafo) && identical(cpo.retrafo[[1]], quote(`{`))) || !is.null(eval(cpo.retrafo, envir = eval.env))) {
    if (.cpotype == "traindata") {
      stop("traindata cpo must have cpo.retrafo = NULL")
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
  cpo.trafo.orig = cpo.trafo
  if (.trafo.type == "trafo.returns.control") {
    if (is.null(cpo.retrafo)) {
      # functional
      cpo.trafo = function(data, target, ...) {
        cpo.retrafo = cpo.trafo.orig(data = data, target = target, ...)
        if (!isTRUE(checkFunction(cpo.retrafo, nargs = 1))) {
          stopf("CPO %s cpo.trafo did not generate a retrafo function with one argument.", .cpo.name)
        }
        cpo.retrafo(data)
      }
    } else {
      # object based
      cpo.trafo = function(data, target, ...) {
        control = cpo.trafo.orig(data = data, target = target, ...)
        cpo.retrafo(data = data, control = control, ...)
      }
    }
  }
  if (!.stateless) {
    cpo.trafo = captureEnvWrapper(cpo.trafo)
  }

  list(cpo.trafo = cpo.trafo, cpo.retrafo = cpo.retrafo, cpo.trafo.orig = cpo.trafo.orig)
}

# create a function with expressions 'expr' in the environment 'env'.
# the function gets the argument list 'required.arglist.
# if 'expr' is actually a function, we just check that it has at least all the
# arguments in 'required.arglist' (or that is has ellipses), that all
# arguments have the same default values as required.arglist, and that
# arguments of the function that are not 'required' have a default value so
# there won't be an error when the function gets called later.
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
captureEnvWrapper = function(fun) {
  envcapture = quote({ assign(".ENV", environment(), envir = parent.frame()) ; 0 })
  envcapture[[3]] = body(fun)
  body(fun) = envcapture
  environment(fun) = new.env(parent = environment(fun))
  fun
}
