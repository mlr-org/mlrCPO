# makeCPO.R contains all CPO constructor functions, that are used both
# internally and are exported for user use.

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
                     "oneclass", "twoclass", "multiclass", "lcens", "rcens", "icens"),
                   packages = character(0), cpo.trafo) {
  dataformat = match.arg(dataformat)

  assertSubset(properties, cpo.dataproperties)
  assertSubset(properties.target, c(cpo.tasktypes, cpo.targetproperties))
  assertSubset(properties.needed, cpo.dataproperties)

  makeCPOGeneral(.cpotype = "databound",
    .cpo.name = cpo.name, .par.set = par.set, .par.vals = par.vals,
    .dataformat = dataformat, .dataformat.factor.with.ordered = dataformat.factor.with.ordered,
    .fix.factors = fix.factors, .data.dependent = TRUE,
    .trafo.type = "trafo.returns.control", .export.params = export.params, .properties = properties,
    .properties.adding = properties.adding, .properties.needed = properties.needed,
    .properties.target = properties.target, .type.from = NULL, .type.to = NULL,
    .predict.type = NULL, .packages = packages,
    cpo.trafo = substitute(cpo.trafo), cpo.retrafo = NULL)

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
#'   Parameters of the CPO, in the format of \code{\link[mlr]{pSSLrn}}.
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
#'     .dataformat \tab data                          \tab target                \cr
#'     ---         \tab ---                             \tab ---                   \cr
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
                   .trafo.type = c("trafo.returns.data", "trafo.returns.control", "stateless"),
                   .export.params = TRUE,  # FALSE, TRUE, names of parameters to export
                   .fix.factors = FALSE, .properties = c("numerics", "factors", "ordered", "missings"),
                   .properties.adding = character(0), .properties.needed = character(0),
                   .properties.target = c("cluster", "classif", "multilabel", "regr", "surv",
                     "oneclass", "twoclass", "multiclass", "lcens", "rcens", "icens"),
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

  makeCPOGeneral(.cpotype = "databound",
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
makeCPOTargetOp = function(.cpo.name, ..., .par.set = NULL, .par.vals = list(),
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

  makeCPOGeneral(.cpotype = "targetbound",
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
makeCPOGeneral = function(.cpotype = c("databound", "targetbound"), .cpo.name, .par.set, .par.vals,
                          .dataformat, .dataformat.factor.with.ordered, .fix.factors, .data.dependent, .trafo.type, .export.params,
                          .properties, .properties.adding, .properties.needed,
                          .properties.target, .type.from, .type.to, .predict.type, .packages, cpo.trafo, cpo.retrafo, ...) {

  .cpotype = match.arg(.cpotype)
  assertFlag(.data.dependent)
  assertString(.cpo.name)
  assertList(.par.vals, names = "unique")
  .stateless = .trafo.type == "stateless"


  if (is.null(.par.set)) {
    .par.set = pSSLrn(..., .pss.env = parent.frame(2))
  }

  assertCharacter(.properties, unique = TRUE)
  assertCharacter(.properties.needed, unique = TRUE)
  assertCharacter(.properties.adding, unique = TRUE)
  assertCharacter(.properties.target, unique = TRUE)
  if (is.null(.predict.type)) {
    # for databound CPOs, this is the identity.
    .predict.type = c(response = "response", prob = "prob", se = "se")
    .properties = c(.properties, "prob", "se")
  }
  assertCharacter(.predict.type, any.missing = FALSE, names = "unique")

  if (.cpotype == "targetbound") {
    assertChoice(.type.from, cpo.tasktypes)
    assertChoice(.type.to, cpo.tasktypes)
    if (.type.from != .type.to) {
      .properties.adding = union(.properties.adding, .type.from)
      .properties.needed = union(.properties.needed, .type.to)
    }
    .properties = union(.properties, .type.from)
  }

  if (length(.properties)) {
    assertSubset(.properties.adding, .properties)
  } else {
    assert(length(.properties.adding) == 0)
  }
  .properties = union(.properties, .properties.target)

  badprops = intersect(.properties.adding, .properties.needed)
  if (length(badprops)) {
    stopf(".properties.adding and .properties.needed must not contain the same properties, but both contained %s.",
      collapse(badprops, sep = ", "))
  }

  # these parameters are either special parameters given to the constructor function (id, affect.*),
  # the possible special values of 'export' that should not clash with param names,
  # special parameters given to the cpo.trafo function (data, target), special parameters given to the
  # cpo.retrafo function (predict.type, control),

  affect.params = c("affect.type", "affect.index", "affect.names", "affect.pattern", "affect.invert", "affect.pattern.ignore.case",
    "affect.pattern.perl", "affect.pattern.fixed")
  export.possibilities = c("export.default", "export.set", "export.default.set", "export.unset", "export.default.unset",
    "export.all", "export.none", "export.all.plus")
  reserved.params = c("data", "df.features", "predict.type", "control", "id", "export", affect.params, export.possibilities)
  if (any(names(.par.set$pars) %in% reserved.params)) {
    stopf("Parameters %s are reserved", collapse(reserved.params, ", "))
  }

  if (isTRUE(.export.params)) {
    .export.params = names2(.par.set$pars)
  } else if (isFALSE(.export.params)) {
    .export.params = character(0)
  } else {
    assertSubset(.export.params, names2(.par.set$pars))
  }


  .par.vals = insert(getParamSetDefaults(.par.set), .par.vals)

  assert(length(setdiff(names(.par.vals), names(.par.set$pars))) == 0)

  .par.vals = convertItemsToNamesDVP(.par.vals, .par.set)

  checkParamsFeasible(.par.set, .par.vals)

  funargs = lapply(.par.set$pars, function(dummy) substitute())
  funargs = insert(funargs, .par.vals)

  required.arglist.trafo = funargs
  if (.data.dependent) {
    required.arglist.trafo$data = substitute()
  }
  required.arglist.trafo$target = substitute()
  trafo.expr = cpo.trafo
  if (!.stateless || (is.recursive(trafo.expr) && identical(trafo.expr[[1]], quote(`{`))) || !is.null(eval(cpo.trafo, env = parent.frame(2)))) {
    cpo.trafo = makeFunction(trafo.expr, required.arglist.trafo, env = parent.frame(2))
  } else if (.cpotype == "targetbound") {
    stop("A target-bound CPO must have a cpo.trafo function, even if stateless.")
  } else if (.dataformat %in% c("task", "df.all")) {
    stop("A stateless CPO without cpo.trafo cannot have .dataformat 'task' or 'no'.")
  } else {
    if (.dataformat == "df.all") {
      .dataformat = "df.features"
    }
    cpo.trafo = NULL
  }
  if (.trafo.type == "trafo.returns.control") {
    retrafo.gen = cpo.trafo
    cpo.trafo = function(data, target, ...) {
      cpo.retrafo = retrafo.gen(data, target, ...)
      if (!isTRUE(checkFunction(cpo.retrafo, nargs = 1))) {
        stopf("CPO %s cpo.trafo did not generate a retrafo function with one argument.", .cpo.name)
      }
      cpo.retrafo(data)
    }
  }

  retrafo.expr = cpo.retrafo
  if ((is.recursive(retrafo.expr) && identical(retrafo.expr[[1]], quote(`{`))) || !is.null(eval(cpo.retrafo, env = parent.frame(2)))) {
    if (.trafo.type == "trafo.returns.control") {
      stop("Combined retrafo must have cpo.retrafo = NULL")
    }
    required.arglist.retrafo = funargs
    if (.cpotype == "targetbound") {
      required.arglist.retrafo$target = substitute()
      required.arglist.retrafo$predict.type = substitute()
    } else if (.data.dependent) {
      required.arglist.retrafo$data = substitute()
    }
    if (!.stateless) {
      required.arglist.retrafo$control = substitute()
    }
    cpo.retrafo = makeFunction(retrafo.expr, required.arglist.retrafo, env = parent.frame(2))
    if (is.null(cpo.trafo) && .stateless) {
      cpo.trafo = function(target, ...) cpo.retrafo(...)
    }
  } else if (.stateless) {
    stop("Stateless CPO must provide cpo.retrafo.")
  } else {
    cpo.retrafo = NULL
  }

  cpo.trafo = captureEnvWrapper(cpo.trafo)

  funargs = insert(funargs, list(id = NULL, export = "export.default",
    affect.type = NULL, affect.index = integer(0),
    affect.names = character(0), affect.pattern = NULL, affect.invert = FALSE,
    affect.pattern.ignore.case = FALSE, affect.pattern.perl = FALSE, affect.pattern.fixed = FALSE))

  if (.dataformat == "split") {
    .dataformat = ifelse(.dataformat.factor.with.ordered, "most", "all")
  } else if (.dataformat == "factor" && !.dataformat.factor.with.ordered) {
    .dataformat = "onlyfactor"
  }

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
    args$id = NULL
    export = args$export
    args$export = NULL
    if (!is.null(id)) {
      assertString(id)
    }
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

    unexported.args = dropNamed(present.pars, export)
    unexported.pars = dropNamed(.par.set$pars, export)
    .par.set$pars = .par.set$pars[export]
    present.pars = present.pars[intersect(names2(present.pars), export)]

    cpo = makeS3Obj(c("CPOPrimitive", "CPO"),
      # --- CPO part
      bare.name = .cpo.name,
      name = .cpo.name,
      par.set = .par.set,
      par.vals = present.pars,
      properties = list(properties = .properties,
        properties.data = .properties,
        properties.adding = .properties.adding,
        properties.needed = .properties.needed),
      bound = .cpotype,
      predict.type = .predict.type,
      # --- CPOPrimitive part
      id = NULL,
      packages = .packages,
      affect.args = affect.args,
      unexported.args = unexported.args,
      unexported.pars = unexported.pars,
      bare.par.set = .par.set,
      datasplit = .dataformat,
      stateless = .stateless,
      fix.factors = .fix.factors,
      type = ifelse(is.null(cpo.retrafo), "functional", "object"),
      trafo = cpo.trafo,
      retrafo = cpo.retrafo,
      convertfrom = .type.from,
      convertto = .type.to,
      data.dependent = .data.dependent,
      hybrid.inverter = .cpotype == "targetbound" && .stateless)
    if (length(getCPOAffect(cpo))) {
      # data is subset, so the overall 'properties' is the maximal set
      cpo$properties$properties = union(cpo$properties$properties,  c("numerics", "factors", "ordered", "missings"))
    }
    requireCPOPackages(cpo)
    setCPOId(cpo, id)  # this also adjusts par.set and par.vals
  })
  addClasses(eval(call("function", as.pairlist(funargs), funbody)), "CPOConstructor")
}
