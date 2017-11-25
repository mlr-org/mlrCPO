# properties.R contains getXXX and setXXX functions that apply to CPO objects.
#' @include callCPO.R

#' @title Get the CPO object's Name
#'
#' @description
#' Return the name associated with a \code{\link{CPO}} operation. This name
#' is set when creating a \code{\link{CPOConstructor}}, e.g. using
#' \code{\link{makeCPO}}, by the \dQuote{.cpo.name} parameter.
#' It is also the default \code{id}, as retrieved by \code{\link{getCPOId}},
#' of a CPO.
#'
#' @template arg_cpo
#' @return [\code{character(1)}] the CPO's name.
#'
#' @family getters and setters
#' @export
getCPOName = function(cpo) {
  UseMethod("getCPOName")
}

#' @title Set the ID of a CPO object.
#'
#' @description
#' Sets the \emph{id} of a \code{\link{CPO}}. Setting the id
#' is also possible during construction by a \code{\link{CPOConstructor}}
#' using the \code{id} parameter.
#'
#' The exported hyperparameters of a CPO will all have the
#' id as prefix. This makes it possible to
#' compose CPOs that have clashing parameter names.
#'
#' @template arg_cpo
#' @param id [\code{character(1)} | \code{NULL}]\cr
#'   The ID. If this is \code{NULL}, the ID is set to the
#'   default for the CPO at hand, which is the CPO \dQuote{name}, see \code{\link{getCPOName}}.
#' @return [\code{CPO}] the CPO with modified id.
#'
#' @family getters and setters
#' @family CPO ID related
#' @export
setCPOId = function(cpo, id) {
  if (!is.null(id)) {
    assertString(id)
  }
  UseMethod("setCPOId")
}

#' @export
setCPOId.default = function(cpo, id) {
  stop("setCPOId for object not defined.")
}


#' @title Get the ID of a CPO object.
#'
#' @description
#' Gets the \emph{id} of a \code{\link{CPO}}. The id can be set
#' during construction by a \code{\link{CPOConstructor}}
#' using the \code{id} parameter, or with \code{\link{setCPOId}}.
#'
#' The exported hyperparameters of a CPO all have the
#' id as prefix. This makes it possible to
#' compose CPOs that have clashing parameter names.
#'
#' @template arg_cpo
#' @return [\code{character(1)}] the CPO's id.
#'
#' @family getters and setters
#' @family CPO ID related
#' @export
getCPOId = function(cpo) {
  UseMethod("getCPOId")
}

#' @title Get the Properties of the Given CPO Object.
#'
#' @description
#' The properties of a \code{\link{CPO}} object determine the kind of data the CPO will be able to handle, and how
#' it transforms data. Properties describe what kind of data a CPO can work with.
#'
#' This function returns a list of three values: \code{$properties}, \code{$properties.adding}, and
#' \code{$properties.needed}.
#'
#' The \code{$properties} determines what data the CPO handles. If a CPO is applied to a data set
#' (using \code{\link{\%>>\%}} or \code{\link{applyCPO}}, or indirectly when a \code{\link{CPOLearner}} is trained)
#' that has a property not listed in \code{$properties}, an error will be given.
#'
#' \code{$properties.adding} can be one or many of the same values as \code{$properties}. These properties
#' get added to a \code{\link[mlr:makeLearner]{Learner}} or CPO coming after / behind this CPO. When a CPO imputes missing values, for example,
#' this is \dQuote{missings}. This is always a subset of \code{$properties}.
#'
#' \code{$properties.needed} can be one or many of the same values as \code{$properties}. These properties
#' are required from a Learner (or CPO) coming after / behind this CPO. E.g., when a CPO converts factors to
#' numerics, this is \dQuote{numerics} (and \code{$properties.adding} would be \dQuote{factors} in this case).
#' \code{$properties.adding} and \code{$properties.needed} never have any value in common.
#'
#' @section Possible properties:
#' \describe{
#'   \item{data properties}{\dQuote{numerics}, \dQuote{factors}, \dQuote{ordered}, \dQuote{missings}:
#'     Whether any data column contains the type in question, or has missings.}
#'   \item{task type properties}{\dQuote{cluster} \dQuote{classif} \dQuote{multilabel} \dQuote{regr} \dQuote{surv}:
#'     The type of the task. \code{\link[base]{data.frame}} data objects have the implicit property \dQuote{cluster}.}
#'   \item{target properties}{\dQuote{oneclass} \dQuote{twoclass} \dQuote{multiclass}:
#'     Whether the target column of a \code{classif} task has one, two, or more classes.}
#' }
#'
#' @template arg_cpo
#' @param only.data [\code{logical(1)}]\cr
#'   Only get the CPO \emph{data properties} (not target or task type properties). Default is \code{FALSE}.
#' @return [\code{list}]. A \code{list} with slots \code{$properties}, \code{$properties.adding}, and \code{$properties.needed}.
#'
#' @aliases CPOProperties
#' @family getters and setters
#' @export
getCPOProperties = function(cpo, only.data = FALSE) {
  assertFlag(only.data)
  UseMethod("getCPOProperties")
}

#' @title Get the Selection Arguments for Affected CPOs.
#'
#' @description
#' Get the \code{affect.*} arguments from when the \code{\link{CPO}} was constructed. These
#' are in one-to-one correspondence to the \code{affect.*} parameters given to the \code{\link{CPOConstructor}},
#' see the parameter documentation there.
#'
#' @template arg_cpo
#' @param drop.defaults [\code{logical(1)}]\cr
#'   Whether to only return the arguments that deviate from the default.
#'   Default is \code{TRUE}.
#' @return [\code{list}]. A named \code{list} of the \code{affect.*} arguments given to the \code{\link{CPOConstructor}}.
#'   The names are stripped of the \dQuote{affect.}-prefix.
#'
#' @family getters and setters
#' @export
getCPOAffect = function(cpo, drop.defaults = TRUE) {
  UseMethod("getCPOAffect")
}

#' @title Get the CPO Object Type.
#'
#' @description
#' Get the type / functionality provided by the given \code{\link{CPO}} object.
#' There is a fundamental distinction between \code{\link{CPO}} objects
#' and \code{\link{CPOTrained}} objects, the latter of which can provide either
#' retrafo or inverter functionality, or both. \code{CPOTrained} are subclassed into
#' \code{\link{CPOInverter}} (only inverter functionality), or
#' \code{\link{CPORetrafo}} (retrafo, possibly also inverter). To get more information
#' about a \code{\link{CPORetrafo}} object's capabilities, use \code{\link{getCPOInvertCapability}}.
#'
#'
#' @param cpo [\code{\link{CPO}} | \code{\link{CPOTrained}}]\cr
#'   The CPO.
#' @return [\code{character(1)}]. \dQuote{CPO} if the given object is a \code{\link{CPO}},
#'   \dQuote{CPOInverter} if the object is a \code{\link{CPOInverter}} only,
#'   \dQuote{CPORetrafo} if the object is a \code{\link{CPORetrafo}} object (which may have inverter capabilities, see
#'   \code{link{getCPOInvertCapability}}),
#'   \dQuote{NULLCPO} if the object is \code{\link{NULLCPO}}.
#' @family getters and setters
#' @family retrafo related
#' @family inverter related
#' @family CPO classifications
#' @export
getCPOObjectType = function(cpo) {
  UseMethod("getCPOObjectType")
}

#' @title Get the CPO's Inverter Capability.
#'
#' @description
#' Both \code{\link{CPORetrafo}} and \code{\link{CPOInverter}} objects could be used for inversion using
#' \code{\link{invert}} in principle. However, some \code{\link{CPORetrafo}}
#' objects forbid inversion (and one must use the \code{\link{CPOInverter}} object instead),
#' and some \code{\link{CPORetrafo}} objects are NO-OPS when called with \code{\link{invert}}.
#'
#' @section Inverter capability:
#' The invert capability of a \code{\link{CPOTrained}} depends on the \code{\link{CPO}} which was used to
#' create it. Whenever a \code{\link{CPO}} is applied to some data, the result has the \code{link{retrafo}}
#' and \code{\link{inverter}} attributes set that can be retrieved using the respectively named functions to
#' get the \code{\link{CPORetrafo}} and \code{\link{CPOInverter}} object.
#'
#' Every \code{\link{CPO}} can be a
#' \dQuote{Feature Operation} CPO, a \dQuote{Target Operation} CPO, or a \dQuote{Retrafoless} CPO, or a composition
#' of these (see \link{OperatingTypes}).
#'
#' If a (possibly compound) CPO contains only Feature Operation CPOs and Retrafoless CPOs, then it does not perform any operation
#' on the target column of a data set; hence there is no inversion to be performed, the resulting \code{\link{CPORetrafo}}
#' is a NO-OP when used with \code{\link{invert}} (and the \code{\link{inverter}} attribute created is in fact a
#' \code{\link{NULLCPO}}).
#'
#' If a (possibly compound) CPO also contains Target Operation CPOs, but they are data independent--e.g. a CPO that
#' takes the logarithm of the target column in a regression task--then the \code{\link{CPOretrafo}} object has enough information
#' to perform inversion and hence can also meaningfully be used with \code{\link{invert}}. In this case the invert capability
#' of the \code{\link{CPORetrafo}} will be \dQuote{hybrid}. The \code{\link{CPOInverter}}
#' object retrieved using the \code{\link{inverter}} function can be used for the same task, but the benefit of the
#' \code{\link{CPORetrafo}} object is that it can be used for all prediction data applied to it, while the
#' \code{\link{CPOInverter}} object needs to be retrieved for each prediction data set anew.
#'
#' If a (possibly compound) CPO contains Target Operation CPOs that are not data independent then the resulting
#' \code{\link{CPORetrafo}} has invert capability \dQuote{retrafo.only}, since the inversion requires information about
#' the particular data set that was transformed.
#'
#' A \code{\link{CPOInverter}} object \emph{always} has the \dQuote{inverter} invert capability, since it can always be used
#' for \code{\link{invert}} and never used in the place of a \code{\link{CPORetrafo}}.
#'
#' @param cpo [\code{\link{CPOTrained}}]\cr
#'   The \code{\link{CPOTrained}} object to query.
#' @return [\code{character(1)}]. \dQuote{inverter} if given object is an inverter only,
#'   \dQuote{hybrid} if given object is retrafo and inverter,
#'   \dQuote{retrafo.only} if given object is retrafo only,
#'   \dQuote{retrafo} if given object is a retrafo that is a NO-OP if used with \code{\link{invert}}.
#' @family getters and setters
#' @family retrafo related
#' @family inverter related
#' @family CPO classifications
#' @aliases InvertCapability
#' @export
getCPOInvertCapability = function(cpo) {
  UseMethod("getCPOInvertCapability")
}

#' @title Get the CPO \code{predict.type}.
#'
#' @description
#' Get the possible predict.types a \code{\link{CPO}} is able to handle.
#'
#' The concept of a \code{predict.type} originates from \code{\link[mlr]{predict.WrappedModel}}, which
#' allows the estimation of different aspects of a prediction. This is, currently:
#' \describe{
#'   \item{\dQuote{response}}{A best estimate of the actual target value}
#'   \item{\dQuote{prob}}{An estimate of probabilities of different target values}
#'   \item{\dQuote{se}}{An estimate of the target value, together with an estimate of the standard error of this first estimation}
#' }
#'
#' A Target Operation CPO is able to change the type of a \code{\link[mlr]{Task}}, but it can also enhance the type of predictions
#' that a \code{\link[mlr:makeLearner]{Learner}} can make for it. Thus a CPO that converts a binary classification into a regression task can
#' use a regression learner to not only predict the \dQuote{response} class, but also the estimated probability (\dQuote{prob})
#' distribution over the two classes. For this, the CPO declares
#' \enumerate{
#'   \item what \code{predict.type}s a \code{\link[mlr:makeLearner]{Learner}}, when attached to it, can provide, and
#'   \item what \code{predict.type} the \code{\link[mlr:makeLearner]{Learner}}, in each case, must be capable of.
#' }
#' This information is provided in the form of a named \code{character}, where the names are the provided predict type capabilities,
#' and the values are the predict type that the underlying \code{\link[mlr:makeLearner]{Learner}} must provide for this.
#'
#' The CPO converting classification to regression mentioned above would thus have the \code{predict.type} of:
#'
#' \code{c(response = "response", prob = "response")}
#'
#' Another example would be a CPO that converts a multiclass classification problem into an ordinary classification problem, but
#' uses the \dQuote{prob} prediction of the underlying learner to make both the \dQuote{response} and \dQuote{prob} predictions.
#' It would have the \code{predict.type} of:
#'
#' \code{c(response = "prob", prob = "prob")}
#'
#' If this second CPO is attached to a \code{\link[mlr:makeLearner]{Learner}} that does not have the \dQuote{prob} property (see
#' \code{\link[mlr]{LearnerProperties}}), an error is given.
#'
#' CPOs that are not Target Operating always have the \code{predict.type} of:
#'
#' \code{c(response = "response", prob = "prob", se = "se")}
#'
#' @template arg_cpo
#' @return [\code{character}]. A named \code{character} that maps potential predict types that a CPO may provide to the required
#' predict type of an underlying learner.
#' @family getters and setters
#' @family CPO classifications
#' @aliases PredictType
#' @export
getCPOPredictType = function(cpo) {
  UseMethod("getCPOPredictType")
}

#' @title Determine the Operating Type of the CPO
#'
#' @description
#' Gives the \emph{operating type} of a CPO or Retrafo, i.e. the part of a given data set it operates on.
#' This can be \dQuote{target} for a CPO / Retrafo / Inverter that
#' manipulates target columns, \dQuote{feature} for
#' a CPO / Retrafo that manipulates non-target columns,
#' or \dQuote{retrafoless} for a CPO that only handles training data
#' (and hence can manipulate both feature and target columns, but produces no retrafo).
#'
#' For a composite CPO / Retrafo of different operating types, all
#' types are returned. \code{NULLCPO} has no operating type.
#'
#' @section Operating types:
#' There are three types of \code{\link{CPO}} that differ in their effects on the data: \dQuote{\emph{Feature Operation}},
#' \dQuote{\emph{Target Operation}}, and \dQuote{\emph{Retrafoless}}.
#'
#' Feature Operation CPOs (\bold{FOCPO}) only change the feature columns
#' of a data set, and don't change the target column(s). They therefore cannot change the type of a \code{\link[mlr]{Task}}, and
#' will never change the number of rows of a data set. They are the easiest CPO to handle, as they do not require
#' inversion of predictions made with processed data. Examples of Feature Operation CPOs is the scaling of individual features
#' to have unit variance (\code{\link{cpoScale}}), or the projection on principal components (\code{\link{cpoPca}}).
#'
#' Target Operation CPOs (\bold{TOCPO}) only change the target column(s) of a data set, not the feature columns. They can thus
#' also change the type
#' of a \code{\link[mlr]{Task}}, and the \link{PredictType}s admitted by a \code{\link[mlr:makeLearner]{Learner}}. They are thus a powerful
#' instrument, but they are harder to handle, since predictions made with data sets processed with this kind of CPO need to be
#' \emph{inverted} using the \code{\link{invert}} function and possibly an \code{\link{CPOInverter}} object (see documentation there).
#' (Note that attaching a Target Operation CPO to a \code{\link[mlr:makeLearner]{Learner}} will hide this complexity from the user and is the
#' recommended way of handling it.)
#' Examples of Target Operation CPOs are the log-transformation of the target column of a regression task, the conversion of a
#' binary classification task into a 0-1-regression task, or the substitution of the target values into the residuals after a
#' \code{\link[mlr:makeLearner]{Learner}} was applied to the task. Note that the last of these examples distinguishes itself by the fact that
#' the inversion operation is dependent on the \emph{prediction} data used. While for the first two examples, the
#' \code{\link{CPORetrafo}} object can be used for inversionk, the last one requires the \code{\link{CPOInverter}} object. See
#' \code{\link{InvertCapability}} for more on this.
#'
#' Retrafoless CPOs (\bold{ROCPO}) can change the feature \emph{and} target columns of a task, but this comes at the cost of not
#' allowing retransformations. When getting the
#' \code{\link{CPORetrafo}} object using \code{\link{retrafo}}, one will always get an identity transformation.
#' While other CPOs can be understood as \emph{transforming} the space of features or target values,
#' respectively, the Retrafoless CPO can only \emph{add} or \emph{subtract} points in the given space. Examples of this operation
#' are subsampling and supersampling.
#'
#'
#' @param cpo [\code{CPO} | \code{CPOTrained}]\cr
#'   The CPO, Retrafo, or Inverter to inspect.
#' @return [\code{character(1)}]. Zero or more of \dQuote{target}, \dQuote{feature}, \dQuote{retrafoless}.
#' @family getters and setters
#' @family retrafo related
#' @family inverter related
#' @family CPO classifications
#' @aliases OperatingType
#' @export
getCPOOperatingType = function(cpo) {
  UseMethod("getCPOOperatingType")
}

# Param Sets and related

#' @export
getParamSet.CPO = function(x) {
  x$par.set
}

#' @export
getParamSet.CPOTrainedPrimitive = function(x) {
  c(x$cpo$bare.par.set, x$cpo$unexported.par.set)
}

#' @export
getParamSet.CPOTrained = function(x) {
  stop("Cannot get param set of compound retrafo. Use as.list to get individual elements")
}

#' @export
getHyperPars.CPO = function(learner, for.fun = c("train", "predict", "both")) {
  learner$par.vals
}

#' @export
getHyperPars.CPOTrainedPrimitive = function(learner, for.fun = c("train", "predict", "both")) {
  getBareHyperPars(learner$cpo)
}

#' @export
getHyperPars.CPOTrained = function(learner, for.fun = c("train", "predict", "both")) {
  stop("Cannot get parameters of compound retrafo. Use as.list to get individual elements")
}

#' @export
setHyperPars2.CPO = function(learner, par.vals = list()) {
  badpars = setdiff(names(par.vals), names(learner$par.set$pars))
  if (length(badpars)) {
    stopf("CPO %s does not have parameter%s %s", getLearnerName(learner),
          ifelse(length(badpars) > 1, "s", ""), collapse(badpars, ", "))
  }
  checkParamsFeasible(learner$par.set, par.vals)
  learner$par.vals = insert(learner$par.vals, par.vals)
  learner
}

#' @export
setHyperPars2.CPOTrained = function(learner, par.vals = list()) {
  stopf("Cannot change parameter values of retrafo / inverter object\n%s\n%s\n",
    "To create a retrafo / inverter with a specific state use makeRetrafoFromState.",
    "Get the state of an existing retrafo / inverter using getRetrafoState.")
}

# Properties

#' @export
getCPOProperties.CPO = function(cpo, only.data = FALSE) {
  if (only.data) {
    lapply(cpo$properties, intersect, y = cpo.dataproperties)
  } else {
    cpo$properties
  }
}

#' @family retrafo related
#' @family inverter related
#' @rdname getCPOProperties
#' @export
getCPOProperties.CPOTrained = function(cpo, only.data = FALSE) {
  if (!is.null(cpo$prev.retrafo)) {
    props = compositeProperties(getCPOProperties(cpo$prev.retrafo), cpo$cpo$properties, "[PREVIOUS RETRAFO CHAIN]", cpo$cpo$name)
  } else {
    props = cpo$cpo$properties
  }
  if (only.data) {
    lapply(props, intersect, y = cpo.dataproperties)
  } else {
    props
  }
}

# CPO ID, NAME

#' @export
getCPOName.CPO = function(cpo) {
  cpo$name
}

#' @family retrafo related
#' @family inverter related
#' @rdname getCPOName
#' @export
getCPOName.CPOTrainedPrimitive = function(cpo) {
  cpo$cpo$name
}

#' @export
getCPOName.CPOTrained = function(cpo) {
  paste(getCPOName(cpo$prev.retrafo), cpo$cpo$name, sep = ".")
}

#' @family CPOConstructor
#' @rdname getCPOName
#' @export
getCPOName.CPOConstructor = function(cpo) {
  environment(cpo)$.cpo.name
}

#' @export
getCPOId.CPOPrimitive = function(cpo) {
  cpo$id
}

#' @export
getCPOId.CPO = function(cpo) {
  stop("Compound CPOs have no IDs.")
}

#' @export
setCPOId.CPO = function(cpo, id) {
  stop("Cannot set ID of compound CPO.")
}

# When changing the ID, we need to change each parameter's name, which
# should have the form <ID>.<bare.par.name>
# This means we need to modify $par.set AND $par.vals
#' @export
setCPOId.CPOPrimitive = function(cpo, id) {

  cpo$id = id
  cpo$debug.name = if (is.null(id) || id == cpo$name) cpo$name else sprintf("%s<%s>", cpo$id, cpo$name)
  cpo$par.vals = getBareHyperPars(cpo, FALSE)
  cpo$par.set = cpo$bare.par.set
  pars = cpo$par.set$pars
  if (!is.null(id) && length(pars)) {
    trans = setNames(paste(id, names(pars), sep = "."), names(pars))
    names(pars) = trans
    pars = lapply(pars, function(x) {
      x$id = trans[x$id]
      if (!is.null(x$requires)) {
        x$requires = renameNonfunctionNames(x$requires, trans)
      }
      x
    })
    cpo$par.set$pars = pars
    names(cpo$par.vals) = trans[names(cpo$par.vals)]
  }
  cpo
}

# CPO Type

#' @export
getCPOObjectType.CPO = function(cpo) {
  "CPO"
}

#' @export
getCPOObjectType.CPORetrafo = function(cpo) {
  "CPORetrafo"
}

#' @export
getCPOObjectType.CPOInverter = function(cpo) {
  "CPOInverter"
}

#' @export
getCPOInvertCapability.CPOInverter = function(cpo) {
  "inverter"
}

#' @export
getCPOInvertCapability.CPORetrafo = function(cpo) {
  "retrafo"
}

#' @export
getCPOInvertCapability.CPORetrafoOnly = function(cpo) {
  "retrafo.only"
}

#' @export
getCPOInvertCapability.CPORetrafoHybrid = function(cpo) {
  "hybrid"
}

# Operating Type

#' @export
getCPOOperatingType.CPO = function(cpo) {
  cpo$operating.type
}

#' @export
getCPOOperatingType.CPOTrained = function(cpo) {
  res = switch(getCPOInvertCapability(cpo),
    inverter = "target",
    retrafo = "feature",
    retrafo.only = "feature",
    hybrid = return(c("target", "feature")))  # if it is both 'target' and 'feature', we're done: no recursion
  unique(res, getCPOOperatingType(nullToNullcpo(cpo$prev.retrafo)))
}

# Predict Type

#' @export
getCPOPredictType.CPO = function(cpo) {
  cpo$predict.type
}

#' @family retrafo related
#' @family inverter related
#' @rdname getCPOPredictType
#' @export
getCPOPredictType.CPOTrained = function(cpo) {
  cpo$cpo$predict.type
}


# Normalize "affect.*" arguments of CPOs

#' @export
getCPOAffect.CPOPrimitive = function(cpo, drop.defaults = TRUE) {
  affect.args = cpo$affect.args
  if (!drop.defaults) {
    if (!length(getCPOAffect(cpo))) {
      affect.args$type = c("numeric", "factor", "ordered", "other")
    }
    return(affect.args)
  }
  if (setequal(affect.args$type, c("numeric", "factor", "ordered", "other"))) {
    affect.args$type = NULL
  }
  if (!length(affect.args$index)) {
    affect.args$index = NULL
  }
  if (!length(affect.args$names)) {
    affect.args$names = NULL
  }
  if (is.null(affect.args$pattern)) {
    affect.args$pattern.ignore.case = NULL
    affect.args$pattern.perl = NULL
    affect.args$pattern.fixed = NULL
  }
  Filter(function(x) !is.null(x) && !identical(x, FALSE), affect.args)
}

#' @export
getCPOAffect.CPO = function(cpo, drop.defaults = TRUE) {
  stop("Compound CPOs have no affect arguments.")
}

