#' @include callCPO.R

##################################
### Generics                   ###
##################################


#' @title CPO Composition
#'
#' @description
#' Composes \code{\link{CPO}} or \code{\link{CPOTrained}} objects. The \code{\link{\%>>\%}} operator can be used
#' synonymously to compose CPO objects.
#'
#' Composition of operators is one of the main features they provide: this makes it possible for
#' complex operations to be represented by single objects. Compound operators represent the operation
#' of applying both its constituent operations in succession. Compound operators can themselves be
#' composed to form arbitrarily long chains of operators.
#'
#' Compound objects behave, in most ways, like primitive objects. Some exceptions are:
#' \itemize{
#'   \item Compound CPOs do not have an ID, so \code{\link{getCPOId}} and \code{\link{setCPOId}} will not work on them.
#'   \item Compound CPOs have no 'affect' property, so \code{\link{getCPOAffect}} will not work.
#' }
#'
#' While \code{\link{CPOTrained}} operators can be composed just as \code{\link{CPO}} operators, this
#' is only recommended in cases where the same primitive CPOTrained objects where retrieved using
#' \code{\link{as.list.CPOTrained}}. This is because CPOTrained are closely related to the data
#' that was used to create it, and therefore on their original position in the CPO pipeline during
#' training.
#'
#' @param cpo1 [\code{\link{CPO}} | \code{\link{CPOTrained}}]\cr
#'   The operation to perform first.
#' @param cpo2 [\code{\link{CPO}} | \code{\link{CPOTrained}}]\cr
#'   The operation to perform second, must have the same class as \code{cpo1}.
#' @return [\code{\link{CPO}} | \code{\link{CPOTrained}}]. The operation representing the application
#'   of \code{cpo1} followed by the application of \code{cpo2}.
#' @family operators
#' @family CPO lifecycle related
#' @export
composeCPO = function(cpo1, cpo2) {
  assert(checkClass(cpo2, "CPO"),
    checkClass(cpo2, "CPOTrained"))
  if (is.nullcpo(cpo2)) {
    return(cpo1)
  }
  UseMethod("composeCPO")
}

#' @title Attach a CPO to a Learner
#'
#' @description
#' A \code{\link{CPO}} object can be attached to a \code{\link[mlr:makeLearner]{Learner}} object to create a
#' pipeline combining preprocessing and model fitting. When the resulting \code{\link{CPOLearner}}
#' is used to create a model using \code{\link[mlr]{train}}, the attached CPO will be applied to the
#' data before the internal model is trained. The resulting model will also contain the required
#' \code{\link{CPOTrained}} elements, and apply the necessary \code{\link{CPORetrafo}} objects to new prediction
#' data, and the \code{\link{CPOInverter}} objects to predictions made by the internal model.
#'
#' The \code{\link{\%>>\%}} operator can be used synonymously to attach CPO objects to Learners.
#'
#' @template arg_cpo
#' @param learner [\code{\link[mlr:makeLearner]{Learner}}]\cr
#'   The learner.
#'
#' @family operators
#' @family CPO lifecycle related
#' @family CPOLearner related
#' @export
attachCPO = function(cpo, learner) {
  checkLearner(learner)
  UseMethod("attachCPO")
}

#' @title Apply a CPO to Data
#'
#' @description
#' The given transformation will be applied to the data in the given \code{\link[mlr]{Task}} or \code{\link[base]{data.frame}}.
#'
#' If the input data is a \code{data.frame}, the returned object will in most cases also be a \code{data.frame}, with exceptions
#' if the applied \code{\link{CPO}} performs a conversion to a \code{\link[mlr]{Task}}. If the input data is a Task, its type
#' will only be changed to a different type of Task if the applied CPO performs such a conversion.
#'
#' The \code{\link{\%>>\%}} operator can be used synonymously to apply CPO objects to data. In case of \code{\link{CPORetrafo}},
#' \code{\link[stats]{predict}} can be used synonymously.
#'
#' @section Application of \code{CPO}:
#' Application of a \code{\link{CPO}} is supposed to perform \emph{preprocessing} on a given data set, to prepare it e.g. for model
#' fitting with a \code{\link[mlr:makeLearner]{Learner}}, or for other data handling tasks. When this preprocessing is performed, care is taken
#' to make the transformation repeatable on later prediction or validation data. For this,
#' the returned data set will have a \code{\link{CPORetrafo}} and
#' \code{\link{CPOInverter}} object attached to it, which can be retrieved using \code{\link{retrafo}} and \code{\link{inverter}}.
#' These can be used to perform the same transformation on new data, or to invert a prediction made with the transformed data.
#'
#' An applied \code{\link{CPO}} can change the content of feature columns, target columns of Tasks,
#' and may even change the number of rows of a given data set.
#'
#' @section Application of \code{CPORetrafo}:
#' Application of a \code{\link{CPORetrafo}} is supposed to perform a transformation that mirrors the transformation done before
#' on a training data set. It should be used when trying to make predictions from new data, using a model that was trained with
#' data preprocessed using a \code{\link{CPO}}. The predictions made may then need to be inverted. For this,
#' the returned data set will have a \code{\link{CPOInverter}} object attached to it,
#' which can be retrieved using \code{\link{inverter}}.
#'
#' An applied \code{\link{CPORetrafo}} may change the content of feature columns and target columns of Tasks, but will never
#' change the number or order of rows of a given data set.
#'
#' @param cpo [\code{\link{CPO}} | \code{\link{CPORetrafo}}]\cr
#'   The CPO or CPORetrafo representing the operation to perform.
#' @param task [\code{\link[mlr]{Task}} | \code{\link[base]{data.frame}}]\cr
#'   The data to operate on.
#' @return [\code{\link[mlr]{Task}} | \code{\link[base]{data.frame}}]. The transformed data, augmented with a \code{\link{inverter}}
#' and possibly a \code{\link{retrafo}} tag.
#' @export
#' @family operators
#' @family retrafo related
#' @family inverter related
applyCPO = function(cpo, task) {
  assert(checkClass(task, "Task"),
    checkClass(task, "data.frame"))
  UseMethod("applyCPO")
}


##################################
### CPO Trafo Composition      ###
##################################
# (as opposed to retrafo, inverter ops)

# CPO %>>% CPO
# Just creates a 'CPOPipeline' object
#' @export
composeCPO.CPO = function(cpo1, cpo2) {
  assertClass(cpo2, "CPO")
  parameterClashAssert(cpo1, cpo2, cpo1$debug.name, cpo2$debug.name)
  newprops = composeProperties(cpo1$properties, cpo2$properties, cpo1$debug.name, cpo2$debug.name)
  newpt = chainPredictType(cpo1$predict.type, cpo2$predict.type, cpo1$debug.name, cpo2$debug.name)

  makeS3Obj(c("CPOPipeline", "CPO"),
    # --- CPO Part
    name = paste(cpo2$name, cpo1$name, sep = "."),
    debug.name = paste(cpo1$debug.name, cpo2$debug.name, sep = " >> "),
    par.set = c(cpo1$par.set, cpo2$par.set),
    par.vals = c(cpo1$par.vals, cpo2$par.vals),
    properties = newprops,
    operating.type = unique(c(cpo1$operating.type, cpo2$operating.type)),
    predict.type = newpt,
    convertfrom = firstNonNull(cpo1$convertfrom, cpo2$convertfrom),
    convertto = firstNonNull(cpo2$convertto, cpo1$convertto),
    constant.invert = cpo1$constant.invert && cpo2$constant.invert,

    # --- CPOPipeline part
    first = cpo1,
    second = cpo2)
}

##################################
### CPO Trafo Splitting        ###
##################################

#' @title Split a Pipeline into Its Constituents
#'
#' @description
#' Split a compound \code{\link{CPO}} or \code{\link{CPOTrained}} into a list of its constituent parts.
#'
#' This is useful for inspection of pipelines, or for possible rearrangements or changes of pipelines. The
#' resulting \code{list} can be changed and rebuilt using \code{\link{pipeCPO}}.
#'
#' @param x [\code{\link{CPO}} | \code{\link{CPOTrained}}]\cr
#'   The \code{\link{CPO}} or \code{\link{CPOTrained}} chain to split apart.
#' @param ... [\code{any}]\cr
#'   Ignored.
#' @return [\code{list} of \code{\link{CPO}} | \code{list} of \code{\link{CPOTrained}}]. The primitive constituents of \code{x}.
#'
#' @name as.list.CPO
#' @family operators
#' @family retrafo related
#' @family inverter related
#' @export
# Splitting a primitive object gives a list of that object
as.list.CPOPrimitive = function(x, ...) {
  assert(length(list(...)) == 0)
  list(x)
}

# Compound objects are a binary tree, so
# splitting a compound object recursively calls as.list to both children
# and then concatenates.
#' @export
as.list.CPOPipeline = function(x, ...) {
  first = x$first
  second = x$second
  first$par.vals = subsetParams(x$par.vals, first$par.set)
  second$par.vals = subsetParams(x$par.vals, second$par.set)
  c(as.list(first), as.list(second))
}


##################################
### Retrafo Composition        ###
##################################

# RETRAFO %>>% RETRAFO
# or INVERTER %>>% INVERTER
# Check types, check properties.
# "Retrafo" and "Inverter" are fundamentally the
# same class, but some special cases need to be handled.
#' @export
composeCPO.CPOTrained = function(cpo1, cpo2) {
  assertClass(cpo2, "CPOTrained")
  if (is.nullcpo(cpo2)) {
    return(cpo1)
  }
  is.prim = "CPOTrainedPrimitive" %in% class(cpo2)
  assert(is.prim == is.null(cpo2$element$prev.retrafo.elt))


  # update prev.retrafo.elt
  recursive.compose = function(c1, c2) {
    if (!is.null(c2$prev.retrafo.elt)) {
      c1 = recursive.compose(c1, c2$prev.retrafo.elt)
    }
    c2$prev.retrafo.elt = c1
    c2$prev.predict.type = chainPredictType(c1$prev.predict.type, c1$cpo$predict.type)
    c2
  }
  cpo1$element = recursive.compose(cpo1$element, cpo2$element)

  ##
  # update cached values


  # name
  cpo1$name = paste(cpo1$name, cpo2$name, sep = ".")

  # check that capabilities don't mismatch
  if (all(pmin(cpo1$capability, cpo2$capability) < 0)) {
    stopf("Cannot compose CPOTrained with capability %s with CPOTrained with capability %s, since the resulting object can neither do retrafo nor invert.",
      namedVecToString(cpo1$capability), namedVecToString(cpo2$capability))
  }

  # capability
  cpo1$capability = (pmax(cpo1$capability %% 3L, cpo2$capability %% 3L) + 1L) %% 3L - 1L
  assertInteger(cpo1$capability, lower = -1, upper = 1, len = 2, any.missing = FALSE)
  assert(identical(names(cpo1$capability), c("retrafo", "invert")))
  assert(any(cpo1$capability > 0))

  # predict.type
  cpo1$predict.type = chainPredictType(cpo1$predict.type, cpo2$predict.type, cpo1$name, cpo2$name)

  # properties
  cpo1$properties = composeProperties(cpo1$properties, cpo2$properties, cpo1$name, cpo2$name)

  # conversion
  cpo1$convertfrom = firstNonNull(cpo1$convertfrom, cpo2$convertfrom)
  cpo1$convertto = firstNonNull(cpo2$convertto, cpo1$convertto)

  newclass = if (cpo1$capability["retrafo"] < 0) "CPOInverter" else "CPORetrafo"
  class(cpo1) = c(newclass, setdiff(class(cpo1), c("CPOTrainedPrimitive", "CPORetrafo", "CPOInverter")))
  cpo1
}

# chain CPOs with predict.type pt1 %>>% pt2
# the 'predict.type' slot is a map (i.e. a named vector of character that maps a -> predict.type[a])
# when chaining CPOs, the predict.types need to be chained as well.
# @param pt1 predict.type of first CPO
# @param pt2 predict.type of second CPO
# @param name1 debug name of first CPO
# @param name2 debug name of second CPO
chainPredictType = function(pt1, pt2, name1, name2) {
  result = vcapply(pt1, function(x) unname(pt2[x]))
  result = result[!is.na(result)]
  if (isPropertyStrict() && !length(result)) {
    # So this is a bit of a weird situation: The CPO chain would work for trafo AND retrafo, but not for predictions.
    stopf("Incompatible chaining of inverters: %s needs a predict.type being%s '%s', but %s can only deliver type%s '%s'.",
      name1, ifelse(length(pt1) == 1, " one of", ""), collapse(pt1, sep = "', '"),
      name2, ifelse(length(pt2) > 1, "s", ""), collapse(names(pt2), sep = "', '"))
  }
  result
}

##################################
### Retrafo Splitting          ###
##################################

# RETRAFO splitting
# retrafos are a linked list, so on a basic level what happens is
# c(as.list(x [with prev.retrafo.elt]), list({x$element$prev.retrafo.elt = NULL ; x}))
# Besides the "cached" values in the CPOTrained object need to be set.
#' @rdname as.list.CPO
#' @export
as.list.CPOTrained = function(x, ...) {
  assert(length(list(...)) == 0)
  x.with.prev = x
  x.with.prev$element = x$element$prev.retrafo.elt
  prev = if (!is.null(x.with.prev$element)) as.list(x.with.prev)

  x$element$prev.retrafo.elt = NULL
  x$element$prev.predict.type = cpo.identity.predict.type.map
  cpo = x$element$cpo
  x$name = cpo$name
  x$capability = x$element$capability
  x$predict.type = cpo$predict.type
  x$properties = cpo$properties
  x$convertfrom = cpo$convertfrom
  x$convertto = cpo$convertto

  newclass = if (x$capability["retrafo"] < 0) "CPOInverter" else "CPORetrafo"
  class(x) = c("CPOTrainedPrimitive", newclass, setdiff(class(x), c("CPOTrainedPrimitive", "CPORetrafo", "CPOInverter")))

  c(prev, list(x))
}

##################################
### Chaining                   ###
##################################

#' @title Turn a \code{list} of CPOs into a Single Chained One
#'
#' @description
#' Chain a list of preprocessing operators, or retrafo objects, turning \code{list(a, b, c)} into
#' \code{a \%>>\% b \%>>\% c}.
#'
#' This is the inverse of \code{\link{as.list.CPO}} / \code{\link{as.list.CPOTrained}} when applied to \code{\link{CPO}} or \code{\link{CPOTrained}}.
#'
#' @param pplist [\code{list} of \code{CPO} | \code{list} of \code{CPOTrained}]\cr
#'   A list of \code{\link{CPO}} or \code{\link{CPOTrained}} objects.
#' @return [\code{\link{CPO}} | \code{\link{CPOTrained}}]. The compound CPO(Trained) obtained when chaining the elements
#'   of the input list.
#'
#' @family operators
#' @family retrafo related
#' @family inverter related
#' @export
pipeCPO = function(pplist) {
  assert(checkList(pplist, types = "CPO"),
    checkList(pplist, types = "CPOTrained"))
  Reduce(composeCPO, c(list(NULLCPO), pplist))
}
