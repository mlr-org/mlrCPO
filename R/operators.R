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
    cpo1
  }
  UseMethod("composeCPO")
}

#' @title Attach a CPO to a Learner.
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

#' @title Apply a CPO to Data.
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
#' An applied \code{\link{CPO}} can change the content of feature columns, target columns of Tasks,
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
  newprops = compositeProperties(cpo1$properties, cpo2$properties, cpo1$debug.name, cpo2$debug.name)
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
    # --- CPOPipeline part
    first = cpo1,
    second = cpo2)
}

##################################
### CPO Trafo Splitting        ###
##################################

#' @title Split a Pipeline Into Its Constituents.
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
# Check types, check properties.
# Since "Retrafo" and "Inverter" are fundamentally the
# same class, some special cases need to be handled.
#' @export
composeCPO.CPOTrained = function(cpo1, cpo2) {
  assertClass(cpo2, "CPOTrained")
  if (is.nullcpo(cpo2)) {
    return(cpo1)
  }
  is.prim = "CPOTrainedPrimitive" %in% class(cpo2)
  assert(is.prim == is.null(cpo2$prev.retrafo))

  objtype = c(getCPOObjectType(cpo1), getCPOObjectType(cpo2))
  invcap = c(getCPOInvertCapability(cpo1), getCPOInvertCapability(cpo2))

  # are we composing the retrafo part?
  retrafo.composition = all(objtype == "CPORetrafo")  # invcap are retrafo, retrafo.only, or hybrid
  # is the inverter part a noop composition?
  inverter.noop.composition = all(invcap == "retrafo")
  # do we need to compose inverter at all?
  inverter.composition = !inverter.noop.composition && all(invcap != "retrafo.only")
  # are both involved CPOs actual inverters?
  pure.inverter.composition = all(objtype == "CPOInverter")

  if (!retrafo.composition && !inverter.composition) {
    # don't need to check inverter.noop.composition, since that is only true if retrafo.composition.
    stopf("Cannot compose retrafos with capability %s with retrafos with capability %s.",
      getCPOInvertCapability(cpo1), getCPOInvertCapability(cpo2))
  }
  if (!is.prim) {
    cpo1 = composeCPO(cpo1, cpo2$prev.retrafo)
  }

  # check for properties match
  if (retrafo.composition) {
    cpo2$needed = compositeProperties(
        list(handling = character(0), adding = character(0), needed = cpo1$needed),
        cpo2$cpo$properties, getCPOName(cpo1), cpo2$cpo$name)$needed
  }
  if (inverter.composition) {
    if (pure.inverter.composition) {
      # pure inverter chaining: do myopic property checking
      assertString(cpo1$cpo$convertto)
      assertString(cpo2$cpo$convertfrom)
      if (cpo1$convertto != cpo2$convertfrom) {
        stopf("Incompatible chaining of inverters: %s converts to %s, but %s needs %s.",
          cpo1$cpo$name, cpo1$cpo$convertto, cpo2$cpo$name, cpo2$cpo$convertfrom)
      }
      compositeProperties(cpo1$cpo$properties, cpo2$cpo$properties, getCPOName(cpo1), cpo2$cpo$name)  # just for checking
    }
    assert(length(cpo1$predict.type) <= 2,  # at least one predict.type cannot be the identity (since otherwise its a noop-composition)
      length(cpo2$predict.type) <= 2)       # the identity (and only the identity) has more than 2 elements.
  }

  cpo2$predict.type = chainPredictType(cpo1$predict.type, cpo2$cpo$predict.type, getCPOName(cpo1), cpo2$cpo$name)
  cpo2$prev.retrafo = cpo1

  if (retrafo.composition) {
    # any chance of the result not just being an inverter?
    if (inverter.noop.composition) {
      # two CPORetrafos --> CPORetrafo
      newclass = "CPORetrafo"
    } else if (inverter.composition) {
      # at least one non-pure CPORetrafo, but the CPORetrafo allows inversion
      newclass = c("CPORetrafoHybrid", "CPORetrafo")
    } else {
      # at least one retrafo.only
      newclass = c("CPORetrafoOnly", "CPORetrafo")
    }
  } else {
    newclass = "CPOInverter"
  }

  class(cpo2) = setdiff(class(cpo2), c("CPOTrainedPrimitive", "CPORetrafoHybrid", "CPORetrafoOnly", "CPORetrafo", "CPOInverter"))

  addClasses(cpo2, newclass)
}

# chain CPOs with predict.type pt1 %>>% pt2
# the 'predict.type' slot is a map (i.e. a named vector of character that maps a -> predict.type[a])
# when chaining CPOs, the predict.types need to be chained as well.
# @param pt1 predict.type of first CPO
# @param pt2 predict.type of second CPO
# @param name1 debug name of first CPO
# @param name2 debug name of second CPO
chainPredictType = function(pt1, pt2, name1, name2) {
  result = sapply(pt1, function(x) unname(pt2[x]))
  result = result[!is.na(result)]
  if (isPropertyStrict() && !length(result)) {
    # So this is a bit of a weird situation: The CPO chain would work for trafo AND retrafo, but not for predictions.
    stopf("Incompatible chaining of inverters: %s needs a predict.type being%s '%s', but %s can only deliver type%s '%s'.",
      name1, ifelse(length(pt1) == 1, " one of", ""), collapse(pt1, sep = "', '"),
      name2, ifelse(length(pt2) > 1, "s", ""), collapse(pt2, sep = "', '"))
  }
  result
}

##################################
### Retrafo Splitting          ###
##################################

# RETRAFO splitting
# retrafos are a linked list, so on a basic level what happens is
# c(as.list(x$prev.retrafo), list({x$prev.retrafo = NULL ; x}))
# However, some other things that happen in composeCPO.CPOTrained need to be undone.
#' @rdname as.list.CPO
#' @export
as.list.CPOTrained = function(x, ...) {
  assert(length(list(...)) == 0)
  prev = if (!is.null(x$prev.retrafo)) as.list(x$prev.retrafo)
  x$prev.retrafo = NULL
  class(x) = setdiff(class(x), c("CPOTrainedPrimitive", "CPORetrafoHybrid", "CPORetrafoOnly", "CPORetrafo", "CPOInverter"))
  if ("properties.needed" %in% names(x)) {
    # was a CPORetrafo before
    x$properties.needed = x$cpo$properties$properties.needed
    x = addClasses(x, getCPORetrafoSubclasses(x$cpo))
  } else {
    x = addClasses(x, "CPOInverter")
  }
  x$predict.type = x$cpo$predict.type
  c(prev, list(addClasses(x, "CPOTrainedPrimitive")))
}

##################################
### Chaining                   ###
##################################

#' @title Turn a \code{list} of CPOs Into a Single Chained One.
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
