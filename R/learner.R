# CPOLearner.R concerns itself with attaching CPOs to mlr Learners

# attachCPO does four things:
#  1) Check that properties of CPOs and learners agree
#  2) Check that parameter names don't clash
#  3) possibly create a wrapper around the learner to get a 'CPOLearner' *
#  4) Modify CPO, Parameters, Properties of the learner
# * When attaching to a learner, we could, in principle, create a long chain of
# wrappers around a learner. This makes retrieving the operations, that go
# on in a learner with a long pipeline, relatively complicated.
# Therefore, if the learner is already a CPOLearner, we just change the
# attached CPO to a compound CPO.
# @param cpo [CPO] a cpo to attach
# @param learner [Learner] the learner to attach the CPO to
# @return [CPOLearner] a learner with attached CPO
#' @export
attachCPO.CPO = function(cpo, learner) {
  learner = checkLearner(learner)
  if (!learner$type %in% union(cpo$properties$needed, setdiff(cpo$properties$handling, cpo$properties$adding))) {
    if (is.null(cpo$convertto)) {
      stopf("Cannot attach a CPO to learner of type %s if the CPO can neither handle nor convert to that type.",
        learner$type)
    } else {
      stopf("Cannot combine CPO that outputs type %s with learner of type %s.",
        cpo$convertto, learner$type)
    }
  }

  parameterClashAssert(cpo, learner, cpo$debug.name, getLearnerName(learner))
  if (!"CPOLearner" %in% class(learner)) {
    learner = makeBaseWrapper(learner$id, learner$type, learner,
      learner.subclass = "CPOLearner", model.subclass = "CPOModel")
    learner$predict.type = learner$next.learner$predict.type
  } else {
    cpo = composeCPO(cpo, learner$cpo)
    learner$id = learner$next.learner$id
  }
  learner$cpo = cpo
  learner$properties = compositeCPOLearnerProps(cpo, learner$next.learner)
  learner$type = intersect(learner$properties, cpo.tasktypes)
  assertString(learner$type)
  learner$properties = setdiff(learner$properties, cpo.tasktypes)
  learner$par.vals = cpo$par.vals
  learner$par.set = cpo$par.set
  learner$id = paste(learner$id, cpo$name, sep = ".")

  # possibly need to reset 'predict.type', or just change it to something else.
  prev.predict.type = learner$next.learner$predict.type
  next.predict.type = "response"
  if (prev.predict.type %in% c("response", getLearnerProperties(learner))) {  # if the previous predict.type is still supported
    next.predict.type = prev.predict.type
  }
  setPredictType(learner, next.predict.type)
}

# Get the properties that a learner must have when a CPO with given properties is attached.
# the learner has only one 'properties' slot, the CPO has more than one. Also, the CPOs
# don't concern all properties a learner may have, so absence of a property in a CPO doesn't
# necessarily mean that it needs to be removed from the learner.
# @param cpo [CPO] a cpo to attach
# @param learner [Learner] the learner to attach the CPO to
# @return [character] properties calculated for the new learner when the cpo is attached
compositeCPOLearnerProps = function(cpo, learner) {
  props = setdiff(getLearnerProperties(learner), "weights")
  props = union(props, getLearnerType(learner))
  # relevant: we only have an influence on these properties.
  relevant = c(cpo.dataproperties, cpo.targetproperties, cpo.tasktypes, "prob", "se")
  props.relevant = intersect(props, relevant)
  props.relevant = composeProperties(cpo$properties,
    list(handling = props.relevant, adding = character(0), needed = character(0),
      adding.min = character(0), needed.max = character(0)),
    cpo$debug.name, getLearnerName(learner))$handling  # checks for property problems automatically
  newprops = c(props.relevant, setdiff(props, relevant), "prob", "se")

  # check whether the promised predict.types actually work
  not.working.predict.types = Filter(function(ptype) {
    cpo$predict.type[ptype] %nin% c(props, "response")
  }, c("response", "prob", "se"))

  if ("response" %in% not.working.predict.types) {
    stopf("Cannot add CPO to Learner, since for 'response' prediction, the Learner must have '%s' prediction capability.",
      cpo$predict.type["response"])
  }

  setdiff(newprops, not.working.predict.types)
}

# wraps around callCPO and makeChainModel
#' @export
trainLearner.CPOLearner = function(.learner, .task, .subset = NULL, ...) {
  if (!is.null(.subset) && !identical(as.integer(c(.subset)), seq_along(.subset))) {
    .task = subsetTask(.task, .subset)  # nocov
  }

  cpo = .learner$cpo
  cpo$par.vals = subsetParams(.learner$par.vals, cpo$par.set)

  # note that an inverter for a model makes no sense, since the inverter is crucially bound to
  # the data that is supposed to be *predicted*.
  .task = clearRI(.task)

  transformed = callCPO(cpo, .task, TRUE, NULLCPO, FALSE, NULL)

  model = makeChainModel(train(.learner$next.learner, transformed$data), "CPOWrappedModel")
  model$retrafo = transformed$retrafo
  model
}

# Wraps around callCPORetrafo and invertCPO
#' @export
predictLearner.CPOLearner = function(.learner, .model, .newdata, ...) {
  cpo.retrafo = .model$learner.model$retrafo
  if (!is.nullcpo(cpo.retrafo)) {
    retrafod = callCPORetrafoElement(cpo.retrafo$element, .newdata, TRUE, NULLCPO)
    inverter = retrafod$inverter
    .newdata = retrafod$data
  } else {
    inverter = NULLCPO
  }
  prediction = NextMethod()
  if (!is.nullcpo(inverter)) {
    # check prediction before we feed it to CPOs
    prediction = checkPredictLearnerOutput(.learner$next.learner, .model$learner.model$next.model, prediction)
    # invert
    invertCPO(inverter$element, prediction, .learner$predict.type)$new.prediction
  } else {
    prediction
  }
}

# Makes sure a valid 'BaseWrapperModel' is created even when CPO fails.
# If CPO fails then we don't create a 'BaseWrapper' model.
#' @export
makeWrappedModel.CPOLearner = function(learner, learner.model, task.desc, subset = NULL, features, factor.levels, time) {
  x = NextMethod()
  if ("FailureModel" %in% class(x)) {
    class(x) = setdiff(class(x), "BaseWrapperModel")
  }
  x
}


#' @export
# Target Bound CPOs have the possibility of mapping some predict.type values
# of a wrapped learner to different predict.type values of the base learner.
# Therefore we need to overload setPredictType.
setPredictType.CPOLearner = function(learner, predict.type) {
  assertChoice(predict.type, c("response", "prob", "se"))
  ptconvert = learner$cpo$predict.type
  supported.below = c(intersect(getLearnerProperties(learner$next.learner), c("prob", "se")), "response")
  supported.here = names(ptconvert)[ptconvert %in% supported.below]
  assertSetEqual(supported.here, c(intersect(getLearnerProperties(learner), c("prob", "se")), "response"))
  if (!predict.type %in% supported.here) {
    stopf("Trying to predict %s, but %s does not support that.", predict.type, learner$id)
  }
  learner$predict.type = predict.type
  learner$next.learner = setPredictType(learner$next.learner, unname(ptconvert[predict.type]))
  learner
}

#' @export
getLearnerProperties.CPOLearner = function(learner) {
  # we could do this dynamically, always query the learner below.
  # then learner's properties could depend on its hyperparameters.
  # Whenever there is a conflict of properties (a cpo producing
  # missings when the learner below in its configuration happens to
  # not be able to handle missings) one could return 'empty' properties
  # -- the learner is not able to handle any data.
  #
  # One would ideally check whether some properties are fixed or depend
  # on parameters, and give an error on construction if there is a conflict
  # that will always be present.
  #
  # The much simpler and almost as good solution is to give maximal freedom
  # with properties and to just let the learner crash when something does not
  # work together as it should. This is bound to happen anyways
  # (e.g. because missings don't give info which kind of data is missing, and
  #  some CPO might be fine with missings in factors, but not with missings in
  #  numerics) unless one rewrites the whole properties-datatypes-hyperparameter
  # stuff in something like prolog.
  learner$properties
}

# Shorthand function to get a name for a learner that can be printed in debug messages
# @param learner [Learner] the learner to query
# @return [character(1)] a shorthand identifier, e.g. for debug output
getLearnerName = function(learner) {
  firstNonNull(learner$name, learner$shortname, learner$id)
}

##################################
### CPO-Learner Disassembly    ###
##################################

#' @title Get the CPO Associated with a Learner
#'
#' @description
#' Returns the (outermost) chain of \code{\link{CPO}}s that are part of a \code{\link[mlr:makeLearner]{Learner}}. This is useful to inspect the
#' preprocessing done by a learner object.
#'
#' If there are hidden CPOs (e.g. if a learner has CPOs, but is then wrapped by a \code{TuneWrapper}),
#' this function can not retrieve these CPOs, but it will emit a warning if \code{warn.buried} is \code{TRUE}.
#'
#' The retrieved CPOs will have the hyperparameter set according to the hyperparameter settings of the Learner.
#'
#' This function is complementary to \code{\link{getLearnerBare}}.
#'
#' @param learner [\code{\link[mlr:makeLearner]{Learner}}]\cr
#'   The learner to query
#' @param warn.buried [\code{logical(1)}]\cr
#'   Whether to warn about CPOs that could not be retrieved.
#' @return [\code{\link{CPO}}]. The (possibly composite) CPO found attached to \code{learner}.
#' @family CPOLearner related
#' @export
getLearnerCPO = function(learner, warn.buried = TRUE) {
  checkLearner(learner)
  name = getLearnerName(learner)
  appending = TRUE
  result = NULLCPO
  repeat {
    if (!"CPOLearner" %in% class(learner)) {
      if (is.atomic(learner) || is.null(learner$next.learner)) {
        break
      }
      appending = FALSE
    } else {
      if (is.atomic(learner) || is.null(learner$next.learner)) {
        stop("Error: found learner with class CPOLearner but without $next.learner slot.")
      }
      if (appending) {
        result = result %>>% singleLearnerCPO(learner)
      } else {
        warningf("Learner %s had buried CPOs", name)
        break
      }
    }
    learner = learner$next.learner
  }
  result
}

#' @title Get the Learner with the CPOs Removed
#'
#' @description
#' Get the bare \code{\link[mlr:makeLearner]{Learner}} without the \code{\link{CPO}}s that were previously added.
#'
#' It is still possible for the result to be a wrapped learner, e.g. a
#' TuningWrapper wrapped learner. It is also possible that below the
#' tuning wrapper, there are more CPOs. These can and will not be removed.
#'
#' This function is complementary to \code{\link{getLearnerCPO}}.
#'
#' @param learner [\code{\link[mlr:makeLearner]{Learner}}]\cr
#'   The learner to strip.
#' @return [\code{\link[mlr:makeLearner]{Learner}}]. The learner without attached CPOs.
#' @family CPOLearner related
#' @export
getLearnerBare = function(learner) {
  checkLearner(learner)
  while ("CPOLearner" %in% class(learner)) {
    if (is.atomic(learner) || is.null(learner$next.learner)) {
      stop("Error: found learner with class CPOLearner but without $next.learner slot.")
    }
    learner = learner$next.learner
  }
  learner
}

# get CPO from learner, without recursing into deeper levels.
# Care needs to be taken that the learner's parameter values that concern the CPO are kept.
# @param learner [CPOLearner] the learner to get the CPO from
# @return [CPO] the cpo of the given CPOLearner
singleLearnerCPO = function(learner) {
  cpo = learner$cpo
  cpo$par.vals = subsetParams(learner$par.vals, cpo$par.set)
  cpo
}

#' @export
retrafo.CPOModel = function(data) {
  # go through the chained model and see if there are wrapped models that
  # are not %>>%-chained (since the user probably wants to be warned about
  # that.
  recurseRetrafo = function(model, prev) {
    res = singleModelRetrafo(model, prev)
    next.model = model$learner.model$next.model
    if ("BaseWrapperModel" %in% class(next.model)) {
      if ("CPOModel" %in% class(next.model)) {
        return(recurseRetrafo(next.model, res))
      }
      do.message = FALSE
      while (!is.null(next.model)) {
        if (!is.list(next.model$learner.model)) {
          break
        }
        next.model = next.model$learner.model$next.model
        if ("CPOModel" %in% class(next.model)) {
          warningf("The model apparently has some CPOs wrapped by other wrappers\n%s\n%s",
            "The resulting retrafo will only cover the operations up to",
            "the first non-CPO wrapper!")
          do.message = FALSE
          break
        }
        if (!"BaseWrapperModel" %in% class(next.model)) {
          do.message = TRUE
        }
      }
      if (do.message) {
        message("The model has some wrappers besides CPOs, which will not be part of the retrafo.")
      }
    }
    res
  }
  recurseRetrafo(data, NULL)
}

# get RETRAFO from mlr model, without recursing into deeper levels.
# possibly concatenate with another retrafo 'prev'
# @param model [CPOModel] the model to query
# @param prev [CPORetrafo | NULL] the retrafo to prepend, if any
# @return [CPORetrafo] the CPORetrafo from `model`, concatenated with `prev` if available
singleModelRetrafo = function(model, prev) {
  retrafo = model$learner.model$retrafo
  if (!is.null(prev)) {
    retrafo = composeCPO(prev, retrafo)
  }
  retrafo
}
