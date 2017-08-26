

##################################
### Generics                   ###
##################################


#' @title CPO Composition
#'
#' @description
#' The arguments will be composed. A new object,
#' representing the operation of performing both object's operations in succession,
#' will be created, which can be handled like a new \link{CPO} object.
#'
#' See the preferred \code{\link{\%>>\%}} for more info.
#'
#' @param cpo1 [\code{\link{CPO}}]\cr
#'   The operation to perform first.
#' @param cpo2 [\code{\link{CPO}}]\cr
#'   The operation to perform second.
#' @export
composeCPO = function(cpo1, cpo2) {
  assert(checkClass(cpo2, "CPO"),
    checkClass(cpo2, "CPORetrafo"))
  if (is.nullcpo(cpo2)) {
    cpo1
  }
  UseMethod("composeCPO")
}

#' @title CPO Attachment
#'
#' @description
#' The second argument is a \code{\link{Learner}} and the CPO will be attached to
#' this learner. The same operation will be performed during the \dQuote{train} and
#' \dQuote{predict} phase; the behaviour during the predict phase may furthermore
#' be depend on the training data.
#'
#' See the preferred \code{\link{\%>>\%}} for more info.
#'
#' @param cpo [\code{\link{CPO}}]\cr
#'   The CPO object
#' @param learner [\code{\link{Learner}}]\cr
#'   The learner.
#'
#' @family CPO
#' @export
attachCPO = function(cpo, learner) {
  checkLearner(learner)
  UseMethod("attachCPO")
}

#' @title CPO Apply
#'
#' @description
#' The given transformation will be applied to the data in the given \code{link{Task}}.
#'
#' See the preferred \code{\link{\%>>\%}} for more info.
#'
#' @param cpo [\code{\link{CPO}}]\cr
#'   The CPO representing the operation to perform.
#' @param task [\code{\link{Task}}]\cr
#'   The task to operate on.
#' @export
#' @family CPO
applyCPO = function(cpo, task) {
  assert(checkClass(task, "Task"),
    checkClass(task, "data.frame"))
  UseMethod("applyCPO")
}

#' @title Get the CPO object's Name
#'
#' @description
#' Return the name given at creation as \dQuote{.cpo.name} to the
#' CPO creator. If the CPO object has an ID, it will be appended.
#'
#' @param cpo [\code{\link{CPO}}]\cr
#'   The CPO object.
#'
#' @family CPO
#' @export
getCPOName = function(cpo) {
  UseMethod("getCPOName")
}

#' @title Get the Retransformation function from a resulting object
#'
#' @description
#' When applying a CPO to a \code{data.frame} or \code{\link{Task}},
#' the data is not only changed, additionally a retransformation
#' function is created that can be applied to other data of the same
#' kind.
#'
#' For example, when performing PCA on training data, the rotation
#' matrix is saved and can be used on new (prediction) data.
#'
#' \dQuote{retrafo} retrieves a function that can be applied to new
#' data sets and \code{Task}s.
#'
#' When chaining \code{\link{\%>>\%}} on a data object, the retrafo
#' associated with the result is also chained automatically. Beware,
#' however, that this just accesses the retrafu function with
#' \code{retrafo} internally. Therefore, if you plan to do apply
#' multiple transformations with certain operations in between,
#' make sure to reset the retrafo function by setting it to \code{NULL}.
#' See examples.
#'
#' @param data [\code{data.frame} | \code{\link{Task}} | \code{\link{WrappedModel}}]\cr
#'   The result of a \code{\link{\%>>\%}} chain applied to a data set.
#'
#' @return [\code{CPORetrafo}]. The retransformation function that can be
#'   applied to new data.
#'
#' @examples
#' \dontrun{
#' # FIXME: need to update this
#' traindat = subsetTask(pid.task, 1:400)
#' preddat = subsetTask(pid.task, 401:768)
#'
#' trained = traindat %>>% cpoPca()
#' reFun = retrafo(trained)
#' predicted = reFun(preddat)
#'
#' # chaining works
#' trained = traindat %>>% cpoPca(FALSE, FALSE) %>>% cpoScale()
#' reFun = retrafo(trained)
#' predicted = reFun(preddat)
#'
#' # reset the retrafo when doing other steps!
#'
#' trained.tmp = traindat %>>% cpoPca(FALSE, FALSE)
#' reFun1 = retrafo(trained.tmp)
#'
#' imp = impute(trained.tmp)
#' trained.tmp = imp$task  # nonsensical example
#' retrafo(trained.tmp) = NULL  # NECESSARY HERE
#'
#' trained = trained.tmp %>>% cpoScale()
#'
#' reFun2 = retrafo(trained)
#' predicted = reFun2(getTaskData(reimpute(
#'   reFun1(preddat), imp$desc), target.extra = TRUE)$data)
#'
#'
#' }
#' @family CPO
#' @export
retrafo = function(data) {
  UseMethod("retrafo")
}


#' @title Get the prediction inverse function
#'
#' @description
#' Gets the retrafo function that can be applied to the prediction.
#'
#' @param data [\code{data.frame} | \code{\link{Task}}]\cr
#'   The result of a \code{\link{\%>>\%}} chain applied to a data set.
#'
#' @family CPO
#' @export
inverter = function(data) {
  UseMethod("inverter")
}

#' @title Get the internal state of a Retrafo object
#'
#' @description
#' A retrafo function always has access to some kind of state
#' that represents information gotten from the training data,
#' as well as the parameters it was called with.
#'
#' The structure of the internal state depends on the CPO backend
#' used. For Functional CPO, the state is the environment of the
#' retrafo function, turned into a list. For Objectbased CPO,
#' the state is a list containing the parameters, as well as the
#' control object generated by the trafo function.
#'
#' The object can be slightly modified and used to create a new
#' CPO retrafo object using \code{\link{makeRetrafoFromState}}.
#'
#' @param retrafo.object [\code{CPORetrafo}]\cr
#'   The object to get the state of.
#'
#' @return a list.
#' @family CPO
#' @export
getRetrafoState = function(retrafo.object) {
  UseMethod("getRetrafoState")
}

#' @title Set the internal state of a Retrafo object
#'
#' @description
#' This creates a new \code{Retrafo} object which will
#' behave according to \dQuote{state}.
#'
#' @param constructor
#'   A cpo constructor
#' @param state
#'   A state gotten from another CPO retrafo object using
#'   \code{\link{getRetrafoState}}
#' @return a \code{CPORetrafo}.
#' @family CPO
#' @export
makeRetrafoFromState = function(constructor, state) {
  UseMethod("makeRetrafoFromState")
}

#' @title set an object's retransformation
#'
#' @description
#' Set an object's retransformation function, as described
#' in \code{\link{retrafo}}. Set to \code{NULL} to delete.
#'
#' @param data [\code{data.frame} | \code{\link{Task}}]\cr
#'   The task of which to set the retrafo.
#' @param value [\code{function} | NULL]\cr
#'   The retrafo function to set. This must either be a
#'   function accepting a \code{data.frame} and returning
#'   an object of the same kind, or NULL.
#'   In most cases, you should use this only within
#'   \code{CPOFunctionalConstructor} functions OR to
#'   reset an object's retrafo to NULL.
#'
#' @family CPO
#' @export
`retrafo<-` = function(data, value) {
  UseMethod("retrafo<-")
}


#' @title Set the prediction inverse function
#'
#' @description
#' Sets the retrafo function that can be applied to the prediction.
#'
#' @param data [\code{data.frame} | \code{\link{Task}}]\cr
#'   Something to be applied to a \code{\link{\%>>\%}} chain.
#'
#' @param value [\code{CPORetrafo}]\cr
#'   An inverter chain.
#'
#' @family CPO
#' @export
`inverter<-` = function(data, value) {
  UseMethod("inverter<-")
}


#' @title Set the ID of a CPO object.
#'
#' @description
#' Setting the ID of a CPO to a value will prefix all its
#' parameter names with this ID. This makes it possible to
#' compose CPOs that have clashing parameter names.
#'
#' @param id [\code{character(1)} | \code{NULL}]\cr
#'   The ID. If this is \code{NULL}, the ID is set to the
#'   default for the CPO at hand.
#'
#' @family CPO
#' @export
setCPOId = function(cpo, id) {
  if (!is.null(id)) {
    assertString(id)
  }
  if (!"CPOPrimitive" %in% class(cpo)) {
    stop("Cannot set ID of compound CPO.")
  }
  UseMethod("setCPOId")
}

#' @title Get the ID of a CPO object.
#'
#' @description
#' The ID of a CPO to a value will prefix all its
#' parameter names with this ID.
#'
#' @family CPO
#' @export
getCPOId = function(cpo) {
  UseMethod("getCPOId")
}

#' @export
getCPOId.CPO = function(cpo) {
  stop("Compound CPOs have no IDs.")
}

#' @title Get the Selection Arguments for affected CPOs
#'
#' @description
#' Get the \code{affected.*} arguments from when the CPO was constructed.
#'
#' @param cpo [\code{CPO}]\cr
#'   The CPO.
#' @param drop.defaults [\code{logical(1)}]\cr
#'   Whether to only return the arguments that deviate from the default.
#'   Default is \code{TRUE}.
#'
#' @family CPO
#' @export
getCPOAffect = function(cpo, drop.defaults = TRUE) {
  UseMethod("getCPOAffect")
}

#' @export
getCPOAffect.CPO = function(cpo, drop.defaults = TRUE) {
  stop("Compound CPOs have no affect arguments.")
}

#' @title Get the Properties of the given CPO object
#'
#' @description
#' The properties of a CPO object determine the kind of data the CPO will be able to handle, and how
#' it transforms data. Each entry can be one of: \dQuote{numerics}, \dQuote{factors}, \dQuote{ordered},
#' \dQuote{missings}.
#'
#' This function returns a list of three values: \dQuote{properties}, \dQuote{properties.adding}, and
#' \dQuote{properties.needed}.
#'
#' The \dQuote{properties} determines what data the CPO handles. If a property of a given data set is absent,
#' the preproc operator will reject the data.
#'
#' \dQuote{properties.adding} can be one or many of the same values as \dQuote{properties}. These properties
#' get added to a Learner (or CPO) coming after / behind this CPO. When a CPO imputes missing values, for example,
#' this is \dQuote{missings}. This is always a subset of \dQuote{properties}.
#'
#' \dQuote{properties.needed} can be one or many of the same values as \dQuote{properties}. These properties
#' are required from a Learner (or CPO) coming after / behind this CPO. E.g., when a CPO converts factors to
#' numerics, this is \dQuote{numerics} (and \dQuote{properties.adding} is \dQuote{factors}).
#'
#' @param cpo [\code{CPO}]\cr
#'   The CPO to query.
#'
#' @param only.data [\code{logical(1)}]\cr
#'   Only get the CPO properties relevant for data (not target or task types). Default is \code{FALSE}.
#'
#' @export
getCPOProperties = function(cpo, only.data = FALSE) {
  assertFlag(only.data)
  UseMethod("getCPOProperties")
}


#' @title Get the CPO Kind
#'
#' @description
#' Is either "trafo", "retrafo", or "inverter"
#'
#' @param cpo [\code{CPO}]\cr
#'   The CPO.
#'
#' @export
getCPOKind = function(cpo) {
  UseMethod("getCPOKind")
}

#' @title Get the CPO predict.type
#'
#' @description
#' Get the possible predict.types this CPO is able to handle.
#'
#' @param cpo [\code{CPO}]\cr
#'   The CPO.
#'
#' @export
getCPOPredictType = function(cpo) {
  UseMethod("getCPOPredictType")
}

#' @title determine the bound of a CPO or Retrafo
#'
#' @description
#' Gives the bound of a CPO or Retrafo. These can be
#' \dQuote{targetbound} for a CPO / Retrafo that
#' manipulates target columns, \dQuote{databound} for
#' a CPO / Retrafo that manipulates non-target columns.
#'
#' For a CPO that affects both, it is a \code{character(2)}
#' with both values; for one that affects neither, it is
#' \code{character(0)}.
#'
#' @param cpo [\code{CPO} | \code{CPORetrafo}]\cr
#'   The CPO or Retrafo to inspect.
#'
#' @export
getCPOBound = function(cpo) {
  UseMethod("getCPOBound")
}
