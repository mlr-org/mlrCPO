# attributes.R defines the functions that access the `retrafo` and `inverter` attributes of
# objects.

##################################
### Generics                   ###
##################################

#' @title Get the Retransformation or Inversion Function from a Resulting Object
#'
#' @description
#' When applying a \code{\link{CPO}} to a \code{\link[base]{data.frame}} or \code{\link[mlr]{Task}},
#' the data is not only changed, additionally a retransformation and an inversion
#' object is created that can be applied to other data of the same
#' kind. This is useful if new data (for prediction or validation) is to be handled in the same machine learning
#' procedure.
#'
#' For example, when performing PCA on training data using \code{\link{cpoPca}}, the rotation
#' matrix is saved and can be used on new (prediction) data. As another example, consider
#' a log-transformation of the target column in a regression problem. When predictions are made with
#' new data, it may be useful to invert the transformation on the predicted values by exponentiating them.
#'
#' The information created when a \code{\link{CPO}} is applied is saved in a \code{CPORetrafo} object, and a \code{CPOInverter}
#' object, which are both saved as \emph{attributes}. The \code{retrafo} and \code{inverter} function
#' retrieve these objects. It is furthermore possible to set these attributes using the \code{retrafo<-}
#' and \code{inverter<-} functions, using constructs like \code{retrafo(data) <- retr.obj}. The \code{retrafo}
#' or \code{inverter} attributes can be reset individually by setting them to \code{NULL}:
#' \code{retrafo(data) <- NULL}, or by using the \code{\link{clearRI}} function.
#'
#' When chaining \code{\link{\%>>\%}} on a data object, the retrafo and inverter
#' associated with the result is also chained automatically. Beware,
#' however, that this just accesses the retrafo attribute internally. Therefore, if you plan to do apply
#' multiple transformations with other operations in between,
#' make sure to reset the retrafo function by setting it to \code{NULL}, or using the \code{\link{clearRI}}
#' function. See examples.
#'
#' @section \code{CPORetrafo} and \code{CPOInverter}:
#' \code{CPORetrafo} and \code{CPOInverter} objects are members of the \emph{\code{CPOTrained}} class, which can be handled similarly to CPO objects:
#' Their hyperparameters can be inspected using \code{\link[mlr]{getParamSet}} and \code{link[mlr]{getHyperPars}},
#' \code{\link{print.CPOTrained}} is used for (possibly verbose) printing. To apply the retrafo or inverter transformation represented by the
#' object to data, use the \code{\link{applyCPO}} or \code{\link{\%>>\%}} function.
#'
#' \code{CPOTrained} objects can be chained using \code{\link{\%>>\%}} or \code{\link{pipeCPO}}, and broken into primitives using \code{\link{as.list.CPOTrained}}.
#' However, since the \code{CPOTrained} objects represent transformations that relate closely to the data used to train it (and therefore
#' to the position within a CPO pipeline), it is only advisable to chain or break apart \code{CPOTrained} pipes for inspection, or
#' if you really know what you are doing.
#'
#' (Primitive) \code{CPORetrafo} objects can be inspected using \code{\link{getCPOTrainedState}}, and it is possible to create new \code{CPORetrafo}
#' objects from (possibly modified) retrafo state using \code{\link{makeCPOTrainedFromState}}.
#'
#' @section Difference between \code{CPORetrafo} and \code{CPOInverter}:
#' The fundamental difference between \code{CPORetrafo} and \code{CPOInverter} is that a \code{CPORetrafo} is
#' created only when a \code{\link{CPO}} is applied to a data set, and is used to perform the same transformation on new
#' (prediction) data. The \code{CPOInverter} is created whenever a \code{\link{CPO}} \emph{or} \code{CPORetrafo} is
#' applied to data (whether training or prediction data). It is in fact used to invert the transformation done to the target
#' column of a \code{\link[mlr]{Task}}. Since this operation may depend on the new prediction data, and not only on the training
#' data fed to the \code{\link{CPO}} when the \code{CPORetrafo} was created, the \code{CPOInverter} object is more
#' closely bound to the particular data set used to create it.
#'
#' In some cases a target transformation is independent of the data used to create it (e.g. log-transform of a regression target
#' column); in that case the \code{CPORetrafo} can be used with \code{\link{invert}}. This is the concept of
#' \code{\link{CPOTrainedCapability}}, which can be queried using \code{\link{getCPOTrainedCapability}}.
#'
#' @section Using \code{CPORetrafo}:
#' \code{CPORetrafo} objects can be applied to new data sets using the \code{\link{\%>>\%}} operator, the
#' \code{\link{applyCPO}} generic, or the \code{\link[stats]{predict}} generic, all of which perform the same action.
#'
#' @section Using \code{CPOInverter}:
#' To use a \code{CPOInverter}, use the \code{\link{invert}} function.
#'
#' @param data [\code{\link[base]{data.frame}} | \code{\link[mlr]{Task}} | \code{\link[mlr:makeWrappedModel]{WrappedModel}}]\cr
#'   The result of a \code{\link{CPO}} applied to a data set.
#'
#' @param value [\code{CPOTrained} | NULL]\cr
#'   The retrafo or inverter to set. This must either be a
#'   \code{CPORetrafo} for \code{retrafo<-} or a \code{CPOInverter} for \code{inverter<-},
#'   or \code{NULL} to reset the \code{retrafo} or \code{inverter} attributes.
#'
#' @return [\code{CPOTrained}]. The retransformation function that can be
#'   applied to new data. This is a \code{CPORetrafo} object for \code{retrafo}
#'   or a \code{CPOInverter} object for \code{inverter}.
#'
#' @seealso \code{\link{clearRI}} about the problem of needing to reset \code{retrafo} and \code{inverter} attributes sometimes.
#' @family CPO lifecycle related
#' @family retrafo related
#' @family inverter related
#' @aliases CPORetrafo CPOInverter
#' @examples
#' traindat = subsetTask(pid.task, 1:400)
#' preddat = subsetTask(pid.task, 401:768)
#'
#' trained = traindat %>>% cpoPca()
#' reFun = retrafo(trained)
#' predicted = preddat %>>% reFun
#' head(getTaskData(predicted))
#'
#' # chaining works
#' trained = traindat %>>% cpoPca() %>>% cpoScale()
#' reFun = retrafo(trained)
#' predicted = preddat %>>% reFun
#' head(getTaskData(predicted))
#'
#' # reset the retrafo when doing other steps!
#'
#' trained.tmp = traindat %>>% cpoPca()
#' reFun1 = retrafo(trained.tmp)
#'
#' imp = impute(trained.tmp)
#' trained.tmp = imp$task  # nonsensical example
#' retrafo(trained.tmp) = NULL  # NECESSARY HERE
#'
#' trained = trained.tmp %>>% cpoScale()
#'
#' reFun2 = retrafo(trained)
#' predicted = getTaskData(reimpute(preddat %>>% reFun1, imp$desc),
#'   target.extra = TRUE)$data %>>% reFun2
#' @name CPOTrained
NULL

#' @rdname CPOTrained
#' @export
retrafo = function(data) {
  UseMethod("retrafo")
}

#' @rdname CPOTrained
#' @export
inverter = function(data) {
  UseMethod("inverter")
}

#' @rdname CPOTrained
#' @export
`retrafo<-` = function(data, value) {
  UseMethod("retrafo<-")
}

#' @rdname CPOTrained
#' @export
`inverter<-` = function(data, value) {
  UseMethod("inverter<-")
}

#' @title Clear Retrafo and Inverter Attributes
#'
#' @description
#' When applying \code{\link{CPO}}s to data, the operation entails
#' saving the \code{\link{CPOTrained}} information that gets generated
#' to an attribute of the resulting object. This is a useful solution to
#' the problem that applying multiple CPOs should also lead to a retrafo
#' object that performs the same multiple operations. However, sometimes
#' this may lead to surprising and unwanted results when a CPO is applied
#' and not meant to be part of a trafo-retrafo machine learning pipeline,
#' e.g. for dropping columns that occur in training but not in prediction
#' data. In that case, it is necessary to reset the \code{retrafo} and
#' possibly \code{inverter} attributes of the data being used. This can
#' be done either by using \code{retrafo(data) <- NULL}, or by using
#' \code{clearRI}. \code{clearRI} clears both \code{retrafo} \emph{and}
#' \code{inverter} attributes.
#'
#' @param data [\code{\link[base]{data.frame}} | \code{\link[mlr]{Task}} | \code{\link[mlr:makeWrappedModel]{WrappedModel}}]\cr
#'   The result of a \code{\link{CPO}} applied to a data set.
#' @return [\code{\link[base]{data.frame}} | \code{\link[mlr]{Task}} | \code{\link[mlr:makeWrappedModel]{WrappedModel}}] the
#'   \code{data} after stripping all \code{retrafo} and \code{inverter} attributes.
#'
#' @family retrafo related
#' @family inverter related
#' @examples
#' # without clearRI
#' transformed = iris.task %>>% cpoPca()
#' transformed2 = transformed %>>% cpoScale()
#' retrafo(transformed2)  # [RETRAFO pca]=>[RETRAFO scale]
#'
#' transformed = iris.task %>>% cpoPca()
#' transformed2 = clearRI(transformed) %>>% cpoScale()
#' retrafo(transformed2)  # [RETRAFO scale]
#'
#' @export
clearRI = function(data) {
  retrafo(data) = NULL
  inverter(data) = NULL
  data
}


##################################
### Retrafo                    ###
##################################

#' @export
retrafo.default = function(data) {
  res = attr(data, "retrafo")
  if (!any(c("data.frame", "Task") %in% class(data))) {
    warningf("data is not a Task or data.frame.\n%s\n%s",
      "are you sure you are applying 'retrafo' to the result",
      "of a %>>% transformation?")
  }

  nullToNullcpo(res)
}

#' @export
retrafo.WrappedModel = function(data) {
  NULLCPO
}

#' @export
`retrafo<-.default` = function(data, value) {
  if (!is.null(value)) {
    assert(is.retrafo(value))
  }
  if (!any(c("data.frame", "Task") %in% class(data))) {
    warningf("argument is neither a Task nor data.frame.\n%s\n%s",
      "are you sure you are applying it to the input or",
      "result of a %>>% transformation?")
  }

  attr(data, "retrafo") = nullcpoToNull(value)
  data
}

#' @export
`retrafo<-.WrappedModel` = function(data, value) {
  stop("Cannot change retrafo of a model!")
}

#' @title Check CPORetrafo
#'
#' @description
#' Check whether the given object is a \code{\link{CPORetrafo}} object.
#'
#' @param x [any]\cr
#'   The object to check.
#'
#' @return \code{TRUE} if \code{x} has class \code{\link{CPORetrafo}}, \code{FALSE} otherwise.
#' @family retrafo related
#' @export
is.retrafo = function(x) {  # nolint
  "CPORetrafo" %in% class(x)
}


##################################
### Inverter                   ###
##################################

#' @export
inverter.default = function(data) {
  res = attr(data, "inverter")
  if (!any(c("data.frame", "Task") %in% class(data))) {
    warningf("data is not a Task or data.frame.\n%s\n%s",
      "are you sure you are applying 'retrafo' to the result",
      "of a %>>% transformation?")
  }
  nullToNullcpo(res)
}

#' @export
inverter.WrappedModel = function(data) {
  stop("Cannot get inverter of a model!")
}

#' @export
`inverter<-.default` = function(data, value) {
  if (!is.null(value)) {
    assert(is.inverter(value))
  }
  if (!any(c("data.frame", "Task") %in% class(data))) {
    warningf("argument is neither a Task nor data.frame.\n%s\n%s",
      "are you sure you are applying it to the input or",
      "result of a %>>% transformation?")
  }
  value = nullcpoToNull(value)
  attr(data, "inverter") = value
  data
}

#' @export
`inverter<-.WrappedModel` = function(data, value) {
  stop("Cannot change inverter of a model!")
}

#' @title Check CPOInverter
#'
#' @description
#' Check whether the given object is a \code{\link{CPOInverter}} object.
#'
#' @param x [any]\cr
#'   The object to check.
#'
#' @return \code{TRUE} if \code{x} has class \code{\link{CPOInverter}}, \code{FALSE} otherwise.
#' @family inverter related
#' @export
is.inverter = function(x) {  # nolint
  "CPOInverter" %in% class(x)
}

