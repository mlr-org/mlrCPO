# attributes.R defines the functions that access the `retrafo` and `inverter` attributes of
# objects.

##################################
### Generics                   ###
##################################

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
#' @return [\code{CPOTrained}]. The retransformation function that can be
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
#' trained = traindat %>>% cpoPca() %>>% cpoScale()
#' reFun = retrafo(trained)
#' predicted = reFun(preddat)
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
#' @param value [\code{CPOTrained}]\cr
#'   An inverter chain.
#'
#' @family CPO
#' @export
`inverter<-` = function(data, value) {
  UseMethod("inverter<-")
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
  nullcpoToNull(value)
  attr(data, "retrafo") = value
  data
}

#' @export
`retrafo<-.WrappedModel` = function(data, value) {
  stop("Cannot change retrafo of a model!")
}

#' @title Check CPORetrafo
#'
#' @description
#' Check whether the given object is a \code{CPORetrafo} object.
#'
#' @param x [any]\cr
#'   The object to check.
#'
#' @return \code{TRUE} if \code{x} has class \code{CPORetrafo}, \code{FALSE} otherwise.
#'
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
