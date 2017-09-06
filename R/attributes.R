# attributes.R defines the functions that access the `retrafo` and `inverter` attributes of
# objects.

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
#' @param x\cr
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
