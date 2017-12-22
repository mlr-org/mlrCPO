

#' @title Over- or Undersample Binary Classification Tasks
#'
#' @template cpo_doc_intro
#'
#' @description
#' Oversamples the minor or undersamples the major class in
#' a binary classification task to alleviate class imbalance.
#' Uses \code{\link[mlr:oversample]{mlr::oversample}} and
#' \code{\link[mlr:oversample]{mlr::undersample}}, see documentation
#' there.
#'
#' @param rate [\code{numeric(1)} | \code{NULL}]\cr
#'   Factor to up- or downsample a class. Must be between 0
#'   and 1 for undersampling and greater or equal 1 for oversampling.
#'   If this is \code{NULL}, this is the ratio of major to minor
#'   class prevalence (for oversampling, or the inverse for undersampling).
#'   Must not be \code{NULL} if \code{cl} is not \code{NULL} and not the
#'   minor class for oversampling / the major class for undersampling.
#'   Default is \code{NULL}.
#' @param cl [\code{character(1)} | \code{NULL}]\cr
#'   Class to over- or undersample. For \code{NULL}, the minor class
#'   for oversampling or the major class for undersampling is chosen
#'   automatically.
#' @template cpo_doc_outro
#' @export
cpoOversample = makeCPORetrafoless("osw",  # nolint
  pSS(rate = NULL: numeric[1, ~.] [[special.vals = list(NULL)]],
    cl = NULL: character [[special.vals = list(NULL)]]),
  dataformat = "task",
  properties.target = c("classif", "twoclass"),
  cpo.trafo = {
    classinfo = getBinClassInfo(data)
    if (is.null(cl)) {
      cl = classinfo$minclass
    } else {
      assertChoice(cl, classinfo$classes)
    }
    if (is.null(rate)) {
      if (cl != classinfo$minclass && classinfo$rate != 1.0) {  # don't complain in a tie
        stop("'rate' may not be NULL when 'cl' is neither NULL nor the minor class")
      }
      rate = classinfo$rate
    }
    oversample(data, rate, cl)
  })

#' @rdname cpoOversample
#' @export
cpoUndersample = makeCPORetrafoless("usw",  # nolint
  pSS(rate = NULL: numeric[~0, 1] [[special.vals = list(NULL)]],
    cl = NULL: character [[special.vals = list(NULL)]]),
  dataformat = "task",
  properties.target = c("classif", "twoclass"),
  cpo.trafo = {
    classinfo = getBinClassInfo(data)
    if (is.null(cl)) {
      cl = classinfo$maxclass
    } else {
      assertChoice(cl, classinfo$classes)
    }
    if (is.null(rate)) {
      if (cl != classinfo$maxclass && classinfo$rate != 1.0) {  # don't complain in a tie
        stop("'rate' may not be NULL when 'cl' is neither NULL nor the major class")
      }
      rate = 1 / classinfo$rate
    }
    undersample(data, rate, cl)
  })

# Get the major / minor class and their relative size
#
# @param task [Task]
# @return [list] list(classes, maxclass, minclass, rate)
#   'rate' is number(maxclass) / number(minclass)
getBinClassInfo = function(task) {
  cdist = table(getTaskData(task, target.extra = TRUE)$target)
  assert(length(cdist) == 2)
  list(classes = names(cdist),

    maxclass = names(which.max(cdist)),
    minclass = names(which.min(rev(cdist))),  # 'rev' for ties
    rate = max(cdist) / min(cdist))
}

registerCPO(cpoOversample(), "subsampling", "binary classif", "oversample minor class")
registerCPO(cpoUndersample(), "subsampling", "binary classif", "undersample major class")


#' @title Sample Data from a Task
#'
#' @template cpo_doc_intro
#'
#' @description
#' Takes samples from a task to decrease (or possibly increase) its size. This
#' can be used to reduce training time, or to implement bootstrapping.
#'
#' @param rate [\code{numeric(1)} | \code{NULL}]\cr
#'   How many samples to take, relative to the task size. Default is \code{NULL}:
#'   Not using relative sampling rate. Exactly one of this or \code{size} must be
#'   non-\code{NULL}.
#' @param size [\code{integer(1)} | \code{NULL}]\cr
#'   How many samples to take. Default is \code{NULL}: Not using absolute size.
#'   Exactly one of this or \code{size} must be non-\code{NULL}.
#' @param replace [\code{logical(1)}]\cr
#'   Whether to sample with replacement. Default is \code{FALSE}.
#' @template cpo_doc_outro
#' @export
cpoSample = makeCPORetrafoless("sample",  # nolint
  pSS(rate = NULL: numeric[~0, ~.] [[special.vals = list(NULL)]],
    size = NULL: integer[1, ] [[special.vals = list(NULL)]],
    replace = FALSE: logical),
  dataformat = "df.all",
  cpo.trafo = {
    if (is.null(rate) + is.null(size) != 1) {
      stop("Exactly one of 'rate' or 'size' must be NULL")
    }
    if (is.null(size)) {
      size = round(nrow(data) * rate)
    }
    indices = sample(nrow(data), size, replace)
    data[indices, ]
  })
registerCPO(cpoSample(), "subsampling", "general", "sample from task")

