

#' @title Over- or Undersample Binary Classification Tasks
#'
#' @template cpo_doc_intro
#'
#' @description
#' Oversamples the minor or undersamples the major class in
#' a binary classification task to alleviate class imbalance.
#' Uses \code{\link[mlr:oversample]{mlr::oversample}} and
#' \code{\link[mlr:undersample]{mlr::undersample}}, see documentation
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



#' @title Subsample a Task
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
#' @param size [\code{numeric(1)} | \code{NULL}]\cr
#'   How many samples to take. Default is \code{NULL}: Not using absolute size.
#'   Exactly one of this or \code{size} must be non-\code{NULL}.
#' @param replace [\code{logical(1)}]\cr
#'   Whether to sample with replacement. Default is \code{FALSE}.
#' @template cpo_doc_outro
#' @export

