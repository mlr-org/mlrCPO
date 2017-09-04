#' @param perc [\code{numeric(1)}]\cr
#'   If set, select \code{perc}*100 top scoring features.
#'   Mutually exclusive with arguments \code{abs} and \code{threshold}.
#' @param abs [\code{numeric(1)}]\cr
#'   If set, select \code{abs} top scoring features.
#'   Mutually exclusive with arguments \code{perc} and \code{threshold}.
#' @param threshold [\code{numeric(1)}]\cr
#'   If set, select features whose score exceeds \code{threshold}.
#'   Mutually exclusive with arguments \code{perc} and \code{abs}.
