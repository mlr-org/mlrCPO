#' @import methods
#' @importFrom stats setNames predict
#' @import BBmisc
#' @import checkmate
#' @import stringi
#' @import ParamHelpers
#' @import mlr


.onLoad = function(libname, pkgname) {
  backports::import(pkgname)
}

.onAttach = function(libname, pkgname) {
  covrTraceCPOs()
}
