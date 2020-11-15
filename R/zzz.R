#' @import methods
#' @importFrom stats setNames predict
#' @import checkmate
#' @import BBmisc
#' @import stringi
#' @import ParamHelpers
#' @import mlr


.onLoad = function(libname, pkgname) {
  backports::import(pkgname)
}

.onAttach = function(libname, pkgname) {
  covrTraceCPOs()
}
