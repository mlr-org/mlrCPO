#' @import methods
#' @import BBmisc
#' @import checkmate
#' @import stringi

.onLoad = function(libname, pkgname) {
  backports::import(pkgname)
}
