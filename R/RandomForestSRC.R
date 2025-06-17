
# ----------------------
# This file is adapted from mlr's randomForestSRC filter code.
# Source: https://github.com/mlr-org/mlr/blob/92053c86c604dd9d9165e2403c9dfffb192e6697/R/Filter.R
#
# mlr is licensed under a BSD 2-clause license
#
# Copyright 2013-2018 Bernd Bischl
#
# Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:
#
# 1. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
#
# 2. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

# randomForestSRC_importance ----------------

#' Filter \dQuote{randomForestSRC_importance} computes the importance of random forests
#' fitted in package \pkg{randomForestSRC}. The concrete method is selected via
#' the `method` parameter. Possible values are `permute` (default), `random`,
#' `anti`, `permute.ensemble`, `random.ensemble`, `anti.ensemble`.
#' See the VIMP section in the docs for [randomForestSRC::rfsrc] for
#' details.
#'
#' @rdname randomForestSRC_filters
#' @name randomForestSRC_filters
#' @family filter
NULL



.FilterRegister = function() {
  .FilterRegister = get(".FilterRegister", envir = getNamespace("mlr"))  # nolint


# for some reason we cannot call 'randomForestSRC_importance' directly as we then face
# nested recursion problems when using other methods than "md".
if (!"randomForestSRC_importance" %in% names(.FilterRegister)) {
rf.importance = makeFilter(
  name = "randomForestSRC_importance",
  desc = "Importance of random forests fitted in package 'randomForestSRC'. Importance is calculated using argument 'permute'.",
  pkg  = "randomForestSRC",
  supported.tasks = c("classif", "regr", "surv"),
  supported.features = c("numerics", "factors", "ordered"),
  fun = function(task, nselect, method = "permute", ...) {
    assertChoice(method, choices = c("permute", "random", "anti", "permute.ensemble", "random.ensemble",  "anti.ensemble"))
    im = randomForestSRC::rfsrc(getTaskFormula(task), data = getTaskData(task), proximity = FALSE,
      forest = FALSE, importance = method, ...)$importance
    if (inherits(task, "ClassifTask")) {
      ns = rownames(im)
      y = im[, "all"]
    } else {
      ns = names(im)
      y = unname(im)
    }
    setNames(y, ns)
  }
)
}
if (!"rf.importance" %in% names(.FilterRegister)) {
.FilterRegister[["rf.importance"]] = .FilterRegister[["randomForestSRC_importance"]]
.FilterRegister[["rf.importance"]]$desc = "(DEPRECATED)"
.FilterRegister[["rf.importance"]]$fun = function(...) {
  .Deprecated(old = "Filter 'rf.importance'", new = "Filter 'randomForestSRC_importance' (package randomForestSRC)")
  .FilterRegister[["randomForestSRC_importance"]]$fun(...)
}

}

if (!"randomForestSRC_importance" %in% names(.FilterRegister)) {
randomForestSRC.rfsrc = makeFilter(
  name = "randomForestSRC_importance",
  desc = "Importance of random forests fitted in package 'randomForestSRC'. Importance is calculated using argument 'permute'.",
  pkg  = "randomForestSRC",
  supported.tasks = c("classif", "regr", "surv"),
  supported.features = c("numerics", "factors", "ordered"),
  fun = function(task, nselect, method = "permute", ...) {
    assertChoice(method, choices = c("permute", "random", "anti", "permute.ensemble", "random.ensemble",  "anti.ensemble"))
    im = randomForestSRC::rfsrc(getTaskFormula(task), data = getTaskData(task), proximity = FALSE,
      forest = FALSE, importance = method, ...)$importance
    if (inherits(task, "ClassifTask")) {
      ns = rownames(im)
      y = im[, "all"]
    } else {
      ns = names(im)
      y = unname(im)
    }
    setNames(y, ns)
  }
)
}
if (!"randomForestSRC.rfsrc" %in% names(.FilterRegister)) {
.FilterRegister[["randomForestSRC.rfsrc"]] = .FilterRegister[["randomForestSRC_importance"]]
.FilterRegister[["randomForestSRC.rfsrc"]]$desc = "(DEPRECATED)"
.FilterRegister[["randomForestSRC.rfsrc"]]$fun = function(...) {
  .Deprecated(old = "Filter 'randomForestSRC.rfsrc'", new = "Filter 'randomForestSRC_importance' (package randomForestSRC)")
  .FilterRegister[["randomForestSRC_importance"]]$fun(...)
}

}

.FilterRegister
}
