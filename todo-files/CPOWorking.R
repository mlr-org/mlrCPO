
library("roxygen2")

changeData
* checkLearner


#' Exported for internal use.
#' @param id [\code{character}]\cr
#'   task id
#' @param data [\code{data.frame}]\cr
#'   data
#' @param target [\code{character}]\cr
#'   target columns
#' @param weights [\code{numeric}]\cr
#'   weights
#' @param blocking [\code{numeric}\cr
#'   task data blocking
#' @name makeTaskDesc

#' @export
#' @rdname makeTaskDesc

roxygenise("../../mlr")

devtools::load_all("../../ParamHelpers")
devtools::test(pkg = "../../ParamHelpers")

roxygenise("../../mlr")

devtools::load_all("../../mlr")

devtools::load_all("..", export_all = FALSE)

devtools::load_all("..", export_all = FALSE)

 constFeatRem = makeCPO("constFeatRem",
   dataformat = "df.features",
   cpo.trafo = function(data, target) {
     cols.keep = names(Filter(function(x) {
         length(unique(x)) > 1
       }, data))
     # the following function will do both the trafo and retrafo
     result = function(data) {
       data[cols.keep]
     }
     result
   })

head(iris) %>>% constFeatRem()

debugger()
options(error = dump.frames)
configureMlr(show.info = TRUE, on.learner.error = "stop", show.learner.output = TRUE)

listCPO()

library("testthat")

devtools::test(pkg = "..")

devtools::test(pkg = "..", filter = "cpo")


makeDiscreteVectorLearnerParam("test",
  default = list("a", "b"), values = c("a", "b", "c"), len = NA)
makeDiscreteVectorLearnerParam("test",
  default = list(), values = c("a", "b", "c"), len = NA)
x
makeDiscreteVectorLearnerParam("test",
  values = c("a", "b", "c"), len = NA)

devtools::load_all("../../ParamHelpers")
devtools::test(pkg = "../../ParamHelpers")

makeLogicalVectorLearnerParam("test",
  default = c(TRUE, TRUE, FALSE))


devtools::test(pkg = "..", filter = "cpo_basic")
devtools::test(pkg = "..", filter = "cpo_properties")
devtools::test(pkg = "..", filter = "cpo_datasplit")
devtools::test(pkg = "..", filter = "cpo_quick")
devtools::test(pkg = "..", filter = "cpo_cbind")
devtools::test(pkg = "..", filter = "cpo_concrete")
devtools::test(pkg = "..", filter = "cpo_meta")
devtools::test(pkg = "..", filter = "cpo_impute")
devtools::test(pkg = "..", filter = "cpo_filter")

system.time(devtools::test(pkg = "..", filter = "cpo_dataformat"), gcFirst = FALSE)

###################################
devtools::test(pkg = "..", filter = "cpo$", reporter = c("summary", "stop"))

system.time(devtools::test(pkg = "..", filter = "cpo_dataformat", reporter = c("summary", "stop")), gcFirst = FALSE)
devtools::test(pkg = "..", filter = "ParamSetSugar")

rm(list = ls())

tst = makeCPOTargetOp("test", .type = "regr",
                      cpo.trafo = {
                        control = 0
                        data.frame(target = target + 1000)
                      }, cpo.retrafo = { data })

df = data.frame(a = c(1, 2, 1), b = c(1, , 1))

getTaskData(df %>>% tst(), target.extra = TRUE)

x = train(tst() %>>% makeLearner("regr.lm"), makeRegrTask(data = df, target = "a"))

x$learner.model$next.model$learner.model

getLearnerType(makeLearner("classif.logreg"))
