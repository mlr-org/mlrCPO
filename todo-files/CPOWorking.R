
library("roxygen2")


roxygenise("..")


debugger()

devtools::load_all("../../ParamHelpers")
devtools::load_all("../../mlr")


devtools::load_all("..", export_all = FALSE)

devtools::load_all("..", export_all = FALSE)

devtools::build_vignettes("..")


devtools::load_all("..")

library("checkmate")




options(error = dump.frames)


options(max.print = 2000)


configureMlr(show.info = TRUE, on.learner.error = "stop", show.learner.output = TRUE)

devtools::load_all("..")



devtools::test(pkg = "..")




# all tests:
devtools::test(pkg = "..", filter = "affect")       # [ 00 ]
devtools::test(pkg = "..", filter = "basic")        # [ 00 ]
devtools::test(pkg = "..", filter = "cbind")        # [ 00 ]
devtools::test(pkg = "..", filter = "concrete")     # [ 00 ]
devtools::test(pkg = "..", filter = "datasplit")    # [ 00 ]
devtools::test(pkg = "..", filter = "filter")       # [ 00 ]
devtools::test(pkg = "..", filter = "impute")       # [ 00 ]
devtools::test(pkg = "..", filter = "meta")         # [ 00 ]
devtools::test(pkg = "..", filter = "properties")   # [ 00 ]
devtools::test(pkg = "..", filter = "quick")        # [ 00 ]
devtools::test(pkg = "..", filter = "tuning")       # [ 00 ]


devtools::test(pkg = "..", filter = "dataflow")


devtools::test(pkg = "..", filter = "lint")         # [ ?? ]




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



#####
# try to find out what return values different learners use

ll = listLearners(create = TRUE)

tasks = list(classif = pid.task, regr = bh.task, surv = lung.task, cluster = mtcars.task, multilabel = agri.task)

configureMlr(on.learner.error = "warn", on.learner.warning = "warn", show.learner.output = TRUE, show.info = TRUE)

cll = Filter(function(x) getLearnerType(x) == "cluster" && "prob" %in% getLearnerProperties(x), ll)

getLearnerParams
(cll[[2]])

names(predict(train(setPredictType(setHyperPars(cll[[2]], centers = 1), "prob"), tasks$cluster), tasks$cluster)$data)
xx = predict(train(setPredictType(cll[[2]], "prob"), tasks$cluster), tasks$cluster)
str(xx)

dropNamed(xx$data, c("id", "response"))

debugger()
