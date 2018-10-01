[![Build Status](https://travis-ci.org/mlr-org/mlrCPO.svg?branch=master)](https://travis-ci.org/mlr-org/mlrCPO)
[![Coverage](https://codecov.io/github/mlr-org/mlrCPO/branch/master/graphs/badge.svg)](https://codecov.io/github/mlr-org/mlrCPO)
[![CRAN Status Badge](https://www.r-pkg.org/badges/version/mlrCPO)](https://CRAN.R-project.org/package=mlrCPO)
[![CRAN Downloads](https://cranlogs.r-pkg.org/badges/mlrCPO)](https://CRAN.R-project.org/package=mlrCPO)


# mlrCPO: Composable Preprocessing Operators for [mlr](https://github.com/mlr-org/mlr)

GSoC 2017 Project: Operator Based Machine Learning Pipeline Construction

## What is CPO?

```R
> task = iris.task
> task %<>>% cpoScale(scale = FALSE) %>>% cpoPca() %>>%  # pca
>   cpoFilterChiSquared(abs = 3) %>>%  # filter
>   cpoModelMatrix(~ 0 + .^2)  # interactions
> head(getTaskData(task))
        PC1        PC2         PC3    PC1:PC2     PC1:PC3      PC2:PC3 Species
1 -2.684126 -0.3193972  0.02791483  0.8573023 -0.07492690 -0.008915919  setosa
2 -2.714142  0.1770012  0.21046427 -0.4804064 -0.57122986  0.037252434  setosa
3 -2.888991  0.1449494 -0.01790026 -0.4187575  0.05171367 -0.002594632  setosa
4 -2.745343  0.3182990 -0.03155937 -0.8738398  0.08664130 -0.010045316  setosa
5 -2.728717 -0.3267545 -0.09007924  0.8916204  0.24580071  0.029433798  setosa
6 -2.280860 -0.7413304 -0.16867766  1.6908707  0.38473006  0.125045884  setosa
```

"Composable Preprocessing Operators" are an extension for the [mlr](https://github.com/mlr-org/mlr) ("Machine Learning in R") project which represent preprocessing operations (e.g. imputation or PCA) in the form of R objects. These CPO objects can be composed to form more complex operations, they can be applied to data sets, and they can be attached to mlr `Learner` objects to generate complex machine learning pipelines that perform both preprocessing and model fitting.

## Table of Contents

* [Short Overview](#short-overview)
* [Installation](#installation)
* [Documentation](#documentation)
* [Project Status](#project-status)
* [Contributing](#contributing)
* [Similar Projects](#similar-projects)
* [License](#license)

## Short Overview

CPOs are created by calling a constructor.
```R
> cpoScale()
scale(center = TRUE, scale = TRUE)
```

The created objects have Hyperparameters that can be manipulated using `getHyperPars`, `setHyperPars` etc, just like in `mlr`.
```R
> getHyperPars(cpoScale())
$scale.center
[1] TRUE

$scale.scale
[1] TRUE

> setHyperPars(cpoScale(), scale.center = FALSE)
scale(center = FALSE, scale = TRUE)
```

The `%>>%`-operator can be used to create complex pipelines.
```R
> cpoScale() %>>% cpoPca()
(scale >> pca)(scale.center = TRUE, scale.scale = TRUE)
```

This operator can also be used to apply an operation to a data set:
```R
> head(iris %>>% cpoPca())
  Species       PC1      PC2          PC3          PC4
1  setosa -5.912747 2.302033  0.007401536  0.003087706
2  setosa -5.572482 1.971826  0.244592251  0.097552888
3  setosa -5.446977 2.095206  0.015029262  0.018013331
4  setosa -5.436459 1.870382  0.020504880 -0.078491501
5  setosa -5.875645 2.328290 -0.110338269 -0.060719326
6  setosa -6.477598 2.324650 -0.237202487 -0.021419633
```

Or to attach an operation to an MLR `Learner`, which extends the Learner's hyperparameters by the CPO's hyperparameters:

```R
> cpoScale() %>>% makeLearner("classif.logreg")
Learner classif.logreg.scale from package stats
Type: classif
Name: ; Short name: 
Class: CPOLearner
Properties: numerics,factors,prob,twoclass
Predict-Type: response
Hyperparameters: model=FALSE,scale.center=TRUE,scale.scale=TRUE
```

Get a list of all `CPO`s by calling `listCPO()`.

## Installation

Install `mlrCPO` from CRAN, or use the more recent GitHub version:

```R
devtools::install_github("mlr-org/mlrCPO")
```

## Documentation

To effectively use `mlrCPO`, you should first familiarize yourself a little with `mlr`. There is an extensive [tutorial](https://mlr-org.github.io/mlr/) online; for more ressources on `mlr`, see the overview on `mlr`'s [GitHub page](https://github.com/mlr-org/mlr).

To get familiar with `mlrCPO`, it is recommended that you read the **vignettes**. For each vignette, there is also a *compact version* that has all the R output removed.

1. [First Steps](https://rawgit.com/mlr-org/mlrCPO/master/inst/doc/a_1_getting_started.html): Introduction and short overview ([compact version](https://rawgit.com/mlr-org/mlrCPO/master/inst/doc/z_1_getting_started_terse.html)).
2. [mlrCPO Core](https://rawgit.com/mlr-org/mlrCPO/master/inst/doc/a_2_mlrCPO_core.html): Description of general tools for `CPO` handling ([compact version](https://rawgit.com/mlr-org/mlrCPO/master/inst/doc/z_2_mlrCPO_core_terse.html)).
3. [Builtin CPOs](https://rawgit.com/mlr-org/mlrCPO/master/inst/doc/a_3_all_CPOs.html): Listing and description of all builtin `CPO`s ([compact version](https://rawgit.com/mlr-org/mlrCPO/master/inst/doc/z_3_all_CPOs_terse.html)).
4. [Custom CPOs](https://rawgit.com/mlr-org/mlrCPO/master/inst/doc/a_4_custom_CPOs.html): How to create your own `CPO`s. ([compact version](https://rawgit.com/mlr-org/mlrCPO/master/inst/doc/z_4_custom_CPOs_terse.html)).
5. [CPO Internals](info/developers.md): A small intro guide for developers into the code base. See the `info` directory for pdf / html versions.

For more documentation of individual `mlrCPO` functions, use R's built-in `help()` functionality.

## Project Status

The foundation of `mlrCPO` is built and is reasonably stable, only small improvements and stability fixes are expected here. There are still many concrete implementations of preprocessing operators to be written.

## Contributing

### Bugs, Questions, Feedback

`mlrCPO` is a free and open source software project that encourages participation and feedback. If you have any issues, questions, suggestions or feedback, please do not hesitate to open an "issue" about it on the GitHub page!

In case of problems / bugs, it is often helpful if you provide a "minimum working example" that showcases the behaviour (but don't worry about this if the bug is obvious). 

Please understand that the ressources of the project are limited: response may sometimes be delayed by a few days, and some suggestions may not not make it to become features for a while.

### Contributing Code, Pull Requests

Pull Requests that fix small issues are very welcome, *especially* if they contain tests that check for the given issue. For larger contributions, or Pull Requests that add features, please note:

1. Adding new `CPO`s is always welcome. Please have a look at a few examples in the current codebase (the [PCA CPO](https://github.com/mlr-org/mlrCPO/blob/master/R/CPO_pca.R) and the [corresponding tests file](https://github.com/mlr-org/mlrCPO/blob/master/tests/testthat/test_cpo_pca.R) are good for this, and show that adding a CPO does not require a lot of code) to familiarise yourself with the conventions. A `CPO` that comes with documentation, in particular also documenting the `CPOTrained` state, and with tests, is most likely to get merged quickly.

2. Adding or changing features of the backend, or changing the functioning of the backend, is a more complicated story. If a Pull Request is incongruent with the "vision" behind `mlrCPO`, or if it appears to put a large burden on the `mlrCPO` developers in the long term relative to the problems it solves, it may have a slim chance of getting merged. Therefore, if you plan to make a contribution changing `CPO` core behaviour, it is best if you first open an "issue" about it for discussion.

When creating Pull Requests, please follow the [Style Guide](https://github.com/rdatsci/PackagesInfo/wiki/R-Style-Guide). Adherence to this is checked by the CI system (Travis). On Linux (and possibly Mac) you can check this locally on your computer using the **`quicklint`** tool in the `tools` directory. This is **recommended** to avoid frustrating failed builds caused by style violations.

Before merging a Pull Request, it is possible that an `mlrCPO` developer makes further changes to it, e.g. to harmonise it with conventions, or to incorporate other ideas. 

When you make a Pull Request, it is assumed that you permit us (and are able to permit us) to incorporate the given code into the `mlrCPO` codebase as given, or with modifications, and distribute the result under the BSD 2-Clause License.

## Similar Projects

There are other projects that provide functionality similar to `mlrCPO` for other machine learning frameworks. The [caret](https://github.com/topepo/caret) project provides some preprocessing functionality, though not as flexible as `mlrCPO`. [dplyr](https://github.com/tidyverse/dplyr) has similar syntax and some overlapping functionality, but is focused ultimately more on (manual) *data manipulation* instead of (machine learning pipeline integrated) *preprocessing*. Much more close to `mlrCPO`'s functionality is the [Recipes package](https://topepo.github.io/recipes/). [scikit learn](http://scikit-learn.org/stable/) also has [preprocessing functionality](http://scikit-learn.org/stable/modules/preprocessing.html) built in.

## License

The BSD 2-Clause License
