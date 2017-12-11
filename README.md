[![Build Status](https://travis-ci.org/mlr-org/mlrCPO.svg?branch=master)](https://travis-ci.org/mlr-org/mlrCPO)
[![Coverage](https://codecov.io/github/mlr-org/mlrCPO/branch/master/graphs/badge.svg)](https://codecov.io/github/mlr-org/mlrCPO)
[![CRAN Status Badge](http://www.r-pkg.org/badges/version/mlrCPO)](https://CRAN.R-project.org/package=mlrCPO)
[![CRAN Downloads](http://cranlogs.r-pkg.org/badges/mlrCPO)](https://cran.rstudio.com/web/packages/mlrCPO/index.html)


# mlrCPO: Composable Preprocessing Operators for [mlr](https://github.com/mlr-org/mlr)

GSoC 2017 Project: Operator Based Machine Learning Pipeline Construction

## What is CPO?

"Composable Preprocessing Operators" are an extension for the [mlr](https://github.com/mlr-org/mlr) ("Machine Learning in R") project which represent preprocessing operations (e.g. imputation or PCA) in the form of R objects. These CPO objects can be composed to form more complex operations, they can be applied to data sets, and they can be attached to mlr `Learner` objects to generate complex machine learning pipelines that perform both preprocessing and model fitting.

## Table of Contents

* [Installation](#installation)
* [Short Overview](#short-overview)
* [Documentation](#documentation)
* [Project Status](#project-status)
* [Similar Projects](#similar-projects)
* [License](#license)

## Installation

`mlrCPO` relies on small extensions of both [`ParamHelpers`](https://github.com/berndbischl/ParamHelpers/pull/190#issuecomment-324618168) and [`mlr`](https://github.com/mlr-org/mlr/pull/1827) which will be merged soon. Until that is the case, you need to install these extensions using [`devtools`](https://cran.r-project.org/web/packages/devtools/README.html):

```R
devtools::install_github("mb706/ParamHelpers", ref = "paramSetSugar")
devtools::install_github("mlr-org/mlr", ref = "ComposablePreprocOperators")
```

Now install `mlrCPO`:

```R
devtools::install_github("mlr-org/mlrCPO")
```

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

## Documentation

To effectively use `mlrCPO`, you should first familiarize yourself a little with `mlr`. There is an extensive [tutorial](https://mlr-org.github.io/mlr-tutorial/devel/html/) online; for more ressources on `mlr`, see the overview on `mlr`'s [GitHub page](https://github.com/mlr-org/mlr).

A thorough reference of `mlrCPO`'s capabilities can be found in the [vignette](https://rawgit.com/mlr-org/mlrCPO/master/inst/doc/mlrCPO.html) ([compact version](https://rawgit.com/mlr-org/mlrCPO/master/inst/doc/mlrCPO_terse.html)). For more documentation of individual `mlrCPO` functions, use R's built-in `help()` functionality.

A small intro guide for developers into the code base can be found in [info/developers.md](info/developers.md).

## Project Status

`mlrCPO` is not yet a mature project and may be unstable at times; setting up a CI system is in progress, which should make things much smoother soon. Furthermore, even though it provides extensive functionality already, it is still lacking many concrete implementations of preprocessing operators.

## Similar Projects

There are other projects that provide functionality similar to `mlrCPO` for other machine learning frameworks. The [caret](https://github.com/topepo/caret) project provides some preprocessing functionality, though not as flexible as `mlrCPO`. Much more close to `mlrCPO`'s functionality is the [Recipes package](https://topepo.github.io/recipes/). [scikit learn](http://scikit-learn.org/stable/) also has [preprocessing functionality](http://scikit-learn.org/stable/modules/preprocessing.html) built in.

## License

The BSD 2-Clause License
