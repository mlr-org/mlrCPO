% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/auxhelp.R
\name{mlrCPO-package}
\alias{mlrCPO-package}
\title{Composable Preprocessing Operators}
\description{
\bold{mlrCPO} is a toolset that enriches \code{mlr} with a diverse set of preprocessing operators.
Composable Preprocessing Operators (\dQuote{CPO}s) are first-class R objects that can be applied to
\code{data.frame}s and \code{mlr} \code{Task}s to modify data, they can be attached to \code{mlr}
\code{Learner}s to add preprocessing to machine learning algorithms, and they can be composed to form
preprocessing pipelines.

\code{mlrCPO} focuses on preprocessing as part of automated machine learning pipelines. This means that
it is designed with the expectation that the same preprocessing options are applied to incoming training data,
and test data. A common mistake in machine learning is that a machine learning method is evaluated (e.g. using
resampling) on a dataset \emph{after} that dataset has been cleaned up and preprocessed in one go. The proper evaluation
would need to consider that the preprocessing of training data may not be influenced by any information contained
in the test data set. \code{mlrCPO} takes this duality into account by providing \code{\link{CPO}} objects that
run on training data, and which then create \code{\link{CPOTrained}} objects that can be used on test data (or entirely new
prediction data).

This focus on preprocessing is the reason for a strict separation between \dQuote{Feature Operation} \code{CPO}s,
\dQuote{Target Operation} \code{CPO}s, and \dQuote{Retrafoless} \code{CPO}s (see \link{OperatingType}).
The first class only changes
(predictor) features of a dataset, and does so in a way reproducible on test data. The second class only changes
(outcome) target data of a dataset, and is then able to \code{\link{invert}} the prediction, made by a learner
on new data, back to the space of the original target data. The \dQuote{Retrafoless} \code{CPO} only operates
during training and may only add or subtract data rows (e.g. for SMOTE-ing or subsampling), without transforming
the space of either predictor or outcome variables.

\code{CPO}'s design is supposed to help its user avoid bugs and errors. Therefore it often avoids doing things
implicitly and relies on explicit commands e.g. for removing data or converting between datatypes. It has certain
restrictions in place (e.g. \link{CPOProperties}, \link{CPOTrainedCapability}) that try to make it hard to do the
wrong thing while not being in the way of the right thing.

Other packages with similar, partially overlapping functionality are \href{https://cran.r-project.org/package=recipes}{recipes},
\href{https://cran.r-project.org/package=dplyr}{dplyr}, and \href{https://cran.r-project.org/package=caret}{caret}.
}
