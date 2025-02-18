% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/farseerModels.R
\name{create.farseer.models}
\alias{create.farseer.models}
\title{create (farseer.models)}
\usage{
create.farseer.models(
  farseerDataFrame,
  target,
  neural_max_threshold = 0.1,
  trainingVector = NULL,
  ...
)
}
\arguments{
\item{farseerDataFrame}{a data for training (must be a farseer.data.frame)}

\item{target}{an index of target from farseer.data.frame$target.variables}

\item{neural_max_threshold}{double, maximum threshold to be used in neural networks training.}

\item{trainingVector}{= NULL optionally a training vector can be provided}

\item{...}{trainingFraction from \link{farseer.training.vector} how many cases should be a training set?}
}
\value{
a farseer.models object
}
\description{
creates a single set of farseer models
}
