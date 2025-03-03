% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/farseerUtilities.R
\name{farseer.rocplot}
\alias{farseer.rocplot}
\title{farseer.rocplot (S3 function)}
\usage{
farseer.rocplot(performanceList, targetName = NULL, title = NULL)
}
\arguments{
\item{performanceList}{a \link[ROCR]{performance-class} object containing "frp" and "tpr" performance of the models}

\item{targetName}{name of the dependent variable}

\item{title}{optional plot title}
}
\value{
a roc plot
}
\description{
creates a roc-plot for models
}
