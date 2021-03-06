% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plotDensities.R
\name{plotDensities}
\alias{plotDensities}
\title{Plot the densities estimated by \code{\link{estimateY}}.}
\usage{
plotDensities(
  yEstimates,
  type = "Individual",
  plotType = "KernelDensity",
  nBins = 15,
  meanType = "1"
)
}
\arguments{
\item{yEstimates}{An object created by \code{\link{estimateY}}. Distributions of the dependent variable.}

\item{type}{A character. Show distribution by "Individual" (default), "Category" (only if supplied by yEstimates) or all individuals "Combined"?}

\item{plotType}{A character. Form of presentation, choices are "KernelDensity" (default), "Histogram" or "Boxplot"}

\item{nBins}{An integer Number of bins for histogram}

\item{meanType}{A character "1" for total variance for category type, "2" for mean variance}
}
\value{
A ggplot2 object with densities
}
\description{
Plot the densities estimated by \code{\link{estimateY}}.
}
