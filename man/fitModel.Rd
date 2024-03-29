% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/estimateRelationShip.R
\name{fitModel}
\alias{fitModel}
\title{Fit Model}
\usage{
fitModel(
  X,
  y,
  yUnc,
  xUnc,
  parNames,
  varNames,
  form,
  startPar = rep(0, length(parNames)),
  iter = 1000,
  chains = 8,
  burnin = 0.4 * iter,
  thinning = 5,
  parNamesDir = NULL,
  shinyUse = TRUE
)
}
\arguments{
\item{X}{Measurements of the independent variable.}

\item{y}{Measurements of the dependent variable.}

\item{yUnc}{A numeric vector. y uncertainties}

\item{xUnc}{A numeric vector. x uncertainties}

\item{parNames}{names of parameter}

\item{varNames}{A character. Name of x variable}

\item{form}{form}

\item{startPar}{numeric vector of length of parNames}

\item{iter}{An integer. Number of iterations for MCMC model}

\item{chains}{An integer. Number of chains for MCMC model}

\item{burnin}{An integer. Number of burnin iterations for MCMC model}

\item{thinning}{thinning}

\item{parNamesDir}{parNamesDir}

\item{shinyUse}{set to TRUE for shiny use and FALSE for non-shiny use}
}
\description{
Fit Model
}
