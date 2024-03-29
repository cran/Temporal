% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/DataGen.R
\name{rLogNormal}
\alias{rLogNormal}
\title{Simulation from the Log-Normal Distribution}
\usage{
rLogNormal(n, m = 0, s = 1, p = 0)
}
\arguments{
\item{n}{Sample size.}

\item{m}{Location.}

\item{s}{Scale.}

\item{p}{Expected censoring proportion.}
}
\value{
Data.frame including the observation time and status.
}
\description{
Generates log-normal event times with location parameter \eqn{\mu} and scale
parameter \eqn{\sigma}. See \code{\link{FitLogNormal}} for the
parameterization. If a censoring proportion \eqn{p} is provided, the event
times are subject to non-informative random right censoring.
}
