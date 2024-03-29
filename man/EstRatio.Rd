% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Comparison.R
\name{EstRatio}
\alias{EstRatio}
\title{Ratio of Estimates}
\usage{
EstRatio(est1, se1, est0, se0, sig = 0.05)
}
\arguments{
\item{est1}{Arm 1 estimate.}

\item{se1}{Arm 1 standard error.}

\item{est0}{Arm 0 estimate.}

\item{se0}{Arm 0 standard error.}

\item{sig}{Significance level.}
}
\value{
Data.frame containing estimated ratio, its standard error, lower and
  upper confidence bounds, and a p-value assessing the null hypothesis that
  the ratio is unity.
}
\description{
Calculate CIs and p-value for the ratio of estimates.
}
