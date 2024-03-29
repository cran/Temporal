% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/GenGamma.R
\name{FitGenGamma}
\alias{FitGenGamma}
\title{Generalized Gamma Distribution Parameter Estimation}
\usage{
FitGenGamma(
  data,
  beta_lower = 0.1,
  beta_upper = 10,
  eps = 1e-06,
  init = list(),
  maxit = 10,
  report = FALSE,
  sig = 0.05,
  status_name = "status",
  tau = NULL,
  time_name = "time"
)
}
\arguments{
\item{data}{Data.frame.}

\item{beta_lower}{If dist="gen-gamma", lower limit on possible values for beta.}

\item{beta_upper}{If dist="gen-gamma", upper limit on possible values for beta.}

\item{eps}{Tolerance for Newton-Raphson iterations.}

\item{init}{List with initial values for the shape `alpha`, `beta` and rate
`lambda` parameters.}

\item{maxit}{Maximum number of NR iterations.}

\item{report}{Report fitting progress?}

\item{sig}{Significance level, for CIs.}

\item{status_name}{Name of the status indicator, 1 if observed, 0 if
censored.}

\item{tau}{Optional truncation times for calculating RMSTs.}

\item{time_name}{Name of column containing the time to event.}
}
\value{
An object of class \code{fit} containing the following:
\describe{
 \item{Parameters}{The estimated shape \eqn{(\alpha,\beta)} and rate \eqn{\lambda} parameters.}
 \item{Information}{The observed information matrix.}
 \item{Outcome}{The fitted mean, median, and variance.}
 \item{RMST}{The estimated RMSTs, if tau was specified.}
}
}
\description{
Estimates parameters for generalized gamma event times subject to non-informative
right censoring. The gamma distribution is parameterized in terms
of the shape parameters \eqn{(\alpha,\beta)}, and the rate \eqn{\lambda}:
\deqn{f(t) = \frac{\beta\lambda}{\Gamma(\alpha)} (\lambda t)^{\alpha\beta-1}e^{-(\lambda t)^{\beta}}, t>0}
}
\examples{
set.seed(103)
# Generate generalized gamma data with 20\% censoring.
data <- GenData(n = 1e4, dist = "gen-gamma", theta = c(2, 2, 2), p = 0.2)

# Estimate parameters.
fit <- FitParaSurv(data, dist = "gen-gamma", report = TRUE)
}
