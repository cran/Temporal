% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Weibull.R
\name{fit.Weibull}
\alias{fit.Weibull}
\title{Weibull Distribution Parameter Estimation}
\usage{
fit.Weibull(time, status, sig = 0.05, tau = NULL, init = NULL)
}
\arguments{
\item{time}{Numierc observation times.}

\item{status}{Status indicator, coded as 1 if an event was observed, 0 if censored.}

\item{sig}{Significance level, for CIs.}

\item{tau}{Optional truncation times for calculating RMSTs}

\item{init}{Numeric vector containing the initial value for \eqn{\alpha}.}
}
\value{
An object of class \code{fit} containing the following:
\describe{
 \item{Parameters}{The estimated shape \eqn{\alpha} and rate \eqn{\lambda}.}
 \item{Information}{The observed information matrix.}
 \item{Outcome}{The fitted mean, median, and variance.}
 \item{RMST}{The estimated RMSTs, if tau was specified.}
}
}
\description{
Estimates parameters for Weibull event times subject to non-informative
right censoring. The Weibull distribution is parameterized in terms
of the shape \eqn{\alpha} and rate \eqn{\lambda}:
\deqn{f(t) = \alpha\lambda^{\alpha}t^{\alpha-1}e^{-(\lambda t)^{\alpha}}, t>0}
}
\examples{
# Generate Weibull data with 20\% censoring
data = genData(n=1e3,dist="weibull",theta=c(2,2),p=0.2);
# Estimate
fit = fitParaSurv(time=data$time,status=data$status,dist="weibull");
}
\seealso{
\itemize{
  \item{Fitting function for parametric survival distributions \code{\link{fitParaSurv}}}
}
}
