## ----global_options, include=FALSE--------------------------------------------
knitr::opts_chunk$set(echo = T, cache = T, results = "hold")
library(Temporal)

## -----------------------------------------------------------------------------
# Generate exponential event time data.
data <- GenData(n = 1e3, dist = "exp", theta = c(2), p = 0.2)

# Estimate parameters.
fit <- FitParaSurv(data, dist = "exp")
show(fit)

## -----------------------------------------------------------------------------
# Generate gamma event time data.
data <- GenData(n = 1e3, dist = "gamma", theta = c(2, 2), p = 0.25)

# Estimate parameters.
fit <- FitParaSurv(data, dist = "gamma", tau = 0.5)
show(fit)

## -----------------------------------------------------------------------------
set.seed(102)

# Generate generalized gamma event time data.
data <- GenData(n = 1e4, dist = "gen-gamma", theta = c(2, 2, 2), p = 0.1)

# Estimate parameters.
fit <- FitParaSurv(data, dist = "gen-gamma", report = TRUE)
show(fit)

## ---- results='markup', eval=FALSE--------------------------------------------
#  # Initialization.
#  fit <- FitParaSurv(
#    data,
#    dist = "gen-gamma",
#    init = list(alpha = 2, beta = 2, lambda = 2)
#  )
#  show(fit)

## -----------------------------------------------------------------------------
# Generate log-normal event time data.
data <- GenData(n = 1e3, dist = "log-normal", theta = c(1, 2), p = 0.15)

# Estimate parameters.
fit <- FitParaSurv(data, dist = "log-normal", tau = c(5, 10, 25))
show(fit)

## -----------------------------------------------------------------------------
# Generate Weibull event time data.
data <- GenData(n = 1e3, dist = "weibull", theta = c(2, 2), p = 0.3)

# Estimate parameters.
fit <- FitParaSurv(data, dist = "weibull")
show(fit)

## -----------------------------------------------------------------------------
set.seed(101)

# Target group.
df1 <- GenData(n = 1e3, dist = "gamma", theta = c(2, 1), p = 0.25)
df1$arm <- 1

# Reference group.
df0 <- GenData(n = 1e3, dist = "gamma", theta = c(2, 2), p = 0.15)
df0$arm <- 0

# Overall data set.
data <- rbind(df1, df0)

# Compare fitted distributions.
comp <- CompParaSurv(data, dist1 = "gamma", dist0 = "gamma")
cat("\n")
show(comp)

## -----------------------------------------------------------------------------
# Target group.
df1 <- GenData(n = 1e3, dist = "weibull", theta = c(2, 2), p = 0.5)
df1$arm <- 1

# Reference group.
d0 <- GenData(n = 1e3, dist = "weibull", theta = c(2, 2), p = 0.0)
d0$arm <- 0

# Overall data set.
data <- rbind(df1, df0)

# Compare fitted distributions.
comp <- CompParaSurv(data, dist1 = "weibull", dist0 = "weibull")
cat("\n")
show(comp)

## -----------------------------------------------------------------------------
set.seed(105)

# Target group.
df1 <- GenData(n = 1e3, dist = "log-normal", theta = c(0, sqrt(2 * log(2))), p = 0.1)
df1$arm <- 1

# Reference group.
d0 <- GenData(n = 1e3, dist = "weibull", theta = c(2, sqrt(log(2))), p = 0.1)
d0$arm <- 0

# Overall data set.
data <- rbind(df1, df0)

# Compare fitted distributions.
comp <- CompParaSurv(data, dist1 = "log-normal", dist0 = "weibull")
cat("\n")
show(comp)

## -----------------------------------------------------------------------------
set.seed(106)

# Target group.
df1 <- GenData(n = 1e3, dist = "gamma", theta = c(4, 4), p = 0.2)
df1$arm <- 1

# Reference group.
df0 <- GenData(n = 1e3, dist = "exp", theta = c(1), p = 0.2)
df0$arm <- 0

# Overall data set.
data <- rbind(df1, df0)

# Compare fitted distributions.
comp <- CompParaSurv(data, dist1 = "gamma", dist0 = "exp", tau = c(0.5, 1.0, 1.5))
cat("\n")
show(comp)

