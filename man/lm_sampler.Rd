% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/estimators.R
\name{lm_sampler}
\alias{lm_sampler}
\title{Sample a Linear Model}
\usage{
lm_sampler(fits, newdata = NULL, indices = NULL, ...)
}
\arguments{
\item{fits}{The fitted linear model model from which to draw samples.}

\item{newdata}{A data.frame containing new inputs from which to sample
responses. If NULL, defaults to the data used to estimate fit.}

\item{indices}{The coordinates of the response from which to draw samples.}

\item{...}{Additional parameters passed to lm.predict}
}
\value{
y_star A data.frame of samples y associated with the new inputs.
}
\description{
Draw samples from a fitted linear model.
}
\examples{
fit <- lm(mpg ~ hp + wt, data = mtcars)
lm_sampler(list(f = fit))

plm <- parallelize(lm)
fit <- plm(mpg + disp ~ hp + wt, data = mtcars)
lm_sampler(fit)
}
