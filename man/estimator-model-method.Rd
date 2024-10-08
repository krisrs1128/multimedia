% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/estimators.R
\name{estimator,model-method}
\alias{estimator,model-method}
\title{Accessor for Estimators}
\usage{
\S4method{estimator}{model}(object)
}
\arguments{
\item{object}{An object of class \code{model}, whose estimator function we want
access to.}
}
\value{
A fitted version of the input model class.
}
\description{
Accessor for Estimators
}
\examples{
m <- rf_model()
fit <- estimator(m)(mpg ~ hp + wt, data = mtcars)
}
