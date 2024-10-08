% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/estimators.R
\name{brms_model}
\alias{brms_model}
\title{Bayesian Regression Model across Responses}
\usage{
brms_model(...)
}
\arguments{
\item{...}{Keyword parameters passed to brm.}
}
\value{
model An object of class \code{model} with estimator, predictor, and
sampler functions associated wtih a Bayesian regression model.
}
\description{
Apply a Bayesian regression model in parallel across each response $y$ in an
outcome or mediation model. This can be helpful when we want to share
information across related
}
\examples{
exper <- demo_joy() |>
    mediation_data("PHQ", "treatment", starts_with("ASV"))
multimedia(exper, brms_model()) # call estimate() on this to fit
}
\seealso{
glmnet_model lnm_model rf_model lm_model
}
