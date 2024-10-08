% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/treatment_profile.R
\name{setup_profile}
\alias{setup_profile}
\title{Define a \code{treatment_profile} object}
\usage{
setup_profile(x, t_mediator = NULL, t_outcome = NULL)
}
\arguments{
\item{x}{An object of class \code{multimedia} specifying the complete mediation
analysis DAG. The treatment, mediator, and outcome names are necessary to
build a profile of counterfactual treatments across each of these
variables.}

\item{t_mediator}{A data.frame whose columns store treatment names and whose
values are the treatment assignments to each sample (row). Defaults to
NULL, in which case this type of data.frame is constructed from the
treatment assignments in the \code{mediation_data}'s \verb{@treatment} slot. Each
column must be either a numeric or factor variable.}

\item{t_outcome}{A data.frame analogous to \code{t_mediator}, but applying to the
outcome node.}
}
\value{
An object of class \code{treatment_profile} giving treatment assignments
for both mediation and outcome terms.
}
\description{
For general mediation analysis, we need to provide counterfactuals for both
the outcome and mediator components of each sample. That is, we need to
understand Y(t, M(t')) where t and t' may not be the same.
\code{treatment_profile} classes place some more structural requirements on
treatment profiles, so that later effect estimation can make simplifying
assumptions. This function creates a treatment profile from a collection of
possible mediator and outcome treatments.
}
\examples{
exper <- demo_spline(tau = c(2, 1)) |>
    mediation_data(starts_with("outcome"), "treatment", "mediator")
fit <- multimedia(exper) |>
    estimate(exper)

t1 <- data.frame(treatment = factor(rep(c(0, 1), each = 5)))
profile <- setup_profile(fit, t_mediator = t1, t_outcome = t1)
profile

t2 <- data.frame(treatment = factor(rep(0, 10)))
profile <- setup_profile(fit, t_mediator = t1, t_outcome = t2)
profile
}
\seealso{
check_profile
}
