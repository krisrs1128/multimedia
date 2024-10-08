% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lookup.R
\name{outcomes,mediation_data-method}
\alias{outcomes,mediation_data-method}
\title{Outcomes Data in a Mediation Data Object}
\usage{
\S4method{outcomes}{mediation_data}(object)
}
\arguments{
\item{object}{An object of class mediation_data.}
}
\value{
A data.frame whose rows are samples and columns different outcomes.
}
\description{
This is an accessor function to the @outcomes slot in a mediation data
object. It returns the entire outcomes dataset, in contrast to outcomes()
applied to a multimedia object, which only returns the names of the outcome
variables.
}
\examples{
exper <- demo_joy() |>
    mediation_data("PHQ", "treatment", starts_with("ASV"))
outcomes(exper)
}
