% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lookup.R
\name{pretreatments,mediation_data-method}
\alias{pretreatments,mediation_data-method}
\title{Pretreatments in a Mediation Data Object}
\usage{
\S4method{pretreatments}{mediation_data}(object)
}
\arguments{
\item{object}{An object of class mediation_data.}
}
\value{
A data.frame whose rows are samples and columns different
pretreatments.
}
\description{
This is an accessor function to the @pretreatments slot in a mediation data
object. It returns the entire set of observed pretreatments, in contrast to
pretreatments() applied to a multimedia object, which only returns the names
of the pretreatment variables.
}
\examples{
exper <- demo_joy() |>
    mediation_data("PHQ", "treatment", starts_with("ASV"))
pretreatments(exper)
}
