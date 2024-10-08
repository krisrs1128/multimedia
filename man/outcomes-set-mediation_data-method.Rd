% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lookup.R
\name{outcomes<-,mediation_data-method}
\alias{outcomes<-,mediation_data-method}
\title{Set the Outcomes in a Mediation Data Object}
\usage{
\S4method{outcomes}{mediation_data}(object) <- value
}
\arguments{
\item{object}{An object of class mediation_data.}

\item{value}{The new outcome values for the object.}
}
\value{
A version of \code{object} whose outcomes slot has been replaced.
}
\description{
This is an setter method for the outcomes slot in a mediation data object. It
lets you supply a new outcomes data.frame for the object.
}
\examples{
exper <- demo_joy() |>
    mediation_data("PHQ", "treatment", starts_with("ASV"))
outcomes(exper) <- data.frame(y = 1:10)
exper
}
