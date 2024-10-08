% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lookup.R
\name{pretreatments}
\alias{pretreatments}
\title{Access Pretreatments}
\usage{
pretreatments(object)
}
\arguments{
\item{object}{An object whose pretreatments slot to modify.}
}
\value{
The output depends on the S4 class of the input object. If it is a
multimedia model object, it will return a character vector of the outcome
variable names. Otherwise it returns the outcome data.
}
\description{
This is an getter method for pretreatments in an S4 object, usually of class
mediation_data.
}
\examples{
exper <- demo_joy() |>
    mediation_data("PHQ", "treatment", starts_with("ASV"))
pretreatments(exper)
}
