% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mediation.R
\name{bind_mediation}
\alias{bind_mediation}
\title{Convert \code{mediation_data} to a single data.frame}
\usage{
bind_mediation(exper)
}
\arguments{
\item{exper}{An object of class \code{mediation_data} containing the variables
that we want horizontally concatenated.}
}
\value{
A data.frame containing all variables from the original
\code{mediation_data} object.
}
\description{
It can be helpful to combine all the data in a \code{mediation_data} S4 object
into a single data.frame. This concatenates all data sources horizontally,
into an N samples x (all outcomes, mediators, treatments, ...) matrix.
}
\examples{
exper <- demo_joy() |>
    mediation_data("PHQ", "treatment", starts_with("ASV"))
exper
bind_mediation(exper)

exper <- demo_spline(tau = c(2, 1)) |>
    mediation_data(starts_with("outcome"), "treatment", "mediator")
exper
bind_mediation(exper)
}
