% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mediation.R
\name{mediation_data}
\alias{mediation_data}
\title{\code{mediation_data} Constructor}
\usage{
mediation_data(x, outcomes, treatments, mediators, pretreatments = NULL)
}
\arguments{
\item{x}{A SummarizedExperiment, phyloseq object, or data.frame whose columns
contain all the variables needed for the mediation analysis.}

\item{outcomes}{A vector or tidyselect specification of the names of all
outcome variables.}

\item{treatments}{A vector or tidyselect specification of the names of all
treatment variables.}

\item{mediators}{A vector or tidyselect specification of the names of all
mediators.}

\item{pretreatments}{A vector containing names of all pretreatment variables.
Defaults to NULL, in which case the pretreatments slot will remain empty.}
}
\value{
result An object of class \code{mediation_data}, with separate slots for
each of the node types in a mediation analysis diagram.
}
\description{
Convert a SummarizedExperiment, phyloseq object, or data.frame into a
\code{mediation_data} object. This conversion helps to organize the variables that
lie on different parts of the mediation analysis graph, so that they are not
all kept in a homogeneous experiment or data.frame. It's possible to specify
outcome, mediator, or treatment variables using either string vectors or
tidyselect syntax, e.g., \code{starts_with("mediator_")} will match all columns of
the input data starting with the string mediator.
}
\examples{
# multiple outcomes, one mediator
mediation_data(
    demo_spline(), starts_with("outcome"), "treatment", "mediator"
)

# one outcome, multiple mediators
mediation_data(demo_joy(), "PHQ", "treatment", starts_with("ASV"))
}
\seealso{
mediation_data-class
}
