#' @importFrom rlang .data
#' @importFrom dplyr filter pull
retrieve_names <- function(object, nm) {
  object@edges |>
    filter(.data$node_type == nm) |>
    pull(.data$name)
}

#' Methods for accessing mediators
#' @noRd
setGeneric("mediators", \(object) standardGeneric("mediators"))

#' Methods for accessing outcomes
#' @noRd
setGeneric("treatments", \(object) standardGeneric("treatments"))

#' Methods for accessing outcomes
#' @noRd
setGeneric("outcomes", \(object) standardGeneric("outcomes"))

#' Methods for accessing pretreatments
#' @noRd
setGeneric("pretreatments", \(object) standardGeneric("pretreatments"))

#' Names of Mediators in a Multimedia Object
#'
#' This is a helper that returns the names of the mediator variables in an
#' object of class multimedia. It parses the graph in the DAG specifying the
#' mediation analysis, and it returns all variables between treatment and
#' outcome.
#'
#' @param object An object of class multimedia.
#' @return m A vector of strings containing the names of all the mediators.
#' @examples
#' exper <- demo_joy() |>
#'   mediation_data("PHQ", "treatment", starts_with("ASV"))
#' multimedia(exper) |>
#'   mediators()
#' @export
setMethod("mediators", "multimedia",
  \(object) {
    retrieve_names(object, "mediator")
})

#' Access to @mediators in Mediation Data
#'
#' This is an accessor returns the @mediators slot in a mediation_data object.
#'
#' @param object An object of class mediation_data.
#' @return m A data.frame whose rows are samples and columns are values of mediators across those samples.
#' @examples
#' exper <- demo_joy() |>
#'   mediation_data("PHQ", "treatment", starts_with("ASV"))
#' mediators(exper)
#' @export
setMethod("mediators", "mediation_data",
  \(object) {
    object@mediators
})

#' Names of Outcomes in a Multimedia Object
#'
#' This is a helper that returns the names of the outcome variables in an
#' object of class multimedia. It parses the graph in the DAG specifying the
#' mediation analysis, and it returns all variables of node type outcome.
#'
#' @param object An object of class multimedia.
#' @return m A vector of strings containing the names of all the outcomes.
#' @examples
#' exper <- demo_joy() |>
#'   mediation_data("PHQ", "treatment", starts_with("ASV"))
#' multimedia(exper) |>
#'   outcomes()
#'
#' exper <- demo_spline(tau = c(2, 1)) |>
#'   mediation_data(starts_with("outcome"), "treatment", "mediator")
#' multimedia(exper) |>
#'   outcomes()
#' @export
setMethod("outcomes", "multimedia",
  \(object) {
    retrieve_names(object, "outcome")
})

#' @export
setMethod("outcomes", "mediation_data",
  \(object) {
    object@outcomes
})

#' @export
setMethod("treatments", "mediation_data",
  \(object) {
    object@treatments
})

#' @export
setMethod("pretreatments", "mediation_data",
  \(object) {
    object@pretreatments
})

setGeneric("edges", \(object) standardGeneric("edges"))
setGeneric("outcomes<-", \(object, value) standardGeneric("outcomes<-"))
setGeneric("treatments<-", \(object, value) standardGeneric("treatments<-"))
setGeneric("pretreatments<-", \(object, value) standardGeneric("pretreatments<-"))
setGeneric("mediators<-", \(object, value) standardGeneric("mediators<-"))

#' @export
setMethod("outcomes<-", "mediation_data",
  \(object, value) {
    object@outcomes <- value
    object
})

#' @export
setMethod("mediators<-", "mediation_data",
  \(object, value) {
    object@mediators <- value
    object
})

#' @export
setMethod("treatments<-", "mediation_data",
  \(object, value) {
    object@treatments <- value
    object
})

#' @export
setMethod("pretreatments<-", "mediation_data",
  \(object, value) {
    object@pretreatments <- value
    object
})

setMethod("edges", "multimedia",
  \(object) {
    object@edges
  })


#' Names of Treatments in a Multimedia Object
#'
#' This is a helper that returns the names of the treatment variables in an
#' object of class multimedia. It parses the graph in the DAG specifying the
#' mediation analysis, and it returns all variables of node type treatment.
#'
#' @param object An object of class multimedia.
#' @return m A vector of strings containing the names of all the treatments.
#' @examples
#' exper <- demo_joy() |>
#'   mediation_data("PHQ", "treatment", starts_with("ASV"))
#' multimedia(exper) |>
#'   treatments()
#' @export
setMethod("treatments", "multimedia",
  function(object) {
    retrieve_names(object, "treatment")
})

#' Number of Mediators in a Multimedia Object
#'
#' @param object An object of class multimedia.
#' @return An integer specifying the number of mediators.
#' @examples
#' exper <- demo_joy() |>
#'   mediation_data("PHQ", "treatment", starts_with("ASV"))
#' multimedia(exper) |>
#'   n_mediators()
#' @export
n_mediators <- function(object) {
  length(mediators(object))
}

#' Number of Outcomes in a Multimedia Object
#'
#' @param object An object of class multimedia.
#' @return An integer specifying the number of outcomes.
#' @examples
#' exper <- demo_joy() |>
#'   mediation_data("PHQ", "treatment", starts_with("ASV"))
#' multimedia(exper) |>
#'   n_outcomes()
#' @export
n_outcomes <- function(object) {
  length(outcomes(object))
}

#' @export
outcome_model <- function(object) {
  object@outcome
}