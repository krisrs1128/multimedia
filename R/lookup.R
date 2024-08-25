#' @importFrom rlang .data
#' @importFrom dplyr filter pull
retrieve_names <- function(object, nm) {
  object@edges |>
    filter(.data$node_type == nm) |>
    pull(.data$name)
}

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
mediators <- function(object) {
  retrieve_names(object, "mediator")
}

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
outcomes <- function(object) {
  retrieve_names(object, "outcome")
}

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
treatments <- function(object) {
  retrieve_names(object, "treatment")
}

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
