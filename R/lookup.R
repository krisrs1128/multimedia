retrieve_names <- function(object, nm) {
  object@edges |>
    filter(node_type == nm) |>
    pull(name)
}

#' @export
mediators <- function(object) {
  retrieve_names(object, "mediator")
}

#' @export
outcomes <- function(object) {
  retrieve_names(object, "outcome")
}

#' @export
treatments <- function(object) {
  retrieve_names(object, "treatment")
}

#' @export
n_mediators <- function(object) {
  length(mediators(object))
}

#' @export
n_outcomes <- function(object) {
  length(outcomes(object))
}
