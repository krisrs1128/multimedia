#' @importFrom dplyr n_distinct
check_profile <- function(object) {
  if (is(object@t_mediator, "list")) {
    m_samples <- map(object@t_mediator, nrow)
    y_samples <- map(object@t_outcome, nrow)
    if (n_distinct(y_samples) != 1) {
      cli_abort("Number of rows in treatment profile must agree across all outcomes.")
    }

    if (!all(m_samples %in% y_samples)) {
      cli_abort("Number of samples in treatment profile must agree across mediators and outcomes.")
    }
  }
}

#' @export
setup_profile <- function(x, t_mediator = NULL, t_outcome = NULL) {
  if (is.null(t_mediator)) {
    t_mediator <- x@treatments
  }
  if (is.null(t_outcome)) {
    t_outcome <- x@treatments
  }

  # apply treatments to all mediators/outcomes
  if (!is(t_mediator, "list")) {
    t_mediator <- t_mediator |>
      replicate(n_mediators(x), expr = _, simplify = FALSE) |>
      set_names(mediators(x))
  }
  if (!is(t_outcome, "list")) {
    t_outcome <- t_outcome |>
      replicate(n_outcomes(x), expr = _, simplify = FALSE) |>
      set_names(outcomes(x))
  }

  new("treatment_profile", t_mediator = t_mediator, t_outcome = t_outcome)
}

#' @export
setClass(
  "treatment_profile",
  representation(
    t_mediator = "ANY",
    t_outcome = "ANY"
  ),
  validity = check_profile
)
