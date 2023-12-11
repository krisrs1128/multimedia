
setClass(
  "treatment_profile",
  representation(
    t_mediator = "ANY",
    t_outcome = "ANY"
  )
)

default_treatment <- function(model, estimates) {
  if (is(estimates, "list")) {
    estimates <- estimates[[1]]
  }

  model.frame(estimates) |>
    select(any_of(treatments(model)))
}

profile_defaults <- function(x, t_mediator = NULL, t_outcome = NULL) {
  # fill in treatments with defaults
  if (is.null(t_mediator)) {
    t_mediator <- default_treatment(x, x@mediation@estimates)
  }
  if (is.null(t_outcome)) {
    t_outcome <- default_treatment(x, x@outcome@estimates)
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
