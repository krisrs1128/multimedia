setClass(
  "model",
  representation(
    estimator = "ANY",
    estimates = "ANY",
    sampler = "ANY",
    model_type = "ANY"
  )
)

sub_formula <- function(formula, yj) {
  update.formula(formula, as.formula(glue("{yj} ~ .")))
}

#' @importFrom formula.tools lhs.vars
#' @examples
#' mat <- data.frame(matrix(rnorm(100), 25, 4))
#' colnames(mat) <- c("y1", "y2", "x1", "x2")
#' plm <- parallelize(lm)
#' plm(y1 + y2 ~ x1 + x2, mat)
parallelize <- function(f) {
  function(formula, ...) {
    models <- list()

    ys <- lhs.vars(formula)
    for (j in seq_along(ys)) {
      fmla <- sub_formula(formula, ys[j])
      models[[ys[j]]] <- f(fmla, ...)
    }
    models
  }
}

#' Linear Model across Responses
lm_model <- function() {
  new(
    "model",
    estimator = parallelize(lm),
    estimates = NULL,
    sampler = lm_sampler,
    model_type = "lm_model()"
  )
}

#' @importFrom dplyr bind_cols
lm_sampler <- function(fits, new_data = NULL, indices = NULL) {
  if (is.null(indices)) {
    indices <- seq_along(fits)
  }

  nm <- names(fits)
  y_hats <- list()
  for (i in indices) {
    sigma <- summary(fits[[i]])$sigma
    y_ <- predict(fits[[i]], newdata = new_data)
    y_hats[[nm[i]]] <- y_ + rnorm(length(y_), 0, sigma)
  }

  bind_cols(y_hats)
}

default_treatment <- function(edges, estimates) {
  treatment_names <- edges |>
    filter(node_type == "treatment") |>
    pull(name)

  model.frame(estimates[[1]]) |>
    select(any_of(treatment_names))
}

path_defaults <- function(x, t_mediator = NULL, t_outcome = NULL) {
  # fill in treatments with defaults
  if (is.null(t_mediator)) {
    t_mediator <- default_treatment(x@edges, x@mediation@estimates)
  }
  if (is.null(t_outcome)) {
    t_outcome <- default_treatment(x@edges, x@outcome@estimates)
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

  list(
    t_mediator = t_mediator,
    t_outcome = t_outcome
  )
}

mediators <- function(object) {
  object@edges |>
    filter(node_type == "mediator") |>
    pull(name)
}

outcomes <- function(object) {
  object@edges |>
    filter(node_type == "outcome") |>
    pull(name)
}

n_mediators <- function(object) {
  length(mediators(object))
}

n_outcomes <- function(object) {
  length(outcomes(object))
}

setMethod("sample", "multimedia", function(
    x, size, pretreatment = NULL,
    t_mediator = NULL,
    t_outcome = NULL, mediators = NULL, ...) {
  if (missing(size)) {
    size <- 1
  }

  # if mediators are not provided, sample them
  d <- path_defaults(x, t_mediator, t_outcome)
  if (is.null(mediators)) {
    mediators <- list()
    for (i in seq_along(d$t_mediator)) {
      mediators[[i]] <- bind_cols(pretreatment, d$t_mediator[[i]]) |>
        x@mediation@sampler(x@mediation@estimates, new_data = _, i)
    }
    mediators <- bind_cols(mediators)
  }

  # sample outcome given everything else
  outcomes <- list()
  for (i in seq_along(d$t_outcome)) {
    outcomes[[i]] <- bind_cols(pretreatment, d$t_outcome[[i]], mediators) |>
      x@outcome@sampler(x@outcome@estimates, new_data = _, i)
  }

  list(mediators = mediators, outcomes = outcomes)
})

setMethod("predict", "multimedia", function(
    object, pretreatment = NULL,
    t_mediator = NULL, t_outcome = NULL, mediators = NULL, ...) {
  d <- path_defaults(object, t_mediator, t_outcome)
  if (is.null(mediators)) {
    nm <- names(d$t_mediator)
    mediators <- list()
    for (i in seq_along(d$t_mediator)) {
      mediator_covariates <- bind_cols(pretreatment, d$t_mediator[[i]])
      mediators[[nm[i]]] <- predict(
        object@mediation@estimates[[i]],
        mediator_covariates
      )
    }
    mediators <- bind_cols(mediators)
  }

  # sample outcome given everything else
  nm <- names(d$t_outcome)
  outcomes <- list()
  for (i in seq_along(d$t_outcome)) {
    outcomes[[nm[i]]] <- bind_cols(pretreatment, d$t_outcome[[i]], mediators) |>
      predict(object@outcome@estimates[[i]], newdata = _)
  }
  list(mediators = mediators, outcomes = bind_cols(outcomes))
})

mediation_formula <- function(edges) {
  edges <- edges %E>%
    filter(state == "active")

  mediators <- edges %N>%
    filter(node_type == "mediator") |>
    pull(name)
  predictors <- edges %N>%
    filter(node_type %in% c("pretreatment", "treatment")) |>
    pull(name)

  glue("{paste0(mediators, collapse = '+')} ~ {paste0(predictors, collapse = '+')}") |>
    as.formula()
}

outcome_formula <- function(edges) {
  edges <- edges %E>%
    filter(state == "active")

  response <- edges %N>%
    filter(node_type == "outcome") |>
    pull(name)
  predictors <- edges %N>%
    filter(!(node_type == "outcome")) |>
    pull(name)

  glue("{paste0(response, collapse = '+')} ~ {paste0(predictors, collapse = '+')}") |>
    as.formula()
}

#' @export
estimate <- function(model, exper) {
  # estimate mediation model
  mediation_est <- model@mediation@estimator
  model@mediation@estimates <- mediation_est(
    mediation_formula(model@edges),
    exper_df(exper)
  )

  # estimate outcome model
  outcome_est <- model@outcome@estimator
  model@outcome@estimates <- outcome_est(
    outcome_formula(model@edges),
    exper_df(exper)
  )

  model
}
