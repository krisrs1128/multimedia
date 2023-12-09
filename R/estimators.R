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
lm_sampler <- function(fits, new_data = NULL) {
  y_hats <- list()
  for (i in seq_along(fits)) {
    y_hats[[i]] <- predict(fits[[i]], newdata = new_data)
  }
  names(y_hats) <- names(fits)
  bind_cols(y_hats)
}

setMethod("sample", "model", function(x, size, new_data = NULL, ...) {
  if (missing(size)) {
    size <- 1
  }
  if (is.null(x@estimates)) {
    cli_abort("Cannot sample an unfitted model.")
  }

  replicate(size, x@sampler(x@estimates, new_data), simplify = FALSE) |>
    bind_rows(.id = "replicate")
})

default_treatment <- function(edges, estimates) {
  treatment_names <- edges |> 
    filter(node_type == "treatment") |> 
    pull(name)

  model.frame(estimates[[1]]) |>
    select(any_of(treatment_names))
}

setMethod("sample", "multimedia", function(x, size, pretreatment = NULL, treatment_mediator = NULL, treatment_outcome = NULL, mediators = NULL, ...) {
  if (missing(size)) {
    size <- 1
  }
  f_out <- x@outcome
  f_med <- x@mediation

  # fill in treatments with defaults 
  if (is.null(treatment_mediator)) {
    treatment_mediator <- default_treatment(x@edges, f_med@estimates)
  }
  if (is.null(treatment_outcome)) {
    treatment_outcome <- default_treatment(x@edges, f_out@estimates)
  }

  # if mediators are not provided, sample them
  if (is.null(mediators)) {
    mediator_covariates <- bind_cols(pretreatment, treatment_mediator)
    mediators <- f_med@sampler(f_med@estimates, mediator_covariates)
  }

  # sample outcome given everything else
  outcome_covariates <- bind_cols(pretreatment, treatment_outcome, mediators)
  outcomes <- f_out@sampler(f_out@estimates, outcome_covariates)
  list(mediators = mediators, outcomes = outcomes)
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
